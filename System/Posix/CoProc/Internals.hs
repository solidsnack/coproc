{-# LANGUAGE OverloadedStrings
           , ParallelListComp
           , TupleSections #-}
-- | Backend interface and CoProc interpreter implementations.
module System.Posix.CoProc.Internals where

import           Control.Applicative
import           Control.Concurrent
import           Data.Bits
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import           Data.Monoid
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process
import           System.Process.Internals
import           System.Posix.ByteString

import           System.IO.Temp

import qualified Text.ShellEscape as Esc


-- | The backend interface, which provides the core of the frontend 'start',
--   'query' and 'done' operations.
class Interpreter interpreter where
  start :: interpreter -> IO (Handle, Handle, Handle, ProcessHandle)
  query :: interpreter -> (Handle, Handle, Handle, ProcessHandle)
                       -> ByteString -> IO (ByteString, ByteString)
  done  :: interpreter -> (Handle, Handle, Handle, ProcessHandle) -> IO ExitCode

-- | Simplest way to shutdown a running interpreter.
close :: (Handle, Handle, Handle, ProcessHandle) -> IO ExitCode
close (i, o, e, p) = hClose' i *> hClose' o *> hClose' e *> waitForProcess p
 where hClose' h = catchIOError (hClose h) (const (return ()))

-- | Run an IO action with two FIFOs in scope, which will be removed after it
--   completes.
withFIFOs :: (RawFilePath -> RawFilePath -> IO a) -> IO a
withFIFOs m = withSystemTempDirectory "hs.coproc." m'
 where m'   = (uncurry m =<<) . mk . Bytes.pack
       mk d = (o, e) <$ (createNamedPipe o mode >> createNamedPipe e mode)
        where (o, e) = (d <> "/o", d <> "/e")
              mode   = ownerReadMode .|. ownerWriteMode .|. namedPipeMode

-- | Use @bash@+@cat@ to drain the FIFO and terminate when complete.
drainFIFO :: ByteString -> IO ByteString
drainFIFO path = do
  (i, o, e, p) <- runInteractiveProcess "bash" [] Nothing (Just [])
  Bytes.hPutStrLn i ("exec cat <" <> (Esc.bytes . Esc.bash) path)
  hFlush i
  hClose i
  hClose e
  Bytes.hGetContents o <* waitForProcess p

-- | Read several FIFOs in the background, in parallel, returning all of their
--   contents.
backgroundReadFIFOs :: [ByteString] -> IO [ByteString]
backgroundReadFIFOs theFIFOs = do
  cells <- sequence (newEmptyMVar <$ theFIFOs)
  sequence_ [ forkIO (drainFIFO p >>= putMVar c) | p <- theFIFOs | c <- cells ]
  sequence (takeMVar <$> cells)

-- | Retrieve the PID from a process handle. Note that calling this function
--   on a process that one has terimated or called wait for will results in an
--   exception being thrown.
pid :: ProcessHandle -> IO CPid
pid handle = withProcessHandle handle getPID
 where getPID h@(OpenHandle pid) = return (h, pid)
       getPID _ = error
        "System.CoProc.Internal.pid: Called on closed handle! Yikes!"

