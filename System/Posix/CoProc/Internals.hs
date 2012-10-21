{-# LANGUAGE OverloadedStrings
           , ParallelListComp
           , TupleSections #-}
-- | Backend interface and CoProc interpreter implementations.
module System.Posix.CoProc.Internals where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.Bits
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import           Data.Maybe
import           Data.Monoid
import qualified GHC.IO.Handle.FD
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process
import           System.Process.Internals
import           System.Posix.ByteString

import           System.IO.Temp
import qualified Text.ShellEscape as Esc


class Interpreter interpreter where
  boot :: interpreter -> IO (Handle, Handle, Handle, ProcessHandle)
  send :: interpreter -> (Handle, Handle, Handle, ProcessHandle)
       -> ByteString -> IO (ByteString, ByteString)
  end  :: interpreter -> (Handle, Handle, Handle, ProcessHandle) -> IO ExitCode

data Bash = Bash
instance Show Bash where show _ = "bash"

instance Interpreter Bash where
  boot _ = runInteractiveProcess "bash" [] Nothing (Just [])
  end  _ = close
  send _ (i, _, _, _) cmd = withFIFOs query'
   where query' ofo efo = do
           Bytes.hPut i (Bytes.unlines wrapped)
           hFlush i
           [ob, eb] <- backgroundReadFIFOs [ofo, efo]
           return (ob, eb)
          where wrapped = [ "{", cmd, "} 1>" <> (Esc.bytes . Esc.bash) ofo <>
                                       " 2>" <> (Esc.bytes . Esc.bash) efo    ]


-- | Simplest way to shutdown a running interpreter.
close :: (Handle, Handle, Handle, ProcessHandle) -> IO ExitCode
close (i, _, _, p) = hClose i *> waitForProcess p

-- | Run an IO action with two FIFOs in scope, which will removed after it
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

backgroundReadFIFOs theFIFOs = do
  cells <- sequence (newEmptyMVar <$ theFIFOs)
  sequence_ [ forkIO (drainFIFO p >>= putMVar c) | p <- theFIFOs | c <- cells ]
  sequence (takeMVar <$> cells)

pid :: ProcessHandle -> IO CPid
pid handle = withProcessHandle handle getPID
 where getPID h@(OpenHandle pid) = return (h, pid)
       getPID _ = error
        "System.CoProc.Internal.pid: Called on closed handle! Yikes!"

