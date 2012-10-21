{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , TupleSections #-}
module System.Posix.CoProc where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import           Data.Monoid
import           System.Exit
import           System.IO
import           System.Process
import           System.Posix.ByteString

import qualified Text.ShellEscape as Esc

import qualified System.Posix.CoProc.Internals as Internals


start :: (Internals.Interpreter int) => int -> IO (Session int)
start interpreter = do
  handles@(_,_,_,p) <- Internals.start interpreter
  pid               <- Internals.pid p
  Session pid interpreter handles <$> newMVar ()

query :: (Internals.Interpreter int) => Session int
      -> ByteString
      -> IO (ByteString, ByteString)
query Session{..} q = withMVar lock
                               (const (Internals.query interpreter handles q))

done :: (Internals.Interpreter int) => Session int -> IO (CPid, ExitCode)
done Session{..} = (pid,) <$> Internals.done interpreter handles

data Session int = Session { pid :: CPid
                           , interpreter :: int
                           , handles :: (Handle, Handle, Handle, ProcessHandle)
                           , lock :: MVar() }
instance (Show int) => Show (Session int) where
  show Session{..} = unwords [ "Session", "pid=" <> show pid
                                        , "interpreter=" <> show interpreter
                                        , "[lock]", "[handles]" ]


-- | Bash as a coprocess.
data Bash = Bash
instance Show Bash where show _ = "bash"

instance Internals.Interpreter Bash where
  start _ = runInteractiveProcess "bash" [] Nothing (Just [])
  done  _ = Internals.close
  query _ (i, _, _, _) cmd = Internals.withFIFOs query'
   where query' ofo efo = do
           Bytes.hPut i (Bytes.unlines wrapped)
           hFlush i
           [ob, eb] <- Internals.backgroundReadFIFOs [ofo, efo]
           return (ob, eb)
          where wrapped = [ "{", cmd, "} 1>" <> (Esc.bytes . Esc.bash) ofo <>
                                       " 2>" <> (Esc.bytes . Esc.bash) efo    ]

