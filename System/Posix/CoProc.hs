{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , TupleSections #-}
module System.Posix.CoProc where

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
import           System.Exit
import           System.IO
import           System.Process
import           System.Posix.ByteString

import qualified System.Posix.CoProc.Internals as Internals


start :: (Internals.Interpreter int) => int -> IO (Session int)
start interpreter = do
  handles@(_,_,_,p) <- Internals.boot interpreter
  pid               <- Internals.pid p
  Session pid interpreter handles <$> newMVar ()

query :: (Internals.Interpreter int) => Session int
      -> ByteString
      -> IO (ByteString, ByteString)
query Session{..} q = withMVar lock
                               (const (Internals.send interpreter handles q))

done :: (Internals.Interpreter int) => Session int -> IO (CPid, ExitCode)
done Session{..} = (pid,) <$> Internals.end interpreter handles

data Session int = Session { pid :: CPid
                           , interpreter :: int
                           , handles :: (Handle, Handle, Handle, ProcessHandle)
                           , lock :: MVar() }
instance (Show int) => Show (Session int) where
  show Session{..} = unwords [ "Session", "pid=" <> show pid
                                        , "interpreter=" <> show interpreter
                                        , "[lock]", "[handles]" ]

