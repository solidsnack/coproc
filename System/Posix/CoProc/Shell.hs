{-# LANGUAGE OverloadedStrings #-}
-- | CoProc implementations for shells.
module System.Posix.CoProc.Shell where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import           Data.Monoid
import           System.IO
import           System.Process

import qualified Text.ShellEscape as Esc

import qualified System.Posix.CoProc.Internals as Internals


-- | Bash as a coprocess. Simply starts @bash@ with no arguments, using
--   whatever @bash@ is first in the @PATH@.
data Bash = Bash
instance Show Bash where show _ = "bash"
instance Internals.Interpreter Bash where
  start _ = go "bash" []
  done  _ = Internals.close
  query _ handles cmd = runRedirected handles cmd

-- | Sh as a coprocess. Starts @/bin/sh@ with no arguments.
data BinSh = BinSh
instance Show BinSh where show _ = "/bin/sh"
instance Internals.Interpreter BinSh where
  start _ = go "/bin/sh" []
  done  _ = Internals.close
  query _ handles cmd = runRedirected handles cmd

-- | Launch any Bourne-compatible shell as a coprocess.
data BourneLike = BourneLike String [String]
instance Show BourneLike where show (BourneLike path flags) = show (path:flags)
instance Internals.Interpreter BourneLike where
  start (BourneLike path flags) = go path flags
  done  _ = Internals.close
  query _ handles cmd = runRedirected handles cmd


-- | Redirecting wrapper for Bourne-compatible shell.
redirect :: (ByteString, ByteString) -> ByteString -> ByteString
redirect (o,e) c = Bytes.unlines [ "{", c, "} 1>" <> (Esc.bytes . Esc.sh) o <>
                                            " 2>" <> (Esc.bytes . Esc.sh) e    ]

-- | Setup FIFOs and run a command, capturing its output.
runRedirected (i, _, _, _) cmd = Internals.withFIFOs query'
 where query' ofo efo = do Bytes.hPut i (redirect (ofo, efo) cmd)
                           hFlush i
                           [ob, eb] <- Internals.backgroundReadFIFOs [ofo, efo]
                           return (ob, eb)

-- | Launch a shell.
go shell flags = runInteractiveProcess shell flags Nothing (Just [])
