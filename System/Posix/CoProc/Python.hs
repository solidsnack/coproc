{-# LANGUAGE OverloadedStrings #-}
-- | CoProc implementations for Python.
module System.Posix.CoProc.Python where

import qualified Data.ByteString
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import           Data.Monoid
import           System.IO
import           System.Process
import           Text.Printf (printf)

import qualified Text.ShellEscape as Esc

import qualified System.Posix.CoProc.Internals as Internals


-- | Python as a coprocess.
data Python = Python
instance Show Python where show _ = "python"
instance Internals.Interpreter Python where
  start _ = go "python" ["-i", "-u"]
  done  _ = Internals.close
  query _ handles cmd = runRedirected handles cmd


-- | Python2 as a coprocess.
data Python2 = Python2
instance Show Python2 where show _ = "python"
instance Internals.Interpreter Python2 where
  start _ = go "python2" []
  done  _ = Internals.close
  query _ handles cmd = runRedirected handles cmd


-- | Python3 as a coprocess.
data Python3 = Python3
instance Show Python3 where show _ = "python"
instance Internals.Interpreter Python3 where
  start _ = go "python3" []
  done  _ = Internals.close
  query _ handles cmd = runRedirected handles cmd


-- | Redirecting wrapper for Pythons.
redirect :: (ByteString, ByteString) -> ByteString -> ByteString
redirect (o, e) userCode = Bytes.unlines [
    "import os",
    "",
    outVar <> " = os.dup(1)",
    "os.close(1)",
    "os.open('" <> o <> "', os.O_RDWR|os.O_CREAT, 0)",
    errVar <> " = os.dup(2)",
    "os.close(2)",
    "os.open('" <> e <> "', os.O_RDWR|os.O_CREAT, 0)",
    "",
    userCode,
    "",
    "os.close(1)",
    "os.dup(" <> outVar <> ")",
    "os.close(" <> outVar <> ")",
    "os.close(2)",
    "os.dup(" <> errVar <> ")",
    "os.close(" <> errVar <> ")",
    "del " <> outVar,
    "del " <> errVar,
    ""
  ]
 where
  hexStub = Data.ByteString.concatMap (Bytes.pack . printf "%02x")
          . Bytes.take 8 . Bytes.reverse
  (outVar, errVar) = ("out_" <> hexStub o, "err_" <> hexStub e)


-- | Setup FIFOs and run a command, capturing its output.
runRedirected (i, _, _, _) cmd = Internals.withFIFOs query'
 where query' ofo efo = do let code = redirect (ofo, efo) cmd
                           Bytes.hPutStrLn stderr code
                           Bytes.hPut i code
                           hFlush i
                           [ob, eb] <- Internals.backgroundReadFIFOs [ofo, efo]
                           return (ob, eb)

-- | Launch Python.
go py flags = runInteractiveProcess py flags Nothing (Just [])
