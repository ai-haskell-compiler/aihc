{-# LANGUAGE MagicHash #-}

module Main where

import Control.Exception (Exception, catch, throwIO)
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

data InnerException = InnerException

data OuterException = OuterException

instance Show InnerException where
  show InnerException = "InnerException"

instance Show OuterException where
  show OuterException = "OuterException"

instance Exception InnerException

instance Exception OuterException

ignoreInner :: InnerException -> IO ()
ignoreInner InnerException = hPutBuf stdout (Ptr "wrong handler\n"# :: Ptr ()) 14

handleOuter :: OuterException -> IO ()
handleOuter OuterException = hPutBuf stdout (Ptr "outer handler caught rethrow\n"# :: Ptr ()) 29

main :: IO ()
main =
  catch
    (catch (throwIO OuterException) ignoreInner)
    handleOuter
