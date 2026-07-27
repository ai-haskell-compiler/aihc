{-# LANGUAGE MagicHash #-}

module Main where

import Data.Typeable (cast)
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

data Box a = Box a

main :: IO ()
main =
  case (cast (Box True) :: Maybe (Box Bool)) of
    Just boxed ->
      case boxed of
        Box True -> hPutBuf stdout (Ptr "cast succeeded\n"# :: Ptr ()) 15
        Box False -> hPutBuf stdout (Ptr "cast failed\n"# :: Ptr ()) 12
    Nothing -> hPutBuf stdout (Ptr "cast failed\n"# :: Ptr ()) 12
