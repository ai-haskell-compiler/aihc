{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

data Mark = Marked | Unmarked

data Payload = Payload

data Tag = Tag

class Markable a where
  mark :: a -> Mark

instance Markable Payload where
  mark Payload = Marked

data Box tag = forall value. (Markable value) => Box tag value

boxed :: Box Tag
boxed = Box Tag Payload

inspect :: Box tag -> Mark
inspect box =
  case box of
    Box _ value -> mark value

main :: IO ()
main =
  case inspect boxed of
    Marked -> hPutBuf stdout (Ptr "existential dictionary\n"# :: Ptr ()) 23
    Unmarked -> hPutBuf stdout (Ptr "missing dictionary\n"# :: Ptr ()) 19
