
module Data.ByteString.ShellEscape.EscapeVector where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector (new, write)

import qualified Data.ByteString.ShellEscape.Put as Put


type EscapeVector escapingMode = Vector (Char, escapingMode)

escWith :: (Char -> escapingMode) -> ByteString -> EscapeVector escapingMode
escWith cf b                 =  Vector.create $ do
  v                         <-  Vector.new (ByteString.length b)
  sequence_ . snd $ ByteString.foldl' (f v) (0, []) b
  return v
 where
  f v (i, ops) c             =  (i + 1, Vector.write v i (c, cf c) : ops)


stripEsc                    ::  Vector (Char, escapingMode) -> ByteString
stripEsc v                   =  ByteString.unfoldr f . fst $ Vector.unzip v
 where
  f v | Vector.null v        =  Nothing
      | otherwise            =  Just (Vector.unsafeHead v, Vector.unsafeTail v)

interpretEsc v f finish init =  (eval . (finish lastMode :)) instructions
 where
  eval                       =  Put.runPut' . sequence_ . reverse
  (instructions, lastMode)   =  Vector.foldl' f' init v
   where
    f' (list, mode) (c, e)   =  (put:list, mode')
     where
      (put, mode')           =  f mode (c, e)

