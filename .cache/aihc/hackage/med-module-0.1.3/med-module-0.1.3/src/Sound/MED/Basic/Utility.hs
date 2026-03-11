module Sound.MED.Basic.Utility where

import qualified Data.List.Reverse.StrictSpine as ListRev
import qualified Data.Traversable as Trav
import Data.List.HT (sliceVertical)
import Data.Maybe.HT (toMaybe)

import Data.Word (Word8, Word16, Word32)
import Data.Int (Int8, Int16, Int32)


type PTR   = Word32
type LONG  = Int32
type ULONG = Word32
type WORD  = Int16
type UWORD = Word16
type BYTE  = Int8
type UBYTE = Word8

infixr 0 $?

($?) :: (Monad m) => (PTR -> m a) -> PTR -> m (Maybe a)
f $? ptr = skipIf (ptr == 0) (f ptr)

skipIf :: (Monad m) => Bool -> m a -> m (Maybe a)
skipIf cond act = Trav.sequence $ toMaybe (not cond) act


pointerRange :: PTR -> ULONG -> Int -> [PTR]
pointerRange start step len =
  take len $ iterate (fromIntegral step +) start

pointerRangeGen :: (Integral i) => PTR -> ULONG -> i -> [PTR]
pointerRangeGen start step len = pointerRange start step (fromIntegral len)

{- |
Return empty list if start pointer is zero.
-}
pointerRangeGenCheck :: (Integral i) => PTR -> ULONG -> i -> [PTR]
pointerRangeGenCheck start step len =
  if start == 0 then [] else pointerRangeGen start step len

pointerRangeGen2 :: (Integral i, Integral j) => PTR -> ULONG -> i -> j -> [PTR]
pointerRangeGen2 start step len0 len1 =
  pointerRange start step (fromIntegral len0 * fromIntegral len1)


chunk :: (Integral i) => i -> [a] -> [[a]]
chunk k = sliceVertical (fromIntegral k)

-- | Strings tend to be fixed width fields with trailing zeros.
stringFromBytes :: [UBYTE] -> String
stringFromBytes = map (toEnum . fromEnum) . ListRev.dropWhile (==0)
