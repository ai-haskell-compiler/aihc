{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Base
  ( module GHC.Prim.Base,
    module GHC.Prim,
    module GHC.Classes,
    Bool (..),
    Int (..),
    Char (..),
    ord,
    unsafeChr,
    eqInt,
    neInt,
    ltInt,
    leInt,
    gtInt,
    geInt,
    quotInt,
    remInt,
    build,
    augment,
    unpackCString#,
    unpackCStringUtf8#,
    unpackFoldrCString#,
    ($),
    id,
    const,
    flip,
    (.),
    (++),
    foldr,
    map,
    otherwise,
    shiftL#,
    shiftRL#,
    iShiftL#,
    iShiftRA#,
    iShiftRL#,
  )
where

import GHC.CString (unpackCString#, unpackCStringUtf8#, unpackFoldrCString#)
import GHC.Classes
import GHC.Int (Int (..))
import GHC.Prim
import GHC.Prim.Base
import GHC.Types (Bool (..), Char (..), RuntimeRep, TYPE, Type, isTrue#)

-- | Convert a code point to a character without a range check.
unsafeChr :: Int -> Char
unsafeChr (I# value) = C# (chr# value)

ord :: Char -> Int
ord (C# value) = I# (ord# value)

-- | The monomorphic 'Int' comparisons.
eqInt, neInt, ltInt, leInt, gtInt, geInt :: Int -> Int -> Bool
eqInt (I# left) (I# right) = isTrue# (left ==# right)
neInt (I# left) (I# right) = isTrue# (left /=# right)
ltInt (I# left) (I# right) = isTrue# (left <# right)
leInt (I# left) (I# right) = isTrue# (left <=# right)
gtInt (I# left) (I# right) = isTrue# (left ># right)
geInt (I# left) (I# right) = isTrue# (left >=# right)

-- | The monomorphic 'Int' truncating division. Division by zero is the
-- caller's responsibility.
quotInt, remInt :: Int -> Int -> Int
quotInt (I# numerator) (I# denominator) = I# (quotInt# numerator denominator)
remInt (I# numerator) (I# denominator) = I# (remInt# numerator denominator)

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build generate = generate (:) []

augment :: (forall b. (a -> b -> b) -> b -> b) -> [a] -> [a]
augment generate = generate (:)

id :: a -> a
id x = x

const :: a -> b -> a
const value _ = value

flip :: (a -> b -> c) -> b -> a -> c
flip function right left = function left right

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = compose
  where
    compose value = f (g value)

infixr 9 .

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : (xs ++ ys)

infixr 5 ++

-- | The always-true guard.
otherwise :: Bool
otherwise = True

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map function (value : values) = function value : map function values

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ initial [] = initial
foldr combine initial (value : values) = combine value (foldr combine initial values)

($) :: forall (r :: RuntimeRep) (a :: Type) (b :: TYPE r). (a -> b) -> a -> b
($) function = function

infixr 0 $

-- | Shift a word left, giving zero for a shift of the word size or more.
-- The unchecked primop leaves such a shift undefined.
shiftL# :: Word# -> Int# -> Word#
shiftL# word count =
  case count >=# 64# of
    1# -> 0##
    _ -> uncheckedShiftL# word count

-- | Shift a word right, filling with zeros, giving zero for a shift of the
-- word size or more.
shiftRL# :: Word# -> Int# -> Word#
shiftRL# word count =
  case count >=# 64# of
    1# -> 0##
    _ -> uncheckedShiftRL# word count

-- | Shift an integer left, giving zero for a shift of the word size or more.
iShiftL# :: Int# -> Int# -> Int#
iShiftL# value count =
  case count >=# 64# of
    1# -> 0#
    _ -> uncheckedIShiftL# value count

-- | Shift an integer right arithmetically, replicating the sign bit. A shift
-- of the word size or more gives all sign bits.
iShiftRA# :: Int# -> Int# -> Int#
iShiftRA# value count =
  case count >=# 64# of
    1# ->
      case value <# 0# of
        1# -> negateInt# 1#
        _ -> 0#
    _ -> uncheckedIShiftRA# value count

-- | Shift an integer right, filling with zeros, giving zero for a shift of
-- the word size or more.
iShiftRL# :: Int# -> Int# -> Int#
iShiftRL# value count =
  case count >=# 64# of
    1# -> 0#
    _ -> uncheckedIShiftRL# value count
