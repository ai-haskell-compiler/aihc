{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Base
  ( module GHC.Prim.Base,
    module GHC.Prim,
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
  )
where

import GHC.CString (unpackCString#, unpackCStringUtf8#, unpackFoldrCString#)
import GHC.Int (Int (..))
import GHC.Prim
import GHC.Prim.Base
import GHC.Types (Bool, Char (..), RuntimeRep, TYPE, Type, isTrue#)

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

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ initial [] = initial
foldr combine initial (value : values) = combine value (foldr combine initial values)

($) :: forall (r :: RuntimeRep) (a :: Type) (b :: TYPE r). (a -> b) -> a -> b
($) function = function

infixr 0 $
