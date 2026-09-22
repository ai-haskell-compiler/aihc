{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# HLINT ignore foldr "Eta reduce" #-}
{-# HLINT ignore mapFB "Eta reduce" #-}
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
    mapFB,
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

-- List fusion follows GHC: a good producer is written with 'build', a
-- good consumer with 'foldr', and the rules below remove the list between
-- them. The phases count down: a producer turns into its 'build' form in
-- phase 2, where the fusion rules fire, and what did not fuse turns back
-- into its plain form in phase 1, where 'build' and the @FB@ helpers are
-- inlined. See @docs/optimization.md@.

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build generate = generate (:) []
{-# INLINE [1] build #-}

augment :: (forall b. (a -> b -> b) -> b -> b) -> [a] -> [a]
augment generate = generate (:)
{-# INLINE [1] augment #-}

{-# RULES
"fold/build" forall k z (g :: forall b. (a -> b -> b) -> b -> b). foldr k z (build g) = g k z
"foldr/augment" forall k z xs (g :: forall b. (a -> b -> b) -> b -> b). foldr k z (augment g xs) = g k (foldr k z xs)
"foldr/id" foldr (:) [] = \x -> x
"foldr/app" [1] forall ys. foldr (:) ys = (++ ys)
"foldr/single" forall k z x. foldr k z [x] = k x z
"foldr/nil" forall k z. foldr k z [] = z
"foldr/cons/build" forall k z x (g :: forall b. (a -> b -> b) -> b -> b). foldr k z (x : build g) = k x (g k z)
"augment/build" forall (g :: forall b. (a -> b -> b) -> b -> b) (h :: forall b. (a -> b -> b) -> b -> b). augment g (build h) = build (\c n -> g c (h c n))
"augment/nil" forall (g :: forall b. (a -> b -> b) -> b -> b). augment g [] = build g
  #-}

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

-- An append is a producer through 'augment'. The second rule turns an
-- append that did not fuse back into itself without waiting for 'augment'
-- to be inlined, which a size-bound pass may not do.
{-# RULES
"++" [~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
"++/augment" [1] forall xs ys. augment (\c n -> foldr c n xs) ys = xs ++ ys
  #-}

-- | The always-true guard.
otherwise :: Bool
otherwise = True

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map function (value : values) = function value : map function values

-- | The step of a 'map' written as a 'foldr': the consumer @c@ gets the
-- mapped element.
mapFB :: (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
mapFB c f x ys = c (f x) ys
{-# INLINE [1] mapFB #-}

-- Up to phase 1 a saturated 'map' is its 'build' form, so that it fuses;
-- from phase 1 the form that did not fuse is a 'map' again, whether or
-- not 'build' was inlined, and compositions of steps collapse.
{-# RULES
"map" [~1] forall f xs. map f xs = build (\c n -> foldr (mapFB c f) n xs)
"mapList" [1] forall f. foldr (mapFB (:) f) [] = map f
"map/build" [1] forall f xs. build (\c n -> foldr (mapFB c f) n xs) = map f xs
"mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f . g)
"mapFB/id" forall c. mapFB c (\x -> x) = c
  #-}

-- 'foldr' takes all three arguments in its head, unlike GHC's, so that a
-- consumer written as a partial application, such as @sum = foldr (+) 0@,
-- is a function of its list and is copied at its calls, where it fuses.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr combine initial values = go values
  where
    go [] = initial
    go (value : rest) = combine value (go rest)
{-# INLINE [1] foldr #-}

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
