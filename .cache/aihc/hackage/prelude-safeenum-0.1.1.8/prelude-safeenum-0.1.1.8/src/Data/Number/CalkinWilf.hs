{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ >= 701
-- N.B., GeneralizedNewtypeDeriving isn't "safe".
{-# LANGUAGE Trustworthy #-}
#endif
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  Data.Number.CalkinWilf
-- Copyright   :  2012--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  Haskell98 + CPP + GeneralizedNewtypeDeriving
--
-- Enumerate the rationals in Calkin--Wilf order. That is, when we
-- give enumeration a well-specified meaning (as "Prelude.SafeEnum"
-- does) this renders instances for 'Ratio' problematic. 'Ratio'
-- instances /can/ be provided so long as the base type is integral
-- and enumerable; but they must be done in an obscure order that
-- does not coincide with the 'Ord' instance for 'Ratio'. Since
-- this is not what people may expect, we only provide an instance
-- for the newtype 'CalkinWilf', not for 'Ratio' itself.
--
--   * Jeremy Gibbons, David Lester, and Richard Bird (2006).
--     /Enumerating the Rationals/. JFP 16(3):281--291.
--     DOI:10.1017\/S0956796806005880
--     <http://www.cs.ox.ac.uk/jeremy.gibbons/publications/rationals.pdf>
----------------------------------------------------------------
module Data.Number.CalkinWilf (CalkinWilf(..), unCalkinWilf) where

import Prelude hiding (Enum(..))
import qualified Prelude (Enum(..))
import Prelude.SafeEnum
import Data.Ratio
import Data.List (elemIndex)

----------------------------------------------------------------
-- | Enumerate the rationals in Calkin--Wilf order. The enumeration
-- is symmetric about zero, ensuring that all the negative rationals
-- come before zero and all the positive rationals come after zero.
--
-- BUG: while the 'succeeds', 'precedes', 'toEnum', and 'fromEnum'
-- methods are correct, they are horribly inefficient. This can be
-- rectified (or at least mitigated), but this remains to be done.
newtype CalkinWilf a = CalkinWilf (Ratio a)
    deriving (Read, Show, Eq, Ord, Num, Fractional, Real, RealFrac)
    -- BUG: Haddock does a horrible job with the generated contexts...


-- | Return the underlying 'Ratio'. Not using record syntax to
-- define this in order to pretty up the derived 'Show' instance.
unCalkinWilf :: CalkinWilf a -> Ratio a
unCalkinWilf (CalkinWilf q) = q
{-# INLINE unCalkinWilf #-}


succCW :: Integral a => CalkinWilf a -> CalkinWilf a
{-# SPECIALIZE succCW :: CalkinWilf Integer -> CalkinWilf Integer #-}
succCW x
    | x < 0 =
        let y = recip x + 1
        in  2 * fromInteger(floor y) - y
    | otherwise =
        let (n,y) = properFraction x
        in  recip (fromInteger n + 1 - y)


predCW :: Integral a => CalkinWilf a -> CalkinWilf a
{-# SPECIALIZE predCW :: CalkinWilf Integer -> CalkinWilf Integer #-}
predCW x
    | x > 0 =
        let y = recip x - 1
        in  2 * fromInteger(ceiling y) - y
    | otherwise =
        let (n,y) = properFraction x
        in  recip (fromInteger n - 1 - y)


-- TODO: We could probably speed everything below up by using the @mod@-based algorithm for 'igcd' and adding on the necessary number of bits; and by replacing [Bool] with a Word where the highest set bit indicates the end of the list. The trick, then, is what to do with [Bool] too large to fit into a Word?


-- TODO: does 'elemIndex' fail if the resulting Int would overflow?
cw2mbint :: Integral a => CalkinWilf a -> Maybe Int
{-# SPECIALIZE cw2mbint :: CalkinWilf Integer -> Maybe Int #-}
cw2mbint q =
    case compare q 0 of
    GT -> fmap (1+) (elemIndex (cw2bits q) boolseqs)
    EQ -> Just 0
    LT -> fmap (negate . (1+)) (elemIndex (cw2bits (abs q)) boolseqs)
    where
    -- Using a local definition to try to avoid memoization
    boolseqs = [] : [ b:bs | bs <- boolseqs, b <- [False,True]]


cw2bits :: Integral a => CalkinWilf a -> [Bool]
{-# SPECIALIZE cw2bits :: CalkinWilf Integer -> [Bool] #-}
cw2bits (CalkinWilf q) = snd (igcd (numerator q) (denominator q))


igcd :: Integral a => a -> a -> (a,[Bool])
{-# SPECIALIZE igcd :: Integer -> Integer -> (Integer,[Bool]) #-}
igcd 0 0 = (0,[])
igcd m n
    | m < 0 || n < 0 = error "igcd is undefined on negative arguments"
    | otherwise =
        case compare m n of
        LT -> second (False:) (igcd m (n-m))
        GT -> second (True:)  (igcd (m-n) n)
        EQ -> (m,[])
    where
    second f (x, y) = (x, f y)


int2cw :: Integral a => Int -> CalkinWilf a
{-# SPECIALIZE int2cw :: Int -> CalkinWilf Integer #-}
int2cw i
    | i == minBound = (predCW . negate . posnat2cw . negate) (i+1)
    | otherwise     =
        case compare i 0 of
        GT -> posnat2cw i
        EQ -> 0
        LT -> (negate . posnat2cw . negate) i -- Beware when i == minBound

posnat2cw :: Integral a => Int -> CalkinWilf a
{-# SPECIALIZE posnat2cw :: Int -> CalkinWilf Integer #-}
posnat2cw i = bits2cw (boolseqs !! (i-1))
    where
    -- Using a local definition to try to avoid memoization
    boolseqs = [] : [ b:bs | bs <- boolseqs, b <- [False,True]]

bits2cw :: Integral a => [Bool] -> CalkinWilf a
{-# SPECIALIZE bits2cw :: [Bool] -> CalkinWilf Integer #-}
bits2cw bs =
    let (m,n) = foldr undo (1,1) bs
    in CalkinWilf (m % n)
    -- GHC.Real doesn't export (:%), but we know this is already normalized...
    where
    undo False (m,n) = (m, n+m)
    undo True  (m,n) = (m+n, n)


----------------------------------------------------------------
instance Integral a => UpwardEnum (CalkinWilf a) where
    succ = Just . succCW
    -- BUG: What about when 'cw2mbint' fails?
    x `succeeds` y = cw2mbint x > cw2mbint y
    {-# INLINE succ #-}
    {-# INLINE succeeds #-}


instance Integral a => DownwardEnum (CalkinWilf a) where
    pred = Just . predCW
    -- BUG: What about when 'cw2mbint' fails?
    x `precedes` y = cw2mbint x < cw2mbint y
    {-# INLINE pred #-}
    {-# INLINE precedes #-}


instance Integral a => Enum (CalkinWilf a) where
    toEnum   = Just . int2cw
    fromEnum = cw2mbint
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}


instance Integral a => Prelude.Enum (CalkinWilf a) where
    succ     = succCW
    pred     = predCW
    toEnum   = int2cw
    fromEnum = maybe _fromEnum_OOR id . cw2mbint
    enumFrom = iterate succCW
    {-# INLINE succ #-}
    {-# INLINE pred #-}
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}
    {-# INLINE enumFrom #-}

    -- TODO: enumFromThen :: a -> a -> [a]
    -- TODO: enumFromTo :: a -> a -> [a]
    -- TODO: enumFromThenTo :: a -> a -> a -> [a]

----------------------------------------------------------------
_fromEnum_OOR :: a
_fromEnum_OOR =
    error "Enum.fromEnum{CalkinWilf}: argument out of range"
{-# NOINLINE _fromEnum_OOR #-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
