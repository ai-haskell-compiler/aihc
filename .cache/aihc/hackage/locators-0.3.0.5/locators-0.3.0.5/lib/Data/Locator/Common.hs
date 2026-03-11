{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Locator.Common (
    Locator (..),
    represent,
    value,
    toLocatorUnique,
    multiply,
    fromLocator,
    concatToInteger,
    digest,
) where

import Prelude hiding (toInteger)

import Crypto.Hash as Crypto
import qualified Data.ByteArray as B
import qualified Data.ByteString.Char8 as S
import Data.List (mapAccumL)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word

class (Ord α, Enum α, Bounded α) => Locator α where
    locatorToDigit :: α -> Char
    digitToLocator :: Char -> α

represent :: Locator α => α -> Int -> Char
represent (_ :: α) n =
    locatorToDigit $ (toEnum n :: α)

{-
value :: Locator α => α -> Char -> Int
value c (_ :: α) =
    fromEnum $ (digitToLocator c :: α)
-}

value :: Locator α => α -> Char -> Int
value (_ :: α) c =
    fromEnum $ (digitToLocator c :: α)

--

{- |
Represent a number in Locator16a format. This uses the Locator16 symbol
set, and additionally specifies that no symbol can be repeated. The /a/ in
Locator16a represents that this transformation is done on the cheap; when
converting if we end up with \'9\' \'9\' we simply pick the subsequent digit
in the enum, in this case getting you \'9\' \'K\'.

Note that the transformation is /not/ reversible. A number like @4369@
(which is @0x1111@, incidentally) encodes as @12C4@. So do @4370@, @4371@,
and @4372@. The point is not uniqueness, but readibility in adverse
conditions. So while you can count locators, they don't map continuously to
base10 integers.

The first argument is the number of digits you'd like in the locator; if the
number passed in is less than 16^limit, then the result will be padded.

>>> toLocator16a 6 4369
12C40F
-}
toLocatorUnique :: Locator α => Int -> Int -> α -> String
toLocatorUnique limit n (_ :: α) =
    let n' = abs n
        ls = convert n' (replicate limit (minBound @α))
        (_, us) = mapAccumL uniq Set.empty ls
     in map locatorToDigit (take limit us)
  where
    convert :: Locator α => Int -> [α] -> [α]
    convert 0 xs = xs
    convert i xs =
        let (d, r) = divMod i 16
            x = toEnum r
         in convert d (x : xs)

    uniq :: Locator α => Set α -> α -> (Set α, α)
    uniq s x =
        if Set.member x s
            then uniq s (subsequent x)
            else (Set.insert x s, x)

    subsequent :: Locator α => α -> α
    subsequent x =
        if x == maxBound
            then minBound
            else succ x

multiply :: Locator α => α -> Int -> Char -> Int
multiply (locator :: a) acc c =
    let base = fromEnum (maxBound @a) + 1
     in (acc * base) + (value locator c)

--

-- | Given a number encoded as a Locator, convert it back to an integer.
fromLocator :: Locator α => α -> String -> Int
fromLocator locator ss =
    foldl (multiply locator) 0 ss

--
-- Given a string, convert it into a N character hash.
--
concatToInteger :: [Word8] -> Int
concatToInteger bytes =
    foldl fn 0 bytes
  where
    fn acc b = (acc * 256) + (fromIntegral b)

digest :: String -> Int
digest ws =
    i
  where
    i = concatToInteger h
    h = B.unpack h'
    h' = Crypto.hash x' :: Crypto.Digest Crypto.SHA1
    x' = S.pack ws
