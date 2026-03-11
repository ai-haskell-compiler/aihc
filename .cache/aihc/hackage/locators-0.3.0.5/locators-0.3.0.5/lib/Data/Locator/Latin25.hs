{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Locator.Latin25 (
    Latin25 (..),
    toLatin25,
    fromLatin25,
    hashStringToLatin25,
) where

import Prelude hiding (toInteger)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Numeric (showIntAtBase)

import Data.Locator.Common
import Data.Locator.Hashes (padWithZeros)

{- |
A symbol set with twenty-five visually distinct characters.

These are not protected against similar pronounciations; if you need to
read your identifiers /aloud/ use 'English16' instead.
-}

{-

    --  | Two       -- Obvious conflict with Z
    --  | Five      -- Obvious conflict with S
    --  | Six       -- Too close to G
    --  | Bravo     -- Too close to 8
    --  | Delta     -- Shape of D too close to O
    --  | Foxtrot   -- Excluded because too close to E
    --  | India     -- Too close to 1 and J
    --  | Oscar     -- Obvious conflict with 0
    --  | Quebec    -- The tail on Q is too easy to miss, thereby colliding with O/0
    --  | Romeo     -- Dropped in favour of P
    --  | Uniform   -- Too close to V

-}
data Latin25
    = -- | @\'0\'@ /0th/
      Zero'
    | -- | @\'1\'@ /1st/
      One'
    | -- | @\'3\'@ /2nd/
      Three'
    | -- | @\'4\'@ /3rd/
      Four'
    | -- | @\'7\'@ /4th/
      Seven'
    | -- | @\'8\'@ /5th/
      Eight'
    | -- | @\'9\'@ /6th/
      Nine'
    | -- | @\'A\'@ /7th/
      Alpha'
    | -- | @\'C\'@ /8th/
      Charlie'
    | -- | @\'E\'@ /9th/
      Echo'
    | -- | @\'G\'@ /10th/
      Golf'
    | -- | @\'H\'@ /11th/
      Hotel'
    | -- | @\'J\'@ /12th/
      Juliet'
    | -- | @\'K\'@ /13th/
      Kilo'
    | -- | @\'L\'@ /14th/
      Lima'
    | -- | @\'M\'@ /15th/
      Mike'
    | -- | @\'N\'@ /16th/
      November'
    | -- | @\'P\'@ /17th/
      Papa'
    | -- | @\'S\'@ /18th/
      Sierra'
    | -- | @\'T\'@ /19th/
      Tango'
    | -- | @\'V\'@ /20th/
      Victor'
    | -- | @\'W\'@ /21st/
      Whiskey'
    | -- | @\'X\'@ /22nd/
      XRay'
    | -- | @\'Y\'@ /23rd/
      Yankee'
    | -- | @\'Z\'@ /24th/
      Zulu'
    deriving (Eq, Ord, Enum, Bounded)

instance Locator Latin25 where
    locatorToDigit x =
        case x of
            Zero' -> '0'
            One' -> '1'
            Three' -> '3'
            Four' -> '4'
            Seven' -> '7'
            Eight' -> '8'
            Nine' -> '9'
            Alpha' -> 'A'
            Charlie' -> 'C'
            Echo' -> 'E'
            Golf' -> 'G'
            Hotel' -> 'H'
            Juliet' -> 'J'
            Kilo' -> 'K'
            Lima' -> 'L'
            Mike' -> 'M'
            November' -> 'N'
            Papa' -> 'P'
            Sierra' -> 'S'
            Tango' -> 'T'
            Victor' -> 'V'
            Whiskey' -> 'W'
            XRay' -> 'X'
            Yankee' -> 'Y'
            Zulu' -> 'Z'

    digitToLocator :: Char -> Latin25
    digitToLocator c =
        case c of
            '0' -> Zero'
            '1' -> One'
            '3' -> Three'
            '4' -> Four'
            '7' -> Seven'
            '8' -> Eight'
            '9' -> Nine'
            'A' -> Alpha'
            'C' -> Charlie'
            'E' -> Echo'
            'G' -> Golf'
            'H' -> Hotel'
            'J' -> Juliet'
            'K' -> Kilo'
            'L' -> Lima'
            'M' -> Mike'
            'N' -> November'
            'P' -> Papa'
            'S' -> Sierra'
            'T' -> Tango'
            'W' -> Whiskey'
            'V' -> Victor'
            'X' -> XRay'
            'Y' -> Yankee'
            'Z' -> Zulu'
            _ -> error "Illegal digit"

instance Show Latin25 where
    show x = [c]
      where
        c = locatorToDigit x

--

{- |
Given a number, convert it to a string in the Latin25 base 25 symbol
alphabet. This is useful for primary keys and object identifiers that you
need to scan for in log output, for example.
-}
toLatin25 :: Int -> String
toLatin25 x =
    showIntAtBase 25 (represent Zulu') x ""

--

-- | Given a number encoded in Locator16, convert it back to an integer.
fromLatin25 :: String -> Int
fromLatin25 ss =
    foldl (multiply Zulu') 0 ss

--

{- |
Take an arbitrary sequence of bytes, hash it with SHA1, then format as a
short @limit@-long Latin25 string.

>>> hashStringToLatin25 5 "You'll get used to it. Or, you'll have a psychotic episode"
SG8XP

17 characters is the widest hash you can request.
-}
hashStringToLatin25 :: Int -> ByteString -> ByteString
hashStringToLatin25 limit s'
    | limit > 17 = error "Can only request a maximum width of 17, sorry"
    | otherwise =
        let s = S.unpack s'
            n = digest s -- SHA1 hash
            r = mod n upperBound -- trim to specified number of base 25 chars
            x = toLatin25 r -- express in Latin25
            b' = S.pack (padWithZeros limit x)
         in b'
  where
    upperBound = 25 ^ limit
