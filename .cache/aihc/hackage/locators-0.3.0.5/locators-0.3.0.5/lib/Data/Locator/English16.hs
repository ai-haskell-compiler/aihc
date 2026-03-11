{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Locator.English16 (
    Locator (..),
    English16 (..),
    fromEnglish16,
    toEnglish16,
    toEnglish16a,
    hashStringToEnglish16a,
    -- Deprecated
    fromLocator16,
    toLocator16,
    toLocator16a,
    hashStringToLocator16a,
) where

import Prelude hiding (toInteger)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.List (mapAccumL)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showIntAtBase)

import Data.Locator.Common

--

{- |
A symbol set with sixteen uniquely pronounceable digits.

The fact there are sixteen symbols is more an indication of a certain degree
of bullheaded-ness on the part of the author, and less of any kind of actual
requirement. We might have a slighly better readback score if we dropped to
15 or 14 unique characters. It does mean you can match up with hexidecimal,
which is not entirely without merit.

The grouping of letters and numbers was the hard part; having come up with
the set and deconflicted the choices, the ordering is then entirely
arbitrary. Since there are some numbers, might as well have them at the same
place they correspond to in base 10; the letters were then allocated in alpha
order in the remaining slots.
-}

{-
        -- 0 Conflicts with @\'O\'@ obviously, and @\'Q\'@ often enough
        --
        -- 2 @\'U\'@, @\'W\'@, and @\'2\'@. @\'W\'@ is disqualifed because of
        -- the way Australians butcher double-this and triple-that. \"Double
        -- @\'U\'@\" or \"@\'W\'@\"?
        --
        -- C @\'B\'@, @\'C\'@, @\'D\'@, @\'E\'@, @\'G\'@, @\'P\'@, @\'T\'@,
        -- @\'V\'@, and @\'3\'@ plus @\'Z\'@ because Americans can't pronounce
        -- Zed properly.
        --
        -- 4 @\'4\'@ and @\'5\'@ are often confused, and @\'5\'@, definitely
        -- out due to its collision with @\'I\'@ when spoken and @\'S\'@ in
        -- writing.
        --
        -- F @\'F\'@ and @\'S\'@ are notoriously confused, making the choice of
        -- @\'F\'@ borderline, but @\'S\'@ is already disqualified for looking
        -- like @\'5\'@.
        --
        -- K group of @\'A\'@, @\'J\'@, @\'K\'@.
        --
        -- L @\'L\'@ has good phonetics, and as long as it's upper case (which
        -- the whole 'English16' symbol set is) there's no conflict with
        -- @\'1\'@.
        --
        -- M choice from @\'M\'@ and @\'N\'@; the latter is a little too close
        -- to @\'7\'@.
        --
        -- X choice from @\'X\'@ and @\'6\'@.
        --
        -- Y choice from @\'I\'@, @\'Y\'@, @\'5\'@. @\'I\'@ is out for the
        -- usual reason of being similar to @\'1\'@.
-}
data English16
    = -- | @\'0\'@ /0th/
      Zero
    | -- | @\'1\'@ /1st/
      One
    | -- | @\'2\'@ /2nd/
      Two
    | -- | @\'C\'@ /3rd/
      Charlie
    | -- | @\'4\'@ /4th/
      Four
    | -- | @\'F\'@ /5th/
      Foxtrot
    | -- | @\'H\'@ /6th/
      Hotel
    | -- | @\'7\'@ /7th/
      Seven
    | -- | @\'8\'@ /8th/
      Eight
    | -- | @\'9\'@ /9th/
      Nine
    | -- | @\'K\'@ /10th/
      Kilo
    | -- | @\'L\'@ /11th/
      Lima
    | -- | @\'M\'@ /12th/
      Mike
    | -- | @\'R\'@ /13th/
      Romeo
    | -- | @\'X\'@ /14th/
      XRay
    | -- | @\'Y\'@ /15th/
      Yankee
    deriving (Eq, Ord, Enum, Bounded)

instance Locator English16 where
    locatorToDigit :: English16 -> Char
    locatorToDigit x =
        case x of
            Zero -> '0'
            One -> '1'
            Two -> '2'
            Charlie -> 'C'
            Four -> '4'
            Foxtrot -> 'F'
            Hotel -> 'H'
            Seven -> '7'
            Eight -> '8'
            Nine -> '9'
            Kilo -> 'K'
            Lima -> 'L'
            Mike -> 'M'
            Romeo -> 'R'
            XRay -> 'X'
            Yankee -> 'Y'

    digitToLocator :: Char -> English16
    digitToLocator c =
        case c of
            '0' -> Zero
            '1' -> One
            '2' -> Two
            'C' -> Charlie
            '4' -> Four
            'F' -> Foxtrot
            'H' -> Hotel
            '7' -> Seven
            '8' -> Eight
            '9' -> Nine
            'K' -> Kilo
            'L' -> Lima
            'M' -> Mike
            'R' -> Romeo
            'X' -> XRay
            'Y' -> Yankee
            _ -> error "Illegal digit"

instance Show English16 where
    show x = [c]
      where
        c = locatorToDigit x

--

{- |
Given a number, convert it to a string in the English16 base 16 symbol
alphabet. You can use this as a replacement for the standard \'0\'-\'9\'
\'A\'-\'F\' symbols traditionally used to express hexidemimal, though really
the fact that we came up with 16 total unique symbols was a nice
co-incidence, not a requirement.
-}
toEnglish16 :: Int -> String
toEnglish16 x =
    showIntAtBase 16 (represent Yankee) x ""

--

{- |
Represent a number in English16a format. This uses the Locator16 symbol
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

>>> toEnglish16a 6 4369
12C40F
-}
toEnglish16a :: Int -> Int -> String
toEnglish16a limit n
    | limit > 16 = error "Can only request a maximum of 16 English16a characters, not " ++ (show limit)
    | otherwise =
        let n' = abs n
            ls = convert n' (replicate limit minBound) :: [English16]
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

--

-- | Given a number encoded in Locator16, convert it back to an integer.
fromEnglish16 :: [Char] -> Int
fromEnglish16 ss =
    foldl (multiply Yankee) 0 ss

--

{- |
Take an arbitrary sequence of bytes, hash it with SHA1, then format as a
short @digits@-long Locator16 string.

>>> hashStringToLocator16a 6 "Hello World"
M48HR0
-}
hashStringToEnglish16a :: Int -> ByteString -> ByteString
hashStringToEnglish16a limit s' =
    let s = S.unpack s'
        n = digest s -- SHA1 hash
        r = mod n upperBound -- trim to specified number of base 16 chars
        x = toLocator16a limit r -- express in locator16
        b' = S.pack x
     in b'
  where
    upperBound = 16 ^ limit

toLocator16 :: Int -> String
toLocator16 = toEnglish16
{-# DEPRECATED toLocator16 "Use toEnglish16 instead" #-}

toLocator16a :: Int -> Int -> String
toLocator16a = toEnglish16a
{-# DEPRECATED toLocator16a "Use toEnglish16a instead" #-}

fromLocator16 :: [Char] -> Int
fromLocator16 = fromEnglish16
{-# DEPRECATED fromLocator16 "Use fromEnglish16 instead" #-}

hashStringToLocator16a :: Int -> ByteString -> ByteString
hashStringToLocator16a = hashStringToEnglish16a
{-# DEPRECATED hashStringToLocator16a "Use hashStringToEnglish16a instead" #-}
