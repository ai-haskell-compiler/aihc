{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Internal.Test.QuickCheck.Quid.Representations.Latin
    where

import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( replicateM )
import Data.Char
    ( ord )
import Data.Data
    ( Data )
import Data.Hashable
    ( Hashable (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.String
    ( IsString (..) )
import GHC.Generics
    ( Generic )
import Internal.Test.QuickCheck
    ( shrinkListNonEmpty )
import Internal.Test.QuickCheck.Quid
    ( Quid (..) )
import Internal.Test.QuickCheck.Quid.Representations
    ( nonEmptyListFromQuid, nonEmptyListToQuid )
import Test.QuickCheck
    ( Arbitrary (..)
    , Function
    , Gen
    , arbitraryBoundedEnum
    , shrinkMap
    , shrinkMapBy
    , sized
    )
import Text.Read
    ( Read (..), readMaybe )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Latin representation
--------------------------------------------------------------------------------

newtype Latin a = Latin { unLatin :: a }
    deriving stock (Data, Eq, Generic, Ord)
    deriving newtype (Hashable, NFData)
    deriving anyclass Function

instance Read (Latin Quid) where
    readPrec = fromString <$> readPrec

instance Show (Latin Quid) where
    show = show . latinStringFromQuid . unLatin

instance IsString (Latin Quid) where
    fromString = Latin . latinStringToQuid . fromString

--------------------------------------------------------------------------------
-- Latin characters
--------------------------------------------------------------------------------

data LatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Arbitrary LatinChar where
    arbitrary = arbitraryLatinChar
    shrink = shrinkLatinChar

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary Latin characters
--------------------------------------------------------------------------------

arbitraryLatinChar :: Gen LatinChar
arbitraryLatinChar = arbitraryBoundedEnum

shrinkLatinChar :: LatinChar -> [LatinChar]
shrinkLatinChar = shrinkMap toEnum fromEnum

--------------------------------------------------------------------------------
-- Conversion between Latin characters and ordinary characters
--------------------------------------------------------------------------------

charToLatinChar :: Char -> Maybe LatinChar
charToLatinChar c = readMaybe [c]

latinCharToChar :: LatinChar -> Char
latinCharToChar = toEnum . (+ ord 'A') . fromEnum

--------------------------------------------------------------------------------
-- Latin strings
--------------------------------------------------------------------------------

newtype LatinString = LatinString
    { unLatinString :: NonEmpty LatinChar }
    deriving stock (Eq, Ord)
    deriving newtype Semigroup

instance Arbitrary LatinString where
    arbitrary = arbitraryLatinString
    shrink = shrinkLatinString

--------------------------------------------------------------------------------
-- Conversion between Latin strings and ordinary strings
--------------------------------------------------------------------------------

instance Read LatinString where
    readPrec = fromString <$> readPrec

instance Show LatinString where
    show = show . latinStringToString

instance IsString LatinString where
    fromString = unsafeStringtoLatinString

latinStringToString :: LatinString -> String
latinStringToString (LatinString cs) = F.foldMap show cs

stringToLatinString :: String -> Maybe LatinString
stringToLatinString s =
    LatinString <$> (NE.nonEmpty =<< traverse charToLatinChar s)

unsafeStringtoLatinString :: String -> LatinString
unsafeStringtoLatinString = fromMaybe raiseError . stringToLatinString
  where
    raiseError = error $ unwords
        [ "A Latin quid string must be composed of one or more uppercase"
        , "characters in the range [A-Z]."
        ]

--------------------------------------------------------------------------------
-- Generation and shrinking of arbitrary Latin strings
--------------------------------------------------------------------------------

arbitraryLatinString :: Gen LatinString
arbitraryLatinString = sized $ \size ->
    fmap LatinString . (:|)
        <$> arbitraryLatinChar
        <*> replicateM size arbitraryLatinChar

shrinkLatinString :: LatinString -> [LatinString]
shrinkLatinString =
    shrinkMapBy LatinString unLatinString $ shrinkListNonEmpty shrinkLatinChar

--------------------------------------------------------------------------------
-- Conversion between Latin strings and quids
--------------------------------------------------------------------------------

latinStringToQuid :: LatinString -> Quid
latinStringToQuid = nonEmptyListToQuid . unLatinString

latinStringFromQuid :: Quid -> LatinString
latinStringFromQuid = LatinString . nonEmptyListFromQuid
