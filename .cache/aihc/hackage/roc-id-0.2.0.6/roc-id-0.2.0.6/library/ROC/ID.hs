{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ROC.ID
  ( Identity (..)
  , identityChecksum
  , parseIdentity
  , ParseError (..)
  , randomIdentity
  ) where

import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Tuple.Only
    ( Only (..) )
import Data.Vector.Sized
    ( Vector )
import GHC.Generics
    ( Generic )

import ROC.ID.Digit
import ROC.ID.Gender
import ROC.ID.Location
import ROC.ID.Serial
import ROC.ID.Utilities

import qualified Data.Text as T
import qualified Data.Vector.Sized as V

-- Types:

-- | Represents a __valid__ 10-digit ROC national identification number
-- (中華民國身份證號碼) of the form __@A123456789@__.
--
-- By construction, invalid values are __not representable__ by this type.
--
-- An identification number encodes a person's 'Gender', the 'Location' in
-- which they first registered for an identification card, and a unique 'Serial'
-- number.
--
data Identity = Identity
  { identityGender   :: Gender
  -- ^ The gender of the person to whom this ID number belongs.
  , identityLocation :: Location
  -- ^ The location in which the person first registered for an ID card.
  , identitySerial   :: Serial
  -- ^ The serial number portion of this ID number.
  } deriving (Eq, Generic, Ord)

instance Show Identity where
  show i@Identity {..} = ""
    <> show identityLocation
    <> foldMap show (toDigits identityGender)
    <> foldMap show (toDigits identitySerial)
    <> show (identityChecksum i)

-- | Calculate the checksum of the specified 'Identity'.
--
identityChecksum :: Identity -> Digit
identityChecksum Identity {..} = toEnum $ negate total `mod` 10
  where
    total = 1 * p 0 + 9 * p 1 + 8 * g 0 + 7 * s 0 + 6 * s 1
          + 5 * s 2 + 4 * s 3 + 3 * s 4 + 2 * s 5 + 1 * s 6
    g = index identityGender
    p = index identityLocation
    s = index identitySerial
    index x = fromEnum . V.index e
      where
        e = toDigits x

class ToDigits t n | t -> n where
  toDigits :: t -> Vector n Digit

instance ToDigits Gender 1 where
  toDigits = V.fromTuple . Only . \case
    Male   -> D1
    Female -> D2

instance ToDigits Location 2 where
  toDigits = V.fromTuple . \case
    A -> (D1, D0); N -> (D2, D2)
    B -> (D1, D1); O -> (D3, D5)
    C -> (D1, D2); P -> (D2, D3)
    D -> (D1, D3); Q -> (D2, D4)
    E -> (D1, D4); R -> (D2, D5)
    F -> (D1, D5); S -> (D2, D6)
    G -> (D1, D6); T -> (D2, D7)
    H -> (D1, D7); U -> (D2, D8)
    I -> (D3, D4); V -> (D2, D9)
    J -> (D1, D8); W -> (D3, D2)
    K -> (D1, D9); X -> (D3, D0)
    L -> (D2, D0); Y -> (D3, D1)
    M -> (D2, D1); Z -> (D3, D3)

instance ToDigits Serial 7 where
  toDigits (Serial c) = c

-- | Attempt to parse an 'Identity' using the specified 'Text' as input.
--
-- The input must be of the form __@A123456789@__.
--
parseIdentity :: Text -> Either ParseError Identity
parseIdentity t = do
    v <-              guard InvalidLength   (parseRaw                     t)
    i <- Identity <$> guard InvalidGender   (parseGender   $ readGender   v)
                  <*> guard InvalidLocation (parseLocation $ readLocation v)
                  <*> guard InvalidSerial   (parseSerial   $ readSerial   v)
    c <-              guard InvalidChecksum (parseDigit    $ readChecksum v)
    if c == identityChecksum i then pure i else Left InvalidChecksum
  where
    readSerial   = V.slice (Proxy :: Proxy 2)
    readLocation = flip V.index 0
    readGender   = flip V.index 1
    readChecksum = flip V.index 9

-- | An error produced when parsing an 'Identity' with the 'parseIdentity'
--   function.
--
data ParseError
  = InvalidLength
    -- ^ The input was either too short or too long.
  | InvalidGender
    -- ^ The gender portion of the input was invalid.
  | InvalidLocation
    -- ^ The location portion of the input included non-alphabetic characters.
  | InvalidSerial
    -- ^ The serial number portion of the input included non-numeric characters.
  | InvalidChecksum
    -- ^ The computed checksum did not match the checksum portion of the input.
  deriving (Eq, Show)

parseRaw :: Text -> Maybe (Vector 10 Char)
parseRaw  = V.fromList . T.unpack

parseGender :: Char -> Maybe Gender
parseGender = \case
  '1' -> pure Male
  '2' -> pure Female
  _   -> Nothing

parseSerial :: Vector 7 Char -> Maybe Serial
parseSerial a = Serial <$> traverse parseDigit a

-- | Generate a random 'Identity'.
--
randomIdentity :: MonadRandom m => m Identity
randomIdentity = Identity <$> randomGender
                          <*> randomLocation
                          <*> randomSerial

