{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ROC.ID.Gender
  ( Gender (..)
  , printGender
  , randomGender
  ) where

import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )

import ROC.ID.Language
import ROC.ID.Utilities

-- | A person's gender, encodable within an ROC identification number.
--
data Gender = Male | Female
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- | Pretty-print the specified 'Gender'.
--
printGender :: Language -> Gender -> Text
printGender = \case
  English -> printGenderEnglish
  Chinese -> printGenderChinese

printGenderEnglish :: Gender -> Text
printGenderEnglish = \case
  Male   -> "Male"
  Female -> "Female"

printGenderChinese :: Gender -> Text
printGenderChinese = \case
  Male   -> "男性"
  Female -> "女性"

-- | Generate a random 'Gender'.
--
randomGender :: MonadRandom m => m Gender
randomGender = randomBoundedEnum

