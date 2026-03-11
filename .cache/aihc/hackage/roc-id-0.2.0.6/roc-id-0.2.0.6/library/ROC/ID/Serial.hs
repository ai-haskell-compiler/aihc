{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module ROC.ID.Serial
  ( Serial (..)
  , randomSerial
  ) where

import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Vector.Sized
    ( Vector )
import GHC.Generics
    ( Generic )

import ROC.ID.Digit
import ROC.ID.Utilities

import qualified Data.Vector.Sized as V

-- | A 7-digit serial number, as found within an ROC identification number.
--
-- A serial number is unique for a gender and location.
--
-- To generate a random 'Serial' number, use the 'randomSerial' function.
--
newtype Serial = Serial (Vector 7 Digit)
  deriving (Eq, Generic, Ord, Show)

-- | Generate a random 'Serial' number.
--
randomSerial :: MonadRandom m => m Serial
randomSerial = Serial <$> V.replicateM randomBoundedEnum

