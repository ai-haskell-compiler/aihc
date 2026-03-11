{-# LANGUAGE TypeSynonymInstances #-}

module HaskellWorks.Data.Positioning
  ( Count
  , Position
  , lastPositionOf
  , toCount
  , toPosition
  ) where

import Data.Int
import Data.Word

-- | A value representing a count
type Count = Word64

-- | A value representing a position
type Position = Int64

class ToPosition a where
  -- | Convert to a position
  toPosition :: a -> Position

instance ToPosition Count where
  toPosition = fromIntegral
  {-# INLINE toPosition #-}

class ToCount a where
  -- | Convert a count to a count
  toCount :: a -> Count

instance ToCount Position where
  toCount = fromIntegral
  {-# INLINE toCount #-}

-- | Get largest position in a sequenced container of size count.
lastPositionOf :: Count -> Position
lastPositionOf c  = fromIntegral c - 1
{-# INLINE lastPositionOf #-}
