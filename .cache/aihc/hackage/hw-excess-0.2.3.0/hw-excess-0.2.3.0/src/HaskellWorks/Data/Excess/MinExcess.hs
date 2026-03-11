{-# OPTIONS_GHC-funbox-strict-fields #-}

module HaskellWorks.Data.Excess.MinExcess
  ( MinExcess(..)
  ) where

import Control.Applicative (pure)
import Foreign.Ptr         (castPtr)
import Foreign.Storable    (Storable (..))

data MinExcess = MinExcess
  { minExcessMin :: !Int
  , minExcessAll :: !Int
  } deriving (Eq, Show)

instance Storable MinExcess where
  sizeOf _    = sizeOf (0 :: Int) * 3
  alignment _ = alignment (0 :: Int)
  peek p      = do let q = castPtr p
                   a <- peek q
                   b <- peekElemOff q 1
                   pure (MinExcess a b)
  poke p t    = do let q = castPtr p
                   poke q (minExcessMin t)
                   pokeElemOff q 1 (minExcessAll t)
