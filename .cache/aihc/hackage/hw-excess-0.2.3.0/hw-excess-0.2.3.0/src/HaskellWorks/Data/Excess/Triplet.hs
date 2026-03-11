{-# OPTIONS_GHC-funbox-strict-fields #-}

module HaskellWorks.Data.Excess.Triplet
  ( Triplet(..)
  ) where

import Control.Applicative (pure)
import Foreign.Ptr         (castPtr)
import Foreign.Storable    (Storable (..))

data Triplet = Triplet
  { tripletMinExcess :: !Int
  , tripletAllExcess :: !Int
  , tripletMaxExcess :: !Int
  } deriving (Eq, Show)

instance Storable Triplet where
  sizeOf _    = sizeOf (0 :: Int) * 3
  alignment _ = alignment (0 :: Int)
  peek p      = do let q = castPtr p
                   a <- peek q
                   b <- peekElemOff q 1
                   c <- peekElemOff q 2
                   pure (Triplet a b c)
  poke p t    = do let q = castPtr p
                   poke q (tripletMinExcess t)
                   pokeElemOff q 1 (tripletAllExcess t)
                   pokeElemOff q 2 (tripletMaxExcess t)

