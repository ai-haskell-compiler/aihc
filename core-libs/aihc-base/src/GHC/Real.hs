{-# LANGUAGE MagicHash #-}

module GHC.Real
  ( Integral (..),
    fromIntegral,
  )
where

import GHC.Int (Int (..))
import GHC.Internal.Integer (Integer (..))
import GHC.Num (Num (..))

class (Num a) => Integral a where
  toInteger :: a -> Integer

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral value = fromInteger (toInteger value)

instance Integral Int where
  toInteger (I# value) = IS value

instance Integral Integer where
  toInteger value = value
