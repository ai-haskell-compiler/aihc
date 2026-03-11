{-# OPTIONS_HADDOCK hide #-}
module Data.PerfectHash.Types.Nonces where

import           Data.Default             (Default, def)


-- * Types

newtype Nonce = Nonce Int
  deriving Show

instance Default Nonce where
  def = Nonce 0



-- * Helper functions

mapNonce :: (Int -> Int) -> Nonce -> Nonce
mapNonce f (Nonce x) = Nonce $ f x


isDirectSlot :: Int -> Bool
isDirectSlot val = val < 0