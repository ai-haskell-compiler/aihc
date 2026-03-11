{-# LANGUAGE ScopedTypeVariables #-}

module Data.BCP47.Roundtrip
  ( roundtrips
  ) where

roundtrips
  :: forall a b f
   . (Eq (f a), Applicative f)
  => (a -> b)
  -- ^ Encode
  -> (b -> f a)
  -- ^ Decode
  -> a
  -> Bool
roundtrips encode decode x = decode (encode x) == pure x
