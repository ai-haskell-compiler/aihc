{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Serialise () where

import Codec.Serialise
import Data.BCP47 (BCP47, fromText, toText)
import Data.Text (unpack)

instance Serialise BCP47 where
  -- bypass children not having their own Serialise instances by using toText/fromText
  encode = encode . toText
  decode = either (fail . unpack) pure . fromText =<< decode
