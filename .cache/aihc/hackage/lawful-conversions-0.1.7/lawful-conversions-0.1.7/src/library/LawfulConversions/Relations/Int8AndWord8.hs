{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Int8AndWord8 where

import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Int8 Word8 where
  to = fromIntegral

instance IsSome Word8 Int8 where
  to = fromIntegral

instance IsMany Int8 Word8

instance IsMany Word8 Int8

instance Is Int8 Word8

instance Is Word8 Int8
