{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Int16AndWord16 where

import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Int16 Word16 where
  to = fromIntegral

instance IsSome Word16 Int16 where
  to = fromIntegral

instance IsMany Int16 Word16

instance IsMany Word16 Int16

instance Is Int16 Word16

instance Is Word16 Int16
