{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.IntAndWord where

import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Int Word where
  to = fromIntegral

instance IsSome Word Int where
  to = fromIntegral

instance IsMany Int Word

instance IsMany Word Int

instance Is Int Word

instance Is Word Int
