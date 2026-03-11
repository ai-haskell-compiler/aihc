{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndString where

import qualified Data.Text.Lazy
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.StringAndText ()

instance IsSome String Data.Text.Lazy.Text where
  to = Data.Text.Lazy.unpack
  maybeFrom = fmap Data.Text.Lazy.fromStrict . maybeFrom

instance IsMany String Data.Text.Lazy.Text where
  from = fromString
