{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextBuilderAndString where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.StringAndText ()

instance IsSome String Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.unpack . Data.Text.Lazy.Builder.toLazyText
  maybeFrom = fmap Data.Text.Lazy.Builder.fromText . maybeFrom

instance IsMany String Data.Text.Lazy.Builder.Builder where
  from = fromString
