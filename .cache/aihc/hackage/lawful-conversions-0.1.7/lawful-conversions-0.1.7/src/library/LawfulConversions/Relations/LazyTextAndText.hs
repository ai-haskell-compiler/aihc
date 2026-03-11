{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndText where

import qualified Data.Text.Lazy
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.Text.Lazy.Text Text where
  to = Data.Text.Lazy.fromStrict

instance IsSome Text Data.Text.Lazy.Text where
  to = Data.Text.Lazy.toStrict

instance IsMany Data.Text.Lazy.Text Text

instance IsMany Text Data.Text.Lazy.Text

instance Is Data.Text.Lazy.Text Text

instance Is Text Data.Text.Lazy.Text
