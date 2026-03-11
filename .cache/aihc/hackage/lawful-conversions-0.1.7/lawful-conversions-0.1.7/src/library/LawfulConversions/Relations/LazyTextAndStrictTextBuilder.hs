{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndStrictTextBuilder where

#if MIN_VERSION_text(2,1,2)

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.Text.Lazy.Text Data.Text.Encoding.StrictTextBuilder where
  to = Data.Text.Lazy.fromStrict . Data.Text.Encoding.strictBuilderToText

instance IsSome Data.Text.Encoding.StrictTextBuilder Data.Text.Lazy.Text where
  to = Data.Text.Encoding.textToStrictBuilder . Data.Text.Lazy.toStrict

instance IsMany Data.Text.Lazy.Text Data.Text.Encoding.StrictTextBuilder

instance IsMany Data.Text.Encoding.StrictTextBuilder Data.Text.Lazy.Text

instance Is Data.Text.Lazy.Text Data.Text.Encoding.StrictTextBuilder

instance Is Data.Text.Encoding.StrictTextBuilder Data.Text.Lazy.Text

#elif MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.Text.Lazy.Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Lazy.fromStrict . Data.Text.Encoding.strictBuilderToText

instance IsSome Data.Text.Encoding.StrictBuilder Data.Text.Lazy.Text where
  to = Data.Text.Encoding.textToStrictBuilder . Data.Text.Lazy.toStrict

instance IsMany Data.Text.Lazy.Text Data.Text.Encoding.StrictBuilder

instance IsMany Data.Text.Encoding.StrictBuilder Data.Text.Lazy.Text

instance Is Data.Text.Lazy.Text Data.Text.Encoding.StrictBuilder

instance Is Data.Text.Encoding.StrictBuilder Data.Text.Lazy.Text

#endif
