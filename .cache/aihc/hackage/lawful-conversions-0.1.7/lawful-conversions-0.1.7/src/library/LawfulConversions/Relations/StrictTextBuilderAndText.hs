{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StrictTextBuilderAndText where

#if MIN_VERSION_text(2,1,2)

import qualified Data.Text.Encoding
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Text Data.Text.Encoding.StrictTextBuilder where
  to = Data.Text.Encoding.strictBuilderToText

instance IsSome Data.Text.Encoding.StrictTextBuilder Text where
  to = Data.Text.Encoding.textToStrictBuilder

instance IsMany Text Data.Text.Encoding.StrictTextBuilder

instance IsMany Data.Text.Encoding.StrictTextBuilder Text

instance Is Text Data.Text.Encoding.StrictTextBuilder

instance Is Data.Text.Encoding.StrictTextBuilder Text

#elif MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Encoding.strictBuilderToText

instance IsSome Data.Text.Encoding.StrictBuilder Text where
  to = Data.Text.Encoding.textToStrictBuilder

instance IsMany Text Data.Text.Encoding.StrictBuilder

instance IsMany Data.Text.Encoding.StrictBuilder Text

instance Is Text Data.Text.Encoding.StrictBuilder

instance Is Data.Text.Encoding.StrictBuilder Text

#endif
