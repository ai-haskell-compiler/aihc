{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StrictTextBuilderAndString where

#if MIN_VERSION_text(2,1,2)

import qualified Data.Text.Encoding
import LawfulConversions.Classes
import LawfulConversions.Relations.StringAndText ()
import LawfulConversions.Prelude

instance IsSome String Data.Text.Encoding.StrictTextBuilder where
  to = to . Data.Text.Encoding.strictBuilderToText
  maybeFrom = fmap Data.Text.Encoding.textToStrictBuilder . maybeFrom

instance IsMany String Data.Text.Encoding.StrictTextBuilder where
  from =  Data.Text.Encoding.textToStrictBuilder . from

#elif MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import LawfulConversions.Classes
import LawfulConversions.Relations.StringAndText ()
import LawfulConversions.Prelude

instance IsSome String Data.Text.Encoding.StrictBuilder where
  to = to . Data.Text.Encoding.strictBuilderToText
  maybeFrom = fmap Data.Text.Encoding.textToStrictBuilder . maybeFrom

instance IsMany String Data.Text.Encoding.StrictBuilder where
  from =  Data.Text.Encoding.textToStrictBuilder . from

#endif
