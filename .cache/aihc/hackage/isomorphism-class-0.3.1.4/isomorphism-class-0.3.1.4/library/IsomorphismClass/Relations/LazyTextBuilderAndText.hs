{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextBuilderAndText where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo Data.Text.Lazy.Builder.Builder Text where
  to = Data.Text.Lazy.Builder.fromText

instance IsomorphicTo Text Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText
