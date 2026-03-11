{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TextBuilderDev.Domains.StrictTextBuilder where

#if MIN_VERSION_text(2,1,2)

import Data.Text.Internal.StrictBuilder
import qualified TextBuilderCore as Core
import TextBuilderDev.Isomorphic
import TextBuilderDev.Prelude

instance Isomorphic StrictTextBuilder where
  {-# INLINE from #-}
  from (StrictTextBuilder size write) =
    Core.TextBuilder
      size
      ( \array offset ->
          write array offset $> offset + size
      )
  {-# INLINE to #-}
  to (Core.TextBuilder size write) =
    StrictTextBuilder
      size
      ( \array offset ->
          write array offset $> ()
      )

#endif
