{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TextBuilderDev.Domains.StrictBuilder where

#if MIN_VERSION_text(2,0,2) && !MIN_VERSION_text(2,1,2)

import Data.Text.Internal.StrictBuilder
import qualified TextBuilderCore as Core
import TextBuilderDev.Isomorphic
import TextBuilderDev.Prelude

instance Isomorphic StrictBuilder where
  {-# INLINE from #-}
  from (StrictBuilder size write) =
    Core.TextBuilder
      size
      ( \array offset ->
          write array offset $> offset + size
      )
  {-# INLINE to #-}
  to (Core.TextBuilder size write) =
    StrictBuilder
      size
      ( \array offset ->
          write array offset $> ()
      )

#endif
