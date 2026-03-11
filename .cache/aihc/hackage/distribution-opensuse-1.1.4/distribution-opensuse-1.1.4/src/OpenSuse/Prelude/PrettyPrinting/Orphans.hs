{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenSuse.Prelude.PrettyPrinting.Orphans ( ) where

import Numeric.Natural
import Text.PrettyPrint.HughesPJClass

instance Pretty Natural where
  pPrint = text . show
