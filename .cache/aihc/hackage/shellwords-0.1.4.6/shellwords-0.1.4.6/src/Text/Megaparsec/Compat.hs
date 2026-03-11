{-# LANGUAGE CPP #-}

module Text.Megaparsec.Compat
  ( module X
  , errorBundlePretty
  ) where

#if MIN_VERSION_megaparsec(7,0,0)
-- Import errorBundlePretty not as part of X, so it can be exported separately
-- as is necessary in the < 7.0.0 case
import Text.Megaparsec as X hiding (errorBundlePretty)
import Text.Megaparsec (errorBundlePretty)
#else
import Prelude

import Text.Megaparsec as X

errorBundlePretty :: Show a => a -> String
errorBundlePretty = show
#endif
