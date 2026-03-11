{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.System.Locale
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for the old 'TimeLocale'.

/Since: 2/
-}
module TextShow.System.Locale () where

import System.Locale (TimeLocale)
import TextShow.TH (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''TimeLocale)
