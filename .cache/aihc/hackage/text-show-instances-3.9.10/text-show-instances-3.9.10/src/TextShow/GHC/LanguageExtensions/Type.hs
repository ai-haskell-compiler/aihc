{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      TextShow.GHC.LanguageExtensions.Type
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for the 'Extension' data type.

/Since: 3.3/
-}
module TextShow.GHC.LanguageExtensions.Type () where

import GHC.LanguageExtensions.Type (Extension)
import TextShow.TH (deriveTextShow)

-- | /Since: 3.3/
$(deriveTextShow ''Extension)
