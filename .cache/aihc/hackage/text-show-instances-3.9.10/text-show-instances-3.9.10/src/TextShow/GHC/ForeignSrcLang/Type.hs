{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      TextShow.GHC.ForeignSrcLang.Type
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for the 'ForeignSrcLang' data type.

/Since: 3.3/
-}
module TextShow.GHC.ForeignSrcLang.Type () where

import GHC.ForeignSrcLang.Type (ForeignSrcLang)
import TextShow.TH (deriveTextShow)

-- | /Since: 3.6/
$(deriveTextShow ''ForeignSrcLang)
