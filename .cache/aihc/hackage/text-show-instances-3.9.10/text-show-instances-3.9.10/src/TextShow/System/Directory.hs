{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.System.Directory
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Permissions'.

/Since: 2/
-}
module TextShow.System.Directory () where

import System.Directory (Permissions, XdgDirectory)

import TextShow.TH (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''Permissions)

-- | /Since: 3.6/
$(deriveTextShow ''XdgDirectory)
