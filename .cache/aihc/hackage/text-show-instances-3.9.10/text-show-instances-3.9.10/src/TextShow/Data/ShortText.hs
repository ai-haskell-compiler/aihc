{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Data.ShortText
Copyright:   (C) 2014-2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'ShortText' (from the @text-short@ package).

/Since: next/
-}
module TextShow.Data.ShortText () where

import           Data.Text.Short    (ShortText, toString)

import           TextShow           (TextShow (showb))
import           TextShow.Data.Char (showbString)

-- | /Since: 3.8/
instance TextShow ShortText where
    showb = showbString . toString
    {-# INLINE showb #-}
