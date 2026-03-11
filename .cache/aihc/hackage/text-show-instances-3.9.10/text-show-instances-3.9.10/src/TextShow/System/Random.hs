{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.System.Random
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'StdGen'.

/Since: 2/
-}
module TextShow.System.Random () where

import System.Random (StdGen)
import TextShow (TextShow(..), FromStringShow(..))

-- | /Since: 2/
instance TextShow StdGen where
    showbPrec p = showbPrec p . FromStringShow
    {-# INLINE showbPrec #-}
