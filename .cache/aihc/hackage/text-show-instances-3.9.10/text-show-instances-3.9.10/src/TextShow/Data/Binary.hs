{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      TextShow.Data.Binary
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Decoder'.

/Since: 2/
-}
module TextShow.Data.Binary () where

import Data.Binary.Get.Internal (Decoder(..))

import Prelude ()
import Prelude.Compat

import TextShow (TextShow(..), TextShow1(..), Builder, fromString, showbPrec1)

-- | /Since: 2/
instance TextShow a => TextShow (Decoder a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 Decoder where
    liftShowbPrec sp' _ _ = go $ sp' 0
      where
        go :: (a -> Builder) -> Decoder a -> Builder
        go _  (Fail _ msg)    = "Fail: " <> fromString msg
        go _  (Partial _)     = "Partial _"
        go sp (Done _ a)      = "Done: " <> sp a
        go _  (BytesRead _ _) = "BytesRead"
    {-# INLINE liftShowbPrec #-}
