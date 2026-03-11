{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Control.Applicative.Trans
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for applicative functor transformers.

/Since: 2/
-}
module TextShow.Control.Applicative.Trans () where

import Control.Applicative.Backwards (Backwards(..))
import Control.Applicative.Lift (Lift(..))

import TextShow (TextShow(..), TextShow1(..),
                 showbPrec1, showbUnaryWith)
import TextShow.Utils (liftShowbUnaryWith)

-- | /Since: 2/
instance (TextShow1 f, TextShow a) => TextShow (Backwards f a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 f => TextShow1 (Backwards f) where
    liftShowbPrec sp sl p (Backwards x) = liftShowbUnaryWith sp sl "Backwards" p x
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance (TextShow1 f, TextShow a) => TextShow (Lift f a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 f => TextShow1 (Lift f) where
    liftShowbPrec sp _  p (Pure  x) = showbUnaryWith sp                    "Pure"  p x
    liftShowbPrec sp sl p (Other y) = showbUnaryWith (liftShowbPrec sp sl) "Other" p y
    {-# INLINE liftShowbPrec #-}
