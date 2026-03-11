{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Data.Functor.Trans
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for functor transformers.

Note that :

* An instance for 'Identity' is found in @text-show@, since it is a part of
  @base-4.8.0.0@ and later.

* Instances for 'Compose', 'Product', and 'Sum' are found in @text-show@, since they
  are part of @base-4.9.0.0@ and later.

/Since: 2/
-}
module TextShow.Data.Functor.Trans () where

import Data.Functor.Constant (Constant(..))
import Data.Functor.Reverse  (Reverse(..))

import TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                 Builder, showbPrec1, showbUnaryWith)
import TextShow.Utils (liftShowbUnaryWith)

-- | Convert a 'Constant' value to a 'Builder' with the given show function
-- and precedence.
liftShowbConstantPrec :: (Int -> a -> Builder) -> Int -> Constant a b -> Builder
liftShowbConstantPrec sp p (Constant x) = showbUnaryWith sp "Constant" p x
{-# INLINE liftShowbConstantPrec #-}

-- | /Since: 2/
instance TextShow a => TextShow (Constant a b) where
    showbPrec = liftShowbConstantPrec showbPrec
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow a => TextShow1 (Constant a) where
    liftShowbPrec _ _ = liftShowbConstantPrec showbPrec
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance TextShow2 Constant where
    liftShowbPrec2 sp _ _ _ = liftShowbConstantPrec sp
    {-# INLINE liftShowbPrec2 #-}

-- | /Since: 2/
instance (TextShow1 f, TextShow a) => TextShow (Reverse f a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 f => TextShow1 (Reverse f) where
    liftShowbPrec sp sl p (Reverse x) = liftShowbUnaryWith sp sl "Reverse" p x
    {-# INLINE liftShowbPrec #-}
