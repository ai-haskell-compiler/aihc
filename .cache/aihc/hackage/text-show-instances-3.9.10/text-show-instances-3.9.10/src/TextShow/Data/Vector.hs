{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Data.Vector
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for @Vector@ types.

/Since: 2/
-}
module TextShow.Data.Vector () where

import qualified Data.Vector as B (Vector)
import           Data.Vector.Fusion.Bundle.Size (Size)
import qualified Data.Vector.Generic as G (Vector)
import           Data.Vector.Generic (toList)
import qualified Data.Vector.Primitive as P (Vector)
import           Data.Vector.Primitive (Prim)
import qualified Data.Vector.Storable as S (Vector)
import qualified Data.Vector.Unboxed as U (Vector)
import           Data.Vector.Unboxed (Unbox)

import           Foreign.Storable (Storable)

import           TextShow (TextShow(..), TextShow1(..), Builder)
import           TextShow.TH (deriveTextShow)

-- | Convert a generic 'G.Vector' to a 'Builder'.
showbVectorGenericPrec :: (G.Vector v a, TextShow a) => v a -> Builder
showbVectorGenericPrec = showbList . toList
{-# INLINE showbVectorGenericPrec #-}

-- | Convert a generic 'G.Vector' to a 'Builder' with the given show function.
liftShowbVectorGenericPrec :: G.Vector v a => ([a] -> Builder) -> v a -> Builder
liftShowbVectorGenericPrec sl = sl . toList
{-# INLINE liftShowbVectorGenericPrec #-}

-- | /Since: 2/
instance TextShow a => TextShow (B.Vector a) where
    showbPrec _ = showbVectorGenericPrec
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 B.Vector where
    liftShowbPrec _ sl _ = liftShowbVectorGenericPrec sl
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance (TextShow a, Prim a) => TextShow (P.Vector a) where
    showbPrec _ = showbVectorGenericPrec
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance (TextShow a, Storable a) => TextShow (S.Vector a) where
    showbPrec _ = showbVectorGenericPrec
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance (TextShow a, Unbox a) => TextShow (U.Vector a) where
    showbPrec _ = showbVectorGenericPrec
    {-# INLINE showbPrec #-}

-- | /Since: 2/
$(deriveTextShow ''Size)
