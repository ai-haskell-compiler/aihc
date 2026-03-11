{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType         #-}
#endif

-- |
-- Module      : Numeric.LinearAlgebra.Static.Vector
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Conversions between statically sized types in
-- "Numeric.LinearAlgebra.Static" from /hmatrix/ and /vector-sized/.
--
-- This module is intentionally minimal, exporting only functions that
-- cannot be written without "unsafe" operations.  With these, however, you
-- can easily write other useful combinators by using type-safe operations
-- like 'fmap', 'VS.map', 'liftA2', 'Data.Vector.Generic.Sized.convert',
-- etc.
--

module Numeric.LinearAlgebra.Static.Vector (
  -- * Vector
  -- ** Real
    rVec
  , grVec
  , vecR
  , gvecR
  -- ** Complex
  , cVec
  , gcVec
  , vecC
  , gvecC
  -- * Matrix
  -- ** Real
  , lRows
  , rowsL
  , lCols
  , colsL
  , lVec
  , glVec
  , vecL
  , gvecL
  -- ** Complex
  , mRows
  , rowsM
  , mCols
  , colsM
  , mVec
  , gmVec
  , vecM
  , gvecM
  ) where

import           Data.Foldable
import           Data.Proxy
import           GHC.TypeLits
import           Unsafe.Coerce
import qualified Data.Vector                  as UV
import qualified Data.Vector.Generic          as UVG
import qualified Data.Vector.Generic.Sized    as VG
import qualified Data.Vector.Sized            as V
import qualified Data.Vector.Storable.Sized   as VS
import qualified Numeric.LinearAlgebra        as HU
import qualified Numeric.LinearAlgebra.Static as H

-- | Convert an /hmatrix/ vector (parameterized by its lenth) to
-- a /vector-sized/ storable vector of 'Double's.
--
-- This is normally /O(1)/, but will be /O(n)/ if the 'H.R' was contructed
-- with 'H.konst' or any other replicated-value constructor (like literals
-- and 'fromInteger'/'fromRational').
rVec :: KnownNat n => H.R n -> VS.Vector n H.ℝ
rVec = unsafeCoerce . H.extract

-- | 'rVec', but generalized to work for all types of sized vectors.
--
-- Usually /O(n)/, but if using this with storable vectors, should have the
-- same characteristics as 'rVec' due to rewrite rules.
--
-- @since 0.1.3.0
grVec :: (KnownNat n, UVG.Vector v H.ℝ) => H.R n -> VG.Vector v n H.ℝ
grVec = VG.convert . rVec
{-# NOINLINE[1] grVec #-}
{-# RULES "grVec" grVec = rVec #-}

-- | Convert a /vector-sized/ storable vector to an /hmatrix/ vector
-- (parameterized by its lenth).
--
-- /O(1)/
vecR :: VS.Vector n H.ℝ -> H.R n
vecR = unsafeCoerce

-- | 'vecR', but generalized to work for all types of sized vectors.
--
-- Usually /O(n)/, but if using this with storable vectors, should be /O(1)/
-- due to rewrite rules (but don't rely on this).
--
-- @since 0.1.3.0
gvecR :: UVG.Vector v H.ℝ => VG.Vector v n H.ℝ -> H.R n
gvecR = vecR . VG.convert
{-# NOINLINE[1] gvecR #-}
{-# RULES "gvecR" gvecR = vecR #-}

-- | Convert an /hmatrix/ complex vector (parameterized by its lenth) to
-- a /vector-sized/ storable vector of 'Complex Double's, preserving the
-- length in the type.
--
-- This is normally /O(1)/, but will be /O(n)/ if the 'H.C' was contructed
-- with 'H.konst' or any other replicated-value constructor (like literals
-- and 'fromInteger'/'fromRational').
cVec :: KnownNat n => H.C n -> VS.Vector n H.ℂ
cVec = unsafeCoerce . H.extract

-- | 'cVec', but generalized to work for all types of sized vectors.
--
-- Usually /O(n)/, but if using this with storable vectors, should have the
-- same characteristics as 'cVec' due to rewrite rules.
--
-- @since 0.1.3.0
gcVec :: (KnownNat n, UVG.Vector v H.ℂ) => H.C n -> VG.Vector v n H.ℂ
gcVec = VG.convert . cVec
{-# NOINLINE[1] gcVec #-}
{-# RULES "gcVec" gcVec = cVec #-}

-- | Convert a /vector-sized/ storable vector to an /hmatrix/ complex
-- vector (parameterized by its lenth), preserving the length in the type.
--
-- /O(1)/
vecC :: VS.Vector n H.ℂ -> H.C n
vecC = unsafeCoerce

-- | 'vecC', but generalized to work for all types of sized vectors.
--
-- Usually /O(n)/, but if using this with storable vectors, should be /O(1)/
-- due to rewrite rules (but don't rely on this).
--
-- @since 0.1.3.0
gvecC :: UVG.Vector v H.ℂ => VG.Vector v n H.ℂ -> H.C n
gvecC = vecC . VG.convert
{-# NOINLINE[1] gvecC #-}
{-# RULES "gvecC" gvecC = vecC #-}

-- | Split an /hmatrix/ matrix (parameterized by its dimensions) to
-- a /vector-sized/ boxed vector of its rows (as /hmatrix/ vectors).
--
-- This is normally /O(m*n)/, but can sometimes be /O(m)/ depending on the
-- representation of the 'H.L' being used.
lRows
    :: (KnownNat m, KnownNat n)
    => H.L m n
    -> V.Vector m (H.R n)
lRows = unsafeCoerce
      . UV.fromList
      . HU.toRows
      . H.extract

-- | Join together a /vector-sized/ boxed vector of /hmatrix/ vectors to an
-- /hmatrix/ matrix as its rows.
--
-- /O(m*n)/
rowsL
    :: forall m n. KnownNat n
    => V.Vector m (H.R n)
    -> H.L m n
rowsL = (unsafeCoerce :: HU.Matrix Double -> H.L m n)
      . HU.fromRows
      . map H.extract
      . toList

-- | Split an /hmatrix/ matrix (parameterized by its dimensions) to
-- a /vector-sized/ boxed vector of its columns (as /hmatrix/ vectors).
--
-- This is normally /O(m*n)/, but can sometimes be /O(n)/ depending on the
-- representation of the 'H.L' being used.
lCols
    :: forall m n. (KnownNat m, KnownNat n)
    => H.L m n
    -> V.Vector n (H.R m)
lCols = unsafeCoerce
      . UV.fromList
      . HU.toColumns
      . H.extract

-- | Join together a /vector-sized/ boxed vector of /hmatrix/ vectors to an
-- /hmatrix/ matrix as its columns.
--
-- /O(m*n)/
colsL
    :: forall m n. KnownNat m
    => V.Vector n (H.R m)
    -> H.L m n
colsL = (unsafeCoerce :: HU.Matrix Double -> H.L m n)
      . HU.fromColumns
      . map H.extract
      . toList

-- | Split an /hmatrix/ complex matrix (parameterized by its dimensions) to
-- a /vector-sized/ boxed vector of its rows (as /hmatrix/ complex
-- vectors).
--
-- This is normally /O(m*n)/, but can sometimes be /O(m)/ depending on the
-- representation of the 'H.C' being used.
mRows
    :: forall m n. (KnownNat m, KnownNat n)
    => H.M m n
    -> V.Vector m (H.C n)
mRows = unsafeCoerce
      . UV.fromList
      . HU.toRows
      . H.extract

-- | Join together a /vector-sized/ boxed vector of /hmatrix/ complex
-- vectors to an /hmatrix/ complex matrix as its rows.
--
-- /O(m*n)/
rowsM
    :: forall m n. KnownNat n
    => V.Vector m (H.C n)
    -> H.M m n
rowsM = (unsafeCoerce :: HU.Matrix H.ℂ -> H.M m n)
      . HU.fromRows
      . map H.extract
      . toList

-- | Split an /hmatrix/ complex matrix (parameterized by its dimensions) to
-- a /vector-sized/ boxed vector of its columns (as /hmatrix/ complex
-- vectors).
--
-- This is normally /O(m*n)/, but can sometimes be /O(n)/ depending on the
-- representation of the 'H.C' being used.
mCols
    :: forall m n. (KnownNat m, KnownNat n)
    => H.M m n
    -> V.Vector n (H.C m)
mCols = unsafeCoerce
      . UV.fromList
      . HU.toColumns
      . H.extract

-- | Join together a /vector-sized/ boxed vector of /hmatrix/ complex
-- vectors to an /hmatrix/ complex matrix as its columns.
--
-- /O(m*n)/
colsM
    :: forall m n. KnownNat m
    => V.Vector n (H.C m)
    -> H.M m n
colsM = (unsafeCoerce :: HU.Matrix H.ℂ -> H.M m n)
      . HU.fromColumns
      . map H.extract
      . toList

-- | Shape a /vector-sized/ storable vector of elements into an /hmatrix/
-- matrix.
--
-- /O(1)/
--
-- @since 0.1.1.0
vecL
    :: forall m n. KnownNat n
    => VS.Vector (m * n) H.ℝ
    -> H.L m n
vecL = (unsafeCoerce :: HU.Matrix H.ℝ -> H.L m n)
     . HU.reshape (fromIntegral (natVal (Proxy @n)))
     . unsafeCoerce

-- | 'vecL', but generalized to work for all types of sized vectors.
--
-- Usually /O(n)/, but if using this with storable vectors, should be /O(1)/
-- due to rewrite rules (but don't rely on this).
--
-- @since 0.1.3.0
gvecL
    :: (KnownNat n, UVG.Vector v H.ℝ)
    => VG.Vector v (m * n) H.ℝ
    -> H.L m n
gvecL = vecL . VG.convert
{-# NOINLINE[1] gvecL #-}
{-# RULES "gvecL" gvecL = vecL #-}

-- | Flatten an /hmatrix/ matrix into a /vector-sized/ storable vector of
-- its items.
--
-- This is normally /O(m*n)/, but can sometimes be /O(1)/ depending on the
-- representation of the 'H.L' being used.
--
-- @since 0.1.1.0
lVec
    :: forall m n. (KnownNat m, KnownNat n)
    => H.L m n
    -> VS.Vector (m * n) H.ℝ
lVec = unsafeCoerce
     . HU.flatten
     . H.extract

-- | 'lVec', but generalized to work for all types of sized vectors.
--
-- Usually /O(m*n)/, but if using this with storable vectors, should have the
-- same characteristics as 'lVec' due to rewrite rules.
--
-- @since 0.1.3.0
glVec
    :: (KnownNat m, KnownNat n, UVG.Vector v H.ℝ)
    => H.L m n
    -> VG.Vector v (m * n) H.ℝ
glVec = VG.convert . lVec
{-# NOINLINE[1] glVec #-}
{-# RULES "glVec" glVec = lVec #-}

-- | Shape a /vector-sized/ storable vector of elements into an /hmatrix/
-- complex matrix.
--
-- /O(1)/
--
-- @since 0.1.1.0
vecM
    :: forall m n. KnownNat n
    => VS.Vector (m * n) H.ℂ
    -> H.M m n
vecM = (unsafeCoerce :: HU.Matrix H.ℂ -> H.M m n)
     . HU.reshape (fromIntegral (natVal (Proxy @n)))
     . unsafeCoerce

-- | 'vecM', but generalized to work for all types of sized vectors.
--
-- Usually /O(m*n)/, but if using this with storable vectors, should be /O(1)/
-- due to rewrite rules (but don't rely on this).
--
-- @since 0.1.3.0
gvecM
    :: (KnownNat n, UVG.Vector v H.ℂ)
    => VG.Vector v (m * n) H.ℂ
    -> H.M m n
gvecM = vecM . VG.convert
{-# NOINLINE[1] gvecM #-}
{-# RULES "gvecM" gvecM = vecM #-}

-- | Flatten an /hmatrix/ complex matrix into a /vector-sized/ storable
-- vector of its items.
--
-- This is normally /O(m*n)/, but can sometimes be /O(1)/ depending on the
-- representation of the 'H.M' being used.
--
-- @since 0.1.1.0
mVec
    :: forall m n. (KnownNat m, KnownNat n)
    => H.M m n
    -> VS.Vector (m * n) H.ℂ
mVec = unsafeCoerce
     . HU.flatten
     . H.extract

-- | 'mVec', but generalized to work for all types of sized vectors.
--
-- Usually /O(m*n)/, but if using this with storable vectors, should have the
-- same characteristics as 'mVec' due to rewrite rules.
--
-- @since 0.1.3.0
gmVec
    :: (KnownNat m, KnownNat n, UVG.Vector v H.ℂ)
    => H.M m n
    -> VG.Vector v (m * n) H.ℂ
gmVec = VG.convert . mVec
{-# NOINLINE[1] gmVec #-}
{-# RULES "gmVec" gmVec = mVec #-}
