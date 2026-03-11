-- This file is auto-generated.  Do not edit directly.
-- |
--   Stability: Experimental
--
--   Generic interface to Blas using unsafe foreign calls.  Refer to the GHC
--   documentation for more information regarding appropriate use of safe and unsafe
--   foreign calls.
--
--   The functions here are named in a similar fashion to the original Blas interface, with
--   the type-dependent letter(s) removed.  Some functions have been merged with
--   others to allow the interface to work on both real and complex numbers.  If you can't a
--   particular function, try looking for its corresponding complex equivalent (e.g.
--   @symv@ is a special case of 'hemv' applied to real numbers).
--
--   Note: although complex versions of 'rot' and 'rotg' exist in many implementations,
--   they are not part of the official Blas standard and therefore not included here.  If
--   you /really/ need them, submit a ticket so we can try to come up with a solution.
--
--   The documentation here is still incomplete.  Consult the
--   <http://netlib.org/blas/#_blas_routines official documentation> for more
--   information.
--
--   Notation:
--
--     * @⋅@ denotes dot product (without any conjugation).
--     * @*@ denotes complex conjugation.
--     * @⊤@ denotes transpose.
--     * @†@ denotes conjugate transpose (Hermitian conjugate).
--
--   Conventions:
--
--     * All scalars are denoted with lowercase Greek letters
--     * All vectors are denoted with lowercase Latin letters and are
--       assumed to be column vectors (unless transposed).
--     * All matrices are denoted with uppercase Latin letters.
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Blas.Generic.Unsafe (
    Numeric(..)
  , RealNumeric(..)
  , D.dsdot
  , S.sdsdot
  ) where
import Data.Complex (Complex)
import Foreign (Ptr, Storable)
import Blas.Primitive.Types (Order, Transpose, Uplo, Diag, Side)
import qualified Blas.Specialized.Float.Unsafe as S
import qualified Blas.Specialized.Double.Unsafe as D
import qualified Blas.Specialized.ComplexFloat.Unsafe as C
import qualified Blas.Specialized.ComplexDouble.Unsafe as Z

-- | Blas operations that are applicable to real and complex numbers.
--
--   Instances are defined for the 4 types supported by Blas: the single- and
--   double-precision floating point types and their complex versions.
class (Floating a, Storable a) => Numeric a where

  -- | The corresponding real type of @a@.
  --
  --   In other words, @'RealType' ('Complex' a)@ is an alias for @a@.  For everything
  --   else, @'RealType' a@ is simply @a@.
  type RealType a :: *
  -- | Swap two vectors:
  --
  --   > (x, y) ← (y, x)
  swap :: Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Multiply a vector by a scalar.
  --
  --   > x ← α x
  scal :: Int
       -> a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Copy a vector into another vector:
  --
  --   > y ← x
  copy :: Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Add a scalar-vector product to a vector.
  --
  --   > y ← α x + y
  axpy :: Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Calculate the bilinear dot product of two vectors:
  --
  --   > x ⋅ y ≡ ∑[i] x[i] y[i]
  dotu :: Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO a

  -- | Calculate the sesquilinear dot product of two vectors.
  --
  --   > x* ⋅ y ≡ ∑[i] x[i]* y[i]
  dotc :: Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO a

  -- | Calculate the Euclidean (L²) norm of a vector:
  --
  --   > ‖x‖₂ ≡ √(∑[i] x[i]²)
  nrm2 :: Int
       -> Ptr a
       -> Int
       -> IO (RealType a)

  -- | Calculate the Manhattan (L¹) norm, equal to the sum of the magnitudes of the elements:
  --
  --   > ‖x‖₁ = ∑[i] |x[i]|
  asum :: Int
       -> Ptr a
       -> Int
       -> IO (RealType a)

  -- | Calculate the index of the element with the maximum magnitude (absolute value).
  iamax :: Int
        -> Ptr a
        -> Int
        -> IO Int

  -- | Perform a general matrix-vector update.
  --
  --   > y ← α T(A) x + β y
  gemv :: Order
       -> Transpose
       -> Int
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform a general banded matrix-vector update.
  --
  --   > y ← α T(A) x + β y
  gbmv :: Order
       -> Transpose
       -> Int
       -> Int
       -> Int
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform a hermitian matrix-vector update.
  --
  --   > y ← α A x + β y
  hemv :: Order
       -> Uplo
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform a hermitian banded matrix-vector update.
  --
  --   > y ← α A x + β y
  hbmv :: Order
       -> Uplo
       -> Int
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform a hermitian packed matrix-vector update.
  --
  --   > y ← α A x + β y
  hpmv :: Order
       -> Uplo
       -> Int
       -> a
       -> Ptr a
       -> Ptr a
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Multiply a triangular matrix by a vector.
  --
  --   > x ← T(A) x
  trmv :: Order
       -> Uplo
       -> Transpose
       -> Diag
       -> Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Multiply a triangular banded matrix by a vector.
  --
  --   > x ← T(A) x
  tbmv :: Order
       -> Uplo
       -> Transpose
       -> Diag
       -> Int
       -> Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Multiply a triangular packed matrix by a vector.
  --
  --   > x ← T(A) x
  tpmv :: Order
       -> Uplo
       -> Transpose
       -> Diag
       -> Int
       -> Ptr a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Multiply an inverse triangular matrix by a vector.
  --
  --   > x ← T(A⁻¹) x
  trsv :: Order
       -> Uplo
       -> Transpose
       -> Diag
       -> Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Multiply an inverse triangular banded matrix by a vector.
  --
  --   > x ← T(A⁻¹) x
  tbsv :: Order
       -> Uplo
       -> Transpose
       -> Diag
       -> Int
       -> Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Multiply an inverse triangular packed matrix by a vector.
  --
  --   > x ← T(A⁻¹) x
  tpsv :: Order
       -> Uplo
       -> Transpose
       -> Diag
       -> Int
       -> Ptr a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform an unconjugated rank-1 update of a general matrix.
  --
  --   > A ← α x y⊤ + A
  geru :: Order
       -> Int
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform a conjugated rank-1 update of a general matrix.
  --
  --   > A ← α x y† + A
  gerc :: Order
       -> Int
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform a rank-1 update of a Hermitian matrix.
  --
  --   > A ← α x y† + A
  her :: Order
      -> Uplo
      -> Int
      -> RealType a
      -> Ptr a
      -> Int
      -> Ptr a
      -> Int
      -> IO ()

  -- | Perform a rank-1 update of a Hermitian packed matrix.
  --
  --   > A ← α x y† + A
  hpr :: Order
      -> Uplo
      -> Int
      -> RealType a
      -> Ptr a
      -> Int
      -> Ptr a
      -> IO ()

  -- | Perform a rank-2 update of a Hermitian matrix.
  --
  --   > A ← α x y† + y (α x)† + A
  her2 :: Order
       -> Uplo
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform a rank-2 update of a Hermitian packed matrix.
  --
  --   > A ← α x y† + y (α x)† + A
  hpr2 :: Order
       -> Uplo
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> IO ()

  -- | Perform a general matrix-matrix update.
  --
  --   > C ← α T(A) U(B) + β C
  gemm :: Order -- ^ Layout of all the matrices.
       -> Transpose -- ^ The operation @T@ to be applied to @A@.
       -> Transpose -- ^ The operation @U@ to be applied to @B@.
       -> Int -- ^ Number of rows of @T(A)@ and @C@.
       -> Int -- ^ Number of columns of @U(B)@ and @C@.
       -> Int -- ^ Number of columns of @T(A)@ and number of rows of @U(B)@.
       -> a -- ^ Scaling factor @α@ of the product.
       -> Ptr a -- ^ Pointer to a matrix @A@.
       -> Int -- ^ Stride of the major dimension of @A@.
       -> Ptr a -- ^ Pointer to a matrix @B@.
       -> Int -- ^ Stride of the major dimension of @B@.
       -> a -- ^ Scaling factor @β@ of the original @C@.
       -> Ptr a -- ^ Pointer to a mutable matrix @C@.
       -> Int -- ^ Stride of the major dimension of @C@.
       -> IO ()

  -- | Perform a symmetric matrix-matrix update.
  --
  --   > C ← α A B + β C    or    C ← α B A + β C
  --
  --   where @A@ is symmetric.  The matrix @A@ must be in an unpacked format, although the
  --   routine will only access half of it as specified by the @'Uplo'@ argument.
  symm :: Order -- ^ Layout of all the matrices.
       -> Side -- ^ Side that @A@ appears in the product.
       -> Uplo -- ^ The part of @A@ that is used.
       -> Int -- ^ Number of rows of @C@.
       -> Int -- ^ Number of columns of @C@.
       -> a -- ^ Scaling factor @α@ of the product.
       -> Ptr a -- ^ Pointer to a symmetric matrix @A@.
       -> Int -- ^ Stride of the major dimension of @A@.
       -> Ptr a -- ^ Pointer to a matrix @B@.
       -> Int -- ^ Stride of the major dimension of @B@.
       -> a -- ^ Scaling factor @α@ of the original @C@.
       -> Ptr a -- ^ Pointer to a mutable matrix @C@.
       -> Int -- ^ Stride of the major dimension of @C@.
       -> IO ()

  -- | Perform a Hermitian matrix-matrix update.
  --
  --   > C ← α A B + β C    or    C ← α B A + β C
  --
  --   where @A@ is Hermitian.  The matrix @A@ must be in an unpacked format, although the
  --   routine will only access half of it as specified by the @'Uplo'@ argument.
  hemm :: Order -- ^ Layout of all the matrices.
       -> Side -- ^ Side that @A@ appears in the product.
       -> Uplo -- ^ The part of @A@ that is used.
       -> Int -- ^ Number of rows of @C@.
       -> Int -- ^ Number of columns of @C@.
       -> a -- ^ Scaling factor @α@ of the product.
       -> Ptr a -- ^ Pointer to a Hermitian matrix @A@.
       -> Int -- ^ Stride of the major dimension of @A@.
       -> Ptr a -- ^ Pointer to a matrix @B@.
       -> Int -- ^ Stride of the major dimension of @B@.
       -> a -- ^ Scaling factor @α@ of the original @C@.
       -> Ptr a -- ^ Pointer to a mutable matrix @C@.
       -> Int -- ^ Stride of the major dimension of @C@.
       -> IO ()

  -- | Perform a symmetric rank-k update.
  --
  --   > C ← α A A⊤ + β C    or    C ← α A⊤ A + β C
  syrk :: Order
       -> Uplo
       -> Transpose
       -> Int
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform a Hermitian rank-k update.
  --
  --   > C ← α A A† + β C    or    C ← α A† A + β C
  herk :: Order
       -> Uplo
       -> Transpose
       -> Int
       -> Int
       -> RealType a
       -> Ptr a
       -> Int
       -> RealType a
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform a symmetric rank-2k update.
  --
  --   > C ← α A B⊤ + α* B A⊤ + β C    or    C ← α A⊤ B + α* B⊤ A + β C
  syr2k :: Order
        -> Uplo
        -> Transpose
        -> Int
        -> Int
        -> a
        -> Ptr a
        -> Int
        -> Ptr a
        -> Int
        -> a
        -> Ptr a
        -> Int
        -> IO ()

  -- | Perform a Hermitian rank-2k update.
  --
  --   > C ← α A B† + α* B A† + β C    or    C ← α A† B + α* B† A + β C
  her2k :: Order
        -> Uplo
        -> Transpose
        -> Int
        -> Int
        -> a
        -> Ptr a
        -> Int
        -> Ptr a
        -> Int
        -> RealType a
        -> Ptr a
        -> Int
        -> IO ()

  -- | Perform a triangular matrix-matrix multiplication.
  --
  --   > B ← α T(A) B    or    B ← α B T(A)
  --
  --   where @A@ is triangular.
  trmm :: Order
       -> Side
       -> Uplo
       -> Transpose
       -> Diag
       -> Int
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

  -- | Perform an inverse triangular matrix-matrix multiplication.
  --
  --   > B ← α T(A⁻¹) B    or    B ← α B T(A⁻¹)
  --
  --   where @A@ is triangular.
  trsm :: Order
       -> Side
       -> Uplo
       -> Transpose
       -> Diag
       -> Int
       -> Int
       -> a
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> IO ()

-- | Blas operations that are only applicable to real numbers.
class Numeric a => RealNumeric a where

  -- | Generate a Givens rotation. (Only available for real floating-point types.)
  rotg :: Ptr a
       -> Ptr a
       -> Ptr a
       -> Ptr a
       -> IO ()

  -- | Generate a modified Givens rotation. (Only available for real floating-point
  --   types.)
  rotmg :: Ptr a
        -> Ptr a
        -> Ptr a
        -> a
        -> Ptr a
        -> IO ()

  -- | Apply a Givens rotation. (Only available for real floating-point types.)
  rot :: Int
      -> Ptr a
      -> Int
      -> Ptr a
      -> Int
      -> a
      -> a
      -> IO ()

  -- | Apply a modified Givens rotation. (Only available for real floating-point types.)
  rotm :: Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> Int
       -> Ptr a
       -> IO ()

instance Numeric Float where
  type RealType Float = Float
  swap = S.swap
  scal = S.scal
  copy = S.copy
  axpy = S.axpy
  dotu = S.dotu
  dotc = S.dotc
  nrm2 = S.nrm2
  asum = S.asum
  iamax = S.iamax
  gemv = S.gemv
  gbmv = S.gbmv
  hemv = S.hemv
  hbmv = S.hbmv
  hpmv = S.hpmv
  trmv = S.trmv
  tbmv = S.tbmv
  tpmv = S.tpmv
  trsv = S.trsv
  tbsv = S.tbsv
  tpsv = S.tpsv
  geru = S.geru
  gerc = S.gerc
  her = S.her
  hpr = S.hpr
  her2 = S.her2
  hpr2 = S.hpr2
  gemm = S.gemm
  symm = S.symm
  hemm = S.hemm
  syrk = S.syrk
  herk = S.herk
  syr2k = S.syr2k
  her2k = S.her2k
  trmm = S.trmm
  trsm = S.trsm


instance Numeric Double where
  type RealType Double = Double
  swap = D.swap
  scal = D.scal
  copy = D.copy
  axpy = D.axpy
  dotu = D.dotu
  dotc = D.dotc
  nrm2 = D.nrm2
  asum = D.asum
  iamax = D.iamax
  gemv = D.gemv
  gbmv = D.gbmv
  hemv = D.hemv
  hbmv = D.hbmv
  hpmv = D.hpmv
  trmv = D.trmv
  tbmv = D.tbmv
  tpmv = D.tpmv
  trsv = D.trsv
  tbsv = D.tbsv
  tpsv = D.tpsv
  geru = D.geru
  gerc = D.gerc
  her = D.her
  hpr = D.hpr
  her2 = D.her2
  hpr2 = D.hpr2
  gemm = D.gemm
  symm = D.symm
  hemm = D.hemm
  syrk = D.syrk
  herk = D.herk
  syr2k = D.syr2k
  her2k = D.her2k
  trmm = D.trmm
  trsm = D.trsm


instance Numeric (Complex Float) where
  type RealType (Complex Float) = Float
  swap = C.swap
  scal = C.scal
  copy = C.copy
  axpy = C.axpy
  dotu = C.dotu
  dotc = C.dotc
  nrm2 = C.nrm2
  asum = C.asum
  iamax = C.iamax
  gemv = C.gemv
  gbmv = C.gbmv
  hemv = C.hemv
  hbmv = C.hbmv
  hpmv = C.hpmv
  trmv = C.trmv
  tbmv = C.tbmv
  tpmv = C.tpmv
  trsv = C.trsv
  tbsv = C.tbsv
  tpsv = C.tpsv
  geru = C.geru
  gerc = C.gerc
  her = C.her
  hpr = C.hpr
  her2 = C.her2
  hpr2 = C.hpr2
  gemm = C.gemm
  symm = C.symm
  hemm = C.hemm
  syrk = C.syrk
  herk = C.herk
  syr2k = C.syr2k
  her2k = C.her2k
  trmm = C.trmm
  trsm = C.trsm


instance Numeric (Complex Double) where
  type RealType (Complex Double) = Double
  swap = Z.swap
  scal = Z.scal
  copy = Z.copy
  axpy = Z.axpy
  dotu = Z.dotu
  dotc = Z.dotc
  nrm2 = Z.nrm2
  asum = Z.asum
  iamax = Z.iamax
  gemv = Z.gemv
  gbmv = Z.gbmv
  hemv = Z.hemv
  hbmv = Z.hbmv
  hpmv = Z.hpmv
  trmv = Z.trmv
  tbmv = Z.tbmv
  tpmv = Z.tpmv
  trsv = Z.trsv
  tbsv = Z.tbsv
  tpsv = Z.tpsv
  geru = Z.geru
  gerc = Z.gerc
  her = Z.her
  hpr = Z.hpr
  her2 = Z.her2
  hpr2 = Z.hpr2
  gemm = Z.gemm
  symm = Z.symm
  hemm = Z.hemm
  syrk = Z.syrk
  herk = Z.herk
  syr2k = Z.syr2k
  her2k = Z.her2k
  trmm = Z.trmm
  trsm = Z.trsm
instance RealNumeric Float where
  rotg = S.rotg
  rotmg = S.rotmg
  rot = S.rot
  rotm = S.rotm


instance RealNumeric Double where
  rotg = D.rotg
  rotmg = D.rotmg
  rot = D.rot
  rotm = D.rotm
