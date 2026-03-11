-- This file is auto-generated.  Do not edit directly.
-- |
--   Stability: Experimental
--
--   Generic interface to Blas using safe foreign calls.  Refer to the GHC documentation
--   for more information regarding appropriate use of safe and unsafe foreign calls.
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
--
--   /Since: 0.1.1/
module Blas.Specialized.ComplexDouble.Safe (
  -- * Level 1: vector-vector operations
  -- ** Basic operations
    swap
  , scal
  , copy
  , axpy
  , dotu
  , dotc
  -- ** Norm operations
  , nrm2
  , asum
  , iamax
  -- * Level 2: matrix-vector operations
  -- ** Multiplication
  , gemv
  , gbmv
  , hemv
  , hbmv
  , hpmv
  -- ** Triangular operations
  , trmv
  , tbmv
  , tpmv
  , trsv
  , tbsv
  , tpsv
  -- ** Rank updates
  , geru
  , gerc
  , her
  , hpr
  , her2
  , hpr2
  -- * Level 3: matrix-matrix operations
  -- ** Multiplication
  , gemm
  , symm
  , hemm
  -- ** Rank updates
  , syrk
  , herk
  , syr2k
  , her2k
  -- ** Triangular operations
  , trmm
  , trsm
  ) where
import Foreign (Ptr)
import Data.Complex (Complex((:+)))
import FFI (getReturnValue)
import Blas.Primitive.Types (Order, Transpose, Uplo, Diag, Side)
import qualified Blas.Primitive.Safe as C

-- | Swap two vectors:
--
--   > (x, y) ← (y, x)
swap :: Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
swap = C.zswap


-- | Multiply a vector by a scalar.
--
--   > x ← α x
scal :: Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
scal n (alpha :+ 0) = C.zdscal n alpha
scal n alpha = C.zscal n alpha


-- | Copy a vector into another vector:
--
--   > y ← x
copy :: Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
copy = C.zcopy


-- | Add a scalar-vector product to a vector.
--
--   > y ← α x + y
axpy :: Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
axpy = C.zaxpy


-- | Calculate the bilinear dot product of two vectors:
--
--   > x ⋅ y ≡ ∑[i] x[i] y[i]
dotu :: Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO (Complex Double)
dotu a b c d e = getReturnValue (C.zdotu_sub a b c d e)


-- | Calculate the sesquilinear dot product of two vectors.
--
--   > x* ⋅ y ≡ ∑[i] x[i]* y[i]
dotc :: Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO (Complex Double)
dotc a b c d e = getReturnValue (C.zdotc_sub a b c d e)


-- | Calculate the Euclidean (L²) norm of a vector:
--
--   > ‖x‖₂ ≡ √(∑[i] x[i]²)
nrm2 :: Int
     -> Ptr (Complex Double)
     -> Int
     -> IO Double
nrm2 = C.dznrm2


-- | Calculate the Manhattan (L¹) norm, equal to the sum of the magnitudes of the elements:
--
--   > ‖x‖₁ = ∑[i] |x[i]|
asum :: Int
     -> Ptr (Complex Double)
     -> Int
     -> IO Double
asum = C.dzasum


-- | Calculate the index of the element with the maximum magnitude (absolute value).
iamax :: Int
      -> Ptr (Complex Double)
      -> Int
      -> IO Int
iamax = C.izamax


-- | Perform a general matrix-vector update.
--
--   > y ← α T(A) x + β y
gemv :: Order
     -> Transpose
     -> Int
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
gemv = C.zgemv


-- | Perform a general banded matrix-vector update.
--
--   > y ← α T(A) x + β y
gbmv :: Order
     -> Transpose
     -> Int
     -> Int
     -> Int
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
gbmv = C.zgbmv


-- | Perform a hermitian matrix-vector update.
--
--   > y ← α A x + β y
hemv :: Order
     -> Uplo
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
hemv = C.zhemv


-- | Perform a hermitian banded matrix-vector update.
--
--   > y ← α A x + β y
hbmv :: Order
     -> Uplo
     -> Int
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
hbmv = C.zhbmv


-- | Perform a hermitian packed matrix-vector update.
--
--   > y ← α A x + β y
hpmv :: Order
     -> Uplo
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Ptr (Complex Double)
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
hpmv = C.zhpmv


-- | Multiply a triangular matrix by a vector.
--
--   > x ← T(A) x
trmv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
trmv = C.ztrmv


-- | Multiply a triangular banded matrix by a vector.
--
--   > x ← T(A) x
tbmv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
tbmv = C.ztbmv


-- | Multiply a triangular packed matrix by a vector.
--
--   > x ← T(A) x
tpmv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Ptr (Complex Double)
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
tpmv = C.ztpmv


-- | Multiply an inverse triangular matrix by a vector.
--
--   > x ← T(A⁻¹) x
trsv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
trsv = C.ztrsv


-- | Multiply an inverse triangular banded matrix by a vector.
--
--   > x ← T(A⁻¹) x
tbsv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
tbsv = C.ztbsv


-- | Multiply an inverse triangular packed matrix by a vector.
--
--   > x ← T(A⁻¹) x
tpsv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Ptr (Complex Double)
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
tpsv = C.ztpsv


-- | Perform an unconjugated rank-1 update of a general matrix.
--
--   > A ← α x y⊤ + A
geru :: Order
     -> Int
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
geru = C.zgeru


-- | Perform a conjugated rank-1 update of a general matrix.
--
--   > A ← α x y† + A
gerc :: Order
     -> Int
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
gerc = C.zgerc


-- | Perform a rank-1 update of a Hermitian matrix.
--
--   > A ← α x y† + A
her :: Order
    -> Uplo
    -> Int
    -> Double
    -> Ptr (Complex Double)
    -> Int
    -> Ptr (Complex Double)
    -> Int
    -> IO ()
her = C.zher


-- | Perform a rank-1 update of a Hermitian packed matrix.
--
--   > A ← α x y† + A
hpr :: Order
    -> Uplo
    -> Int
    -> Double
    -> Ptr (Complex Double)
    -> Int
    -> Ptr (Complex Double)
    -> IO ()
hpr = C.zhpr


-- | Perform a rank-2 update of a Hermitian matrix.
--
--   > A ← α x y† + y (α x)† + A
her2 :: Order
     -> Uplo
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
her2 = C.zher2


-- | Perform a rank-2 update of a Hermitian packed matrix.
--
--   > A ← α x y† + y (α x)† + A
hpr2 :: Order
     -> Uplo
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> IO ()
hpr2 = C.zhpr2


-- | Perform a general matrix-matrix update.
--
--   > C ← α T(A) U(B) + β C
gemm :: Order -- ^ Layout of all the matrices.
     -> Transpose -- ^ The operation @T@ to be applied to @A@.
     -> Transpose -- ^ The operation @U@ to be applied to @B@.
     -> Int -- ^ Number of rows of @T(A)@ and @C@.
     -> Int -- ^ Number of columns of @U(B)@ and @C@.
     -> Int -- ^ Number of columns of @T(A)@ and number of rows of @U(B)@.
     -> Complex Double -- ^ Scaling factor @α@ of the product.
     -> Ptr (Complex Double) -- ^ Pointer to a matrix @A@.
     -> Int -- ^ Stride of the major dimension of @A@.
     -> Ptr (Complex Double) -- ^ Pointer to a matrix @B@.
     -> Int -- ^ Stride of the major dimension of @B@.
     -> Complex Double -- ^ Scaling factor @β@ of the original @C@.
     -> Ptr (Complex Double) -- ^ Pointer to a mutable matrix @C@.
     -> Int -- ^ Stride of the major dimension of @C@.
     -> IO ()
gemm = C.zgemm


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
     -> Complex Double -- ^ Scaling factor @α@ of the product.
     -> Ptr (Complex Double) -- ^ Pointer to a symmetric matrix @A@.
     -> Int -- ^ Stride of the major dimension of @A@.
     -> Ptr (Complex Double) -- ^ Pointer to a matrix @B@.
     -> Int -- ^ Stride of the major dimension of @B@.
     -> Complex Double -- ^ Scaling factor @α@ of the original @C@.
     -> Ptr (Complex Double) -- ^ Pointer to a mutable matrix @C@.
     -> Int -- ^ Stride of the major dimension of @C@.
     -> IO ()
symm = C.zsymm


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
     -> Complex Double -- ^ Scaling factor @α@ of the product.
     -> Ptr (Complex Double) -- ^ Pointer to a Hermitian matrix @A@.
     -> Int -- ^ Stride of the major dimension of @A@.
     -> Ptr (Complex Double) -- ^ Pointer to a matrix @B@.
     -> Int -- ^ Stride of the major dimension of @B@.
     -> Complex Double -- ^ Scaling factor @α@ of the original @C@.
     -> Ptr (Complex Double) -- ^ Pointer to a mutable matrix @C@.
     -> Int -- ^ Stride of the major dimension of @C@.
     -> IO ()
hemm = C.zhemm


-- | Perform a symmetric rank-k update.
--
--   > C ← α A A⊤ + β C    or    C ← α A⊤ A + β C
syrk :: Order
     -> Uplo
     -> Transpose
     -> Int
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
syrk = C.zsyrk


-- | Perform a Hermitian rank-k update.
--
--   > C ← α A A† + β C    or    C ← α A† A + β C
herk :: Order
     -> Uplo
     -> Transpose
     -> Int
     -> Int
     -> Double
     -> Ptr (Complex Double)
     -> Int
     -> Double
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
herk = C.zherk


-- | Perform a symmetric rank-2k update.
--
--   > C ← α A B⊤ + α* B A⊤ + β C    or    C ← α A⊤ B + α* B⊤ A + β C
syr2k :: Order
      -> Uplo
      -> Transpose
      -> Int
      -> Int
      -> Complex Double
      -> Ptr (Complex Double)
      -> Int
      -> Ptr (Complex Double)
      -> Int
      -> Complex Double
      -> Ptr (Complex Double)
      -> Int
      -> IO ()
syr2k = C.zsyr2k


-- | Perform a Hermitian rank-2k update.
--
--   > C ← α A B† + α* B A† + β C    or    C ← α A† B + α* B† A + β C
her2k :: Order
      -> Uplo
      -> Transpose
      -> Int
      -> Int
      -> Complex Double
      -> Ptr (Complex Double)
      -> Int
      -> Ptr (Complex Double)
      -> Int
      -> Double
      -> Ptr (Complex Double)
      -> Int
      -> IO ()
her2k = C.zher2k


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
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
trmm = C.ztrmm


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
     -> Complex Double
     -> Ptr (Complex Double)
     -> Int
     -> Ptr (Complex Double)
     -> Int
     -> IO ()
trsm = C.ztrsm
