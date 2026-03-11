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
--
--   /Since: 0.1.1/
module Blas.Specialized.Double.Unsafe (
  -- * Level 1: vector-vector operations
  -- ** Givens rotations
    rotg
  , rotmg
  , rot
  , rotm
  -- ** Basic operations
  , swap
  , scal
  , copy
  , axpy
  , dotu
  , dotc
  , dsdot
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
import Blas.Primitive.Types (Order, Transpose, Uplo, Diag, Side)
import qualified Blas.Primitive.Unsafe as C

-- | Generate a Givens rotation. (Only available for real floating-point types.)
rotg :: Ptr Double
     -> Ptr Double
     -> Ptr Double
     -> Ptr Double
     -> IO ()
rotg = C.drotg


-- | Generate a modified Givens rotation. (Only available for real floating-point
--   types.)
rotmg :: Ptr Double
      -> Ptr Double
      -> Ptr Double
      -> Double
      -> Ptr Double
      -> IO ()
rotmg = C.drotmg


-- | Apply a Givens rotation. (Only available for real floating-point types.)
rot :: Int
    -> Ptr Double
    -> Int
    -> Ptr Double
    -> Int
    -> Double
    -> Double
    -> IO ()
rot = C.drot


-- | Apply a modified Givens rotation. (Only available for real floating-point types.)
rotm :: Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> IO ()
rotm = C.drotm


-- | Swap two vectors:
--
--   > (x, y) ← (y, x)
swap :: Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
swap = C.dswap


-- | Multiply a vector by a scalar.
--
--   > x ← α x
scal :: Int
     -> Double
     -> Ptr Double
     -> Int
     -> IO ()
scal = C.dscal


-- | Copy a vector into another vector:
--
--   > y ← x
copy :: Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
copy = C.dcopy


-- | Add a scalar-vector product to a vector.
--
--   > y ← α x + y
axpy :: Int
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
axpy = C.daxpy


-- | Calculate the bilinear dot product of two vectors:
--
--   > x ⋅ y ≡ ∑[i] x[i] y[i]
dotu :: Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO Double
dotu = C.ddot


-- | Calculate the sesquilinear dot product of two vectors.
--
--   > x* ⋅ y ≡ ∑[i] x[i]* y[i]
dotc :: Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO Double
dotc = C.ddot


-- | Calculate the dot product of two vectors with extended precision accumulation of the
--   intermediate results and return a double-precision result.  (Only available in
--   the @Double@ module.)
dsdot :: Int
      -> Ptr Float
      -> Int
      -> Ptr Float
      -> Int
      -> IO Double
dsdot = C.dsdot


-- | Calculate the Euclidean (L²) norm of a vector:
--
--   > ‖x‖₂ ≡ √(∑[i] x[i]²)
nrm2 :: Int
     -> Ptr Double
     -> Int
     -> IO Double
nrm2 = C.dnrm2


-- | Calculate the Manhattan (L¹) norm, equal to the sum of the magnitudes of the elements:
--
--   > ‖x‖₁ = ∑[i] |x[i]|
asum :: Int
     -> Ptr Double
     -> Int
     -> IO Double
asum = C.dasum


-- | Calculate the index of the element with the maximum magnitude (absolute value).
iamax :: Int
      -> Ptr Double
      -> Int
      -> IO Int
iamax = C.idamax


-- | Perform a general matrix-vector update.
--
--   > y ← α T(A) x + β y
gemv :: Order
     -> Transpose
     -> Int
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> IO ()
gemv = C.dgemv


-- | Perform a general banded matrix-vector update.
--
--   > y ← α T(A) x + β y
gbmv :: Order
     -> Transpose
     -> Int
     -> Int
     -> Int
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> IO ()
gbmv = C.dgbmv


-- | Perform a hermitian matrix-vector update.
--
--   > y ← α A x + β y
hemv :: Order
     -> Uplo
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> IO ()
hemv = C.dsymv


-- | Perform a hermitian banded matrix-vector update.
--
--   > y ← α A x + β y
hbmv :: Order
     -> Uplo
     -> Int
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> IO ()
hbmv = C.dsbmv


-- | Perform a hermitian packed matrix-vector update.
--
--   > y ← α A x + β y
hpmv :: Order
     -> Uplo
     -> Int
     -> Double
     -> Ptr Double
     -> Ptr Double
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> IO ()
hpmv = C.dspmv


-- | Multiply a triangular matrix by a vector.
--
--   > x ← T(A) x
trmv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
trmv = C.dtrmv


-- | Multiply a triangular banded matrix by a vector.
--
--   > x ← T(A) x
tbmv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
tbmv = C.dtbmv


-- | Multiply a triangular packed matrix by a vector.
--
--   > x ← T(A) x
tpmv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Ptr Double
     -> Ptr Double
     -> Int
     -> IO ()
tpmv = C.dtpmv


-- | Multiply an inverse triangular matrix by a vector.
--
--   > x ← T(A⁻¹) x
trsv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
trsv = C.dtrsv


-- | Multiply an inverse triangular banded matrix by a vector.
--
--   > x ← T(A⁻¹) x
tbsv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
tbsv = C.dtbsv


-- | Multiply an inverse triangular packed matrix by a vector.
--
--   > x ← T(A⁻¹) x
tpsv :: Order
     -> Uplo
     -> Transpose
     -> Diag
     -> Int
     -> Ptr Double
     -> Ptr Double
     -> Int
     -> IO ()
tpsv = C.dtpsv


-- | Perform an unconjugated rank-1 update of a general matrix.
--
--   > A ← α x y⊤ + A
geru :: Order
     -> Int
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
geru = C.dger


-- | Perform a conjugated rank-1 update of a general matrix.
--
--   > A ← α x y† + A
gerc :: Order
     -> Int
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
gerc = C.dger


-- | Perform a rank-1 update of a Hermitian matrix.
--
--   > A ← α x y† + A
her :: Order
    -> Uplo
    -> Int
    -> Double
    -> Ptr Double
    -> Int
    -> Ptr Double
    -> Int
    -> IO ()
her = C.dsyr


-- | Perform a rank-1 update of a Hermitian packed matrix.
--
--   > A ← α x y† + A
hpr :: Order
    -> Uplo
    -> Int
    -> Double
    -> Ptr Double
    -> Int
    -> Ptr Double
    -> IO ()
hpr = C.dspr


-- | Perform a rank-2 update of a Hermitian matrix.
--
--   > A ← α x y† + y (α x)† + A
her2 :: Order
     -> Uplo
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
her2 = C.dsyr2


-- | Perform a rank-2 update of a Hermitian packed matrix.
--
--   > A ← α x y† + y (α x)† + A
hpr2 :: Order
     -> Uplo
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> IO ()
hpr2 = C.dspr2


-- | Perform a general matrix-matrix update.
--
--   > C ← α T(A) U(B) + β C
gemm :: Order -- ^ Layout of all the matrices.
     -> Transpose -- ^ The operation @T@ to be applied to @A@.
     -> Transpose -- ^ The operation @U@ to be applied to @B@.
     -> Int -- ^ Number of rows of @T(A)@ and @C@.
     -> Int -- ^ Number of columns of @U(B)@ and @C@.
     -> Int -- ^ Number of columns of @T(A)@ and number of rows of @U(B)@.
     -> Double -- ^ Scaling factor @α@ of the product.
     -> Ptr Double -- ^ Pointer to a matrix @A@.
     -> Int -- ^ Stride of the major dimension of @A@.
     -> Ptr Double -- ^ Pointer to a matrix @B@.
     -> Int -- ^ Stride of the major dimension of @B@.
     -> Double -- ^ Scaling factor @β@ of the original @C@.
     -> Ptr Double -- ^ Pointer to a mutable matrix @C@.
     -> Int -- ^ Stride of the major dimension of @C@.
     -> IO ()
gemm = C.dgemm


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
     -> Double -- ^ Scaling factor @α@ of the product.
     -> Ptr Double -- ^ Pointer to a symmetric matrix @A@.
     -> Int -- ^ Stride of the major dimension of @A@.
     -> Ptr Double -- ^ Pointer to a matrix @B@.
     -> Int -- ^ Stride of the major dimension of @B@.
     -> Double -- ^ Scaling factor @α@ of the original @C@.
     -> Ptr Double -- ^ Pointer to a mutable matrix @C@.
     -> Int -- ^ Stride of the major dimension of @C@.
     -> IO ()
symm = C.dsymm


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
     -> Double -- ^ Scaling factor @α@ of the product.
     -> Ptr Double -- ^ Pointer to a Hermitian matrix @A@.
     -> Int -- ^ Stride of the major dimension of @A@.
     -> Ptr Double -- ^ Pointer to a matrix @B@.
     -> Int -- ^ Stride of the major dimension of @B@.
     -> Double -- ^ Scaling factor @α@ of the original @C@.
     -> Ptr Double -- ^ Pointer to a mutable matrix @C@.
     -> Int -- ^ Stride of the major dimension of @C@.
     -> IO ()
hemm = C.dsymm


-- | Perform a symmetric rank-k update.
--
--   > C ← α A A⊤ + β C    or    C ← α A⊤ A + β C
syrk :: Order
     -> Uplo
     -> Transpose
     -> Int
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> IO ()
syrk = C.dsyrk


-- | Perform a Hermitian rank-k update.
--
--   > C ← α A A† + β C    or    C ← α A† A + β C
herk :: Order
     -> Uplo
     -> Transpose
     -> Int
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> Double
     -> Ptr Double
     -> Int
     -> IO ()
herk = C.dsyrk


-- | Perform a symmetric rank-2k update.
--
--   > C ← α A B⊤ + α* B A⊤ + β C    or    C ← α A⊤ B + α* B⊤ A + β C
syr2k :: Order
      -> Uplo
      -> Transpose
      -> Int
      -> Int
      -> Double
      -> Ptr Double
      -> Int
      -> Ptr Double
      -> Int
      -> Double
      -> Ptr Double
      -> Int
      -> IO ()
syr2k = C.dsyr2k


-- | Perform a Hermitian rank-2k update.
--
--   > C ← α A B† + α* B A† + β C    or    C ← α A† B + α* B† A + β C
her2k :: Order
      -> Uplo
      -> Transpose
      -> Int
      -> Int
      -> Double
      -> Ptr Double
      -> Int
      -> Ptr Double
      -> Int
      -> Double
      -> Ptr Double
      -> Int
      -> IO ()
her2k = C.dsyr2k


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
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
trmm = C.dtrmm


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
     -> Double
     -> Ptr Double
     -> Int
     -> Ptr Double
     -> Int
     -> IO ()
trsm = C.dtrsm
