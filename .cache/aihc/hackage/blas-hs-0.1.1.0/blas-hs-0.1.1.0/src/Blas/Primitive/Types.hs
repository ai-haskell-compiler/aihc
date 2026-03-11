{-|

Stability: experimental

Enumerations used by the Blas interface.

-}
module Blas.Primitive.Types
       ( Order(..)
       , Transpose(..)
       , Uplo(..)
       , Diag(..)
       , Side(..)
       ) where
import Prelude hiding (Either(Left, Right))
import Data.Ix (Ix)

-- | Memory layout of a matrix.
data Order = RowMajor                   -- ^ Row-major order.
           | ColMajor                   -- ^ Column-major order.
           deriving (Bounded, Eq, Ix, Ord, Read, Show)

instance Enum Order where
  fromEnum RowMajor = 101
  fromEnum ColMajor = 102
  toEnum 101 = RowMajor
  toEnum 102 = ColMajor
  toEnum x   = error $ "Order.toEnum: Cannot match " ++ show x

-- | Operation applied to a matrix.
data Transpose = NoTrans                -- ^ No operation.
               | Trans                  -- ^ Transposition.
               | ConjTrans              -- ^ Conjugation and transposition.
               deriving (Bounded, Eq, Ix, Ord, Read, Show)

instance Enum Transpose where
  fromEnum NoTrans   = 111
  fromEnum Trans     = 112
  fromEnum ConjTrans = 113
  toEnum 111 = NoTrans
  toEnum 112 = Trans
  toEnum 113 = ConjTrans
  toEnum x   = error $ "Transpose.toEnum: Cannot match " ++ show x

-- | Specifies which triangular part of a matrix is used.
data Uplo = Upper                       -- ^ Upper triangular part.
          | Lower                       -- ^ Lower triangular part.
          deriving (Bounded, Eq, Ix, Ord, Read, Show)

instance Enum Uplo where
  fromEnum Upper = 121
  fromEnum Lower = 122
  toEnum 121 = Upper
  toEnum 122 = Lower
  toEnum x   = error $ "Uplo.toEnum: Cannot match " ++ show x

-- | Whether a matrix is unit triangular.
data Diag = NonUnit                     -- ^ Not unit triangular.
          | Unit                        -- ^ Unit triangular.
          deriving (Bounded, Eq, Ix, Ord, Read, Show)

instance Enum Diag where
  fromEnum NonUnit = 131
  fromEnum Unit    = 132
  toEnum 131 = NonUnit
  toEnum 132 = Unit
  toEnum x   = error $ "Diag.toEnum: Cannot match " ++ show x

-- | The side that a matrix appears in the product.
data Side = Left                        -- ^ On the left side.
          | Right                       -- ^ On the right side.
          deriving (Bounded, Eq, Ix, Ord, Read, Show)

instance Enum Side where
  fromEnum Left  = 141
  fromEnum Right = 142
  toEnum 141 = Left
  toEnum 142 = Right
  toEnum x   = error $ "Side.toEnum: Cannot match " ++ show x
