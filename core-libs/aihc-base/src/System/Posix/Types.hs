{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The POSIX types that the C bindings of the boot libraries use.
module System.Posix.Types
  ( CSsize (..),
    Fd (..),
  )
where

import Foreign.C.Types (CInt (..))
import Foreign.C.Types.Repr (CSsizeRep)
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Num (Num (..))
import GHC.Real (Integral (..), Real (..))

-- | The C @ssize_t@: a byte count that can also carry @-1@ for an error.
newtype CSsize = CSsize CSsizeRep
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

-- | The POSIX file descriptor: the C @int@ that names an open file.
newtype Fd = Fd CInt
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)
