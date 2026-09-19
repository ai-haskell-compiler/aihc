{-# LANGUAGE DerivingStrategies #-}

-- | The mode in which a program opens a file.
module GHC.IO.IOMode
  ( IOMode (..),
  )
where

import GHC.Enum (Bounded, Enum)
import GHC.Internal.Classes (Eq, Ord)
import GHC.Show (Show)

data IOMode
  = ReadMode
  | WriteMode
  | AppendMode
  | ReadWriteMode
  deriving stock (Eq, Ord, Enum, Bounded, Show)
