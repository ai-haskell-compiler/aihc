{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Enum where
import qualified Data.Vector.Unboxing as V
import Data.Int

data OrderingEq = LT | LE | EQ | GE | GT
  deriving (Eq, Ord, Enum, Bounded, Read, Show)
  deriving V.Unboxable via V.Enum OrderingEq

data Direction = North | South | East | West
  deriving (Eq, Ord, Enum, Bounded, Read, Show)
  deriving V.Unboxable via V.EnumRep Int8 Direction
