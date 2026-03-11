-- |
-- Bidirectional version of "Data.Functor".
{-# LANGUAGE Safe, QuasiQuotes, TypeOperators #-}
module Data.Invertible.Functor
  ( bifmap
  , identity
  ) where

import Prelude hiding (fmap, (<$>))
import qualified Data.Functor as F
import Data.Functor.Identity (Identity(..))

import Data.Invertible.Bijection
import Data.Invertible.TH

-- |Lift both sides of an bijection over a functor using 'F.fmap'.
-- We name this bifmap in deference to the more useful 'Control.Invertible.Functor.fmap'.
bifmap :: Functor f => a <-> b -> f a <-> f b
bifmap (f :<->: g) = F.fmap f :<->: F.fmap g

-- |Convert the 'Identity' functor.
identity :: a <-> Identity a
identity = [biCase|a <-> Identity a|]
