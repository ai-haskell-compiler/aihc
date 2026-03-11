-- |
-- Bidirectional functions.
-- The type 'Data.Invertible.Bijection.<->' is the basis, representing a bijective function between two types.
-- Bijections can be constructed from two functions with 'Control.Invertible.BiArrow.<->' or from a set of Haskell cases using 'biCase'.
--
-- This and other modules in this package export functionality generally mirroring that provided by the base modules, but over bijections.  They are thus intended to be imported qualified, e.g.,:
-- 
-- > import qualified Data.Invertible as Inv
--
{-# LANGUAGE CPP #-}
#if !(defined(VERSION_semigroupoids) && MIN_VERSION_semigroupoids(5,2,2))
{-# LANGUAGE Safe #-}
#endif
module Data.Invertible
  ( module Data.Invertible.Bijection
  , module Data.Invertible.TH
  , module Data.Invertible.Prelude
  , module Data.Invertible.Bits
  , module Data.Invertible.Bool
  , module Data.Invertible.Complex
  , module Data.Invertible.Either
  , module Data.Invertible.Function
  , module Data.Invertible.Functor
  , module Data.Invertible.List
  , module Data.Invertible.Maybe
  , module Data.Invertible.Monad
  , module Data.Invertible.Monoid
  , module Data.Invertible.Ord
  , module Data.Invertible.Tuple
  , module Control.Invertible.BiArrow
  , module Control.Invertible.Functor
  ) where

import Data.Invertible.Bijection
import Data.Invertible.TH
import Data.Invertible.Prelude
import Data.Invertible.Bits
import Data.Invertible.Bool
import Data.Invertible.Complex
import Data.Invertible.Either
import Data.Invertible.Function
import Data.Invertible.Functor
import Data.Invertible.List
import Data.Invertible.Maybe
import Data.Invertible.Monad
import Data.Invertible.Monoid
import Data.Invertible.Ord
import Data.Invertible.Tuple
import Control.Invertible.BiArrow
import Control.Invertible.Functor
