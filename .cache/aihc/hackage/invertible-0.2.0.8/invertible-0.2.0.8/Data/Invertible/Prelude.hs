-- |
-- The bidirectional \"Prelude\", which re-exports various bijections similar to functions from "Prelude".
-- Most \"un\"-functions are left out for obvious reasons.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
#if !(defined(VERSION_semigroupoids) && MIN_VERSION_semigroupoids(5,2,2))
{-# LANGUAGE Safe #-}
#endif
module Data.Invertible.Prelude
  ( (<->)
  , type (<->)

  , const
  , flip
  , id
  , (.)
  , not
  , enum
  , succ
  , fst
  , snd
  , curry
  , cons
  , uncons
  , bifmap
  , Functor(..)
  , (<$>)
  , map
  , reverse
  , zip
  , zip3
  , zipWith
  , lines
  , words
  ) where

import Prelude hiding (not, id, (.), const, succ, flip, Functor(..), (<$>), fst, snd, curry, uncurry, map, reverse, zip, zip3, unzip, zipWith, lines, words)

import Control.Invertible.BiArrow
import Control.Invertible.Functor
import Data.Invertible.Bijection
import Data.Invertible.Bool
import Data.Invertible.Enum
import Data.Invertible.Function
import Data.Invertible.Functor
import Data.Invertible.Tuple
import Data.Invertible.List
