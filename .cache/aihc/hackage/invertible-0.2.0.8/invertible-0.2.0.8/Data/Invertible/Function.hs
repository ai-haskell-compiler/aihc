-- |
-- Bidirectional version of "Data.Function".
{-# LANGUAGE Safe, TypeOperators #-}
module Data.Invertible.Function
  ( id
  , (.)
  , consts
  , const
  , flip
  ) where

import Prelude hiding (id, (.), const, flip)
import qualified Control.Category as C
import qualified Data.Function as F

import Data.Invertible.Bijection

-- |Identity bijection.
id :: a <-> a
id = C.id

-- |Bijection composition
(.) :: (b <-> c) -> (a <-> b) -> a <-> c
(.) = (C..)
infixr 9 .

-- |Bidirectional constant function (not a true bijection).
consts :: a -> b -> a <-> b
consts a b = F.const b :<->: F.const a

-- |Convert between '()' and a constant (not a true bijection).
const :: a -> () <-> a
const = consts ()

-- |'F.flip' the order of the first two arguments of a function.
flip :: (a -> b -> c) <-> (b -> a -> c)
flip = F.flip :<->: F.flip
