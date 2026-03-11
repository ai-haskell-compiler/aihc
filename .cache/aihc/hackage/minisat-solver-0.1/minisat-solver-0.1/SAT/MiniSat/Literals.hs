-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | This module provides a type of literals. 

module SAT.MiniSat.Literals where

-- | A literal is a positive or negative atom.
data Lit a = Pos a | Neg a
           deriving (Eq, Show)

-- | We order literals first by variable, then by sign, so that dual
-- literals are neighbors in the ordering.
instance (Ord a) => Ord (Lit a) where
  compare (Pos x) (Pos y) = compare x y
  compare (Neg x) (Neg y) = compare x y
  compare (Pos x) (Neg y) = compare (x,False) (y,True)
  compare (Neg x) (Pos y) = compare (x,True) (y,False)

-- | Negate a literal.
neg :: Lit a -> Lit a
neg (Pos a) = Neg a
neg (Neg a) = Pos a
