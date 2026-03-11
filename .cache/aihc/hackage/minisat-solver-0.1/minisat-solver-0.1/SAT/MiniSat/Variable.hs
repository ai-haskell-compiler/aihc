-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | A more convenient interface to the functionality of 
-- "SAT.MiniSat.Functional". This layer:
--
-- * permits any ordered type to be used as the type of boolean
-- variables;
--
-- * represents solutions as a 'Map' from variables to booleans.

module SAT.MiniSat.Variable where

import SAT.MiniSat.Functional
import SAT.MiniSat.Literals

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe

-- ----------------------------------------------------------------------
-- * Auxiliary functions

-- | Extract the underlying variable of a literal.
var_of_lit :: Lit a -> a
var_of_lit (Pos v) = v
var_of_lit (Neg v) = v

-- | Map a variable function over literals.
lit_map :: (a -> b) -> Lit a -> Lit b
lit_map f (Pos x) = Pos (f x)
lit_map f (Neg x) = Neg (f x)

-- | Turn a list into a set and then back into a list. This sorts the
-- list and removes duplicates.
setify :: (Ord a) => [a] -> [a]
setify a = Set.toList (Set.fromList a)

-- ----------------------------------------------------------------------
-- * Primitive API function

-- | Compute the list of all solutions of a SAT instance.
--
-- A conjunctive normal form is represented as a list of clauses, each
-- of which is a list of literals.
--
-- A solution is represented as a (total or partial) truth assignment,
-- i.e., a map from variables to booleans.
cnf_solve_all :: (Ord v) => [[Lit v]] -> [Map v Bool]
cnf_solve_all cnf = results where
  vars = setify [ v | c <- cnf, l <- c, let v = var_of_lit l ]
  bij = zip vars [0..]
  int_of_var = (Map.!) (Map.fromList bij)
  var_of_int = (Map.!) (Map.fromList [ (n,v) | (v,n) <- bij ])
  m_cnf = map convert_clause cnf
  convert_clause c = setify [ lit_map int_of_var l | l <- c ]
  m_results = m_solve m_cnf
  results = map convert_result m_results
  convert_result l = r
    where
      r = Map.fromList [ (var_of_int n, b) | (n, Just b) <- zip [0..] l ]

-- ----------------------------------------------------------------------
-- * Derived API functions

-- | Check whether the SAT instance is satisfiable, and return a
-- satisfying assignment if there is one.
cnf_solve :: (Ord v) => [[Lit v]] -> Maybe (Map v Bool)
cnf_solve cnf = listToMaybe (cnf_solve_all cnf)

-- | Check whether the SAT instance is satisfiable.
cnf_satisfiable :: (Ord v) => [[Lit v]] -> Bool
cnf_satisfiable cnf = isJust (cnf_solve cnf)

