-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | This module provides high-level Haskell bindings for the well-known
-- MiniSat satisfiability solver. It solves the boolean satisfiability
-- problem, i.e., the input is a boolean formula, and the output is a
-- list of all satisfying assignments.  MiniSat is a fully automated,
-- well-optimized general-purpose SAT solver written by Niklas Een and
-- Niklas Sorensson, and further modified by Takahisa Toda.
--
-- Unlike other similar Haskell packages, the purpose of this library
-- is not to provide access to low-level C functions, to give the user
-- fine-grained interactive control over the SAT solver, or to provide
-- building blocks for developing new SAT solvers. Rather, we package
-- the SAT solver as a general-purpose tool that can easily be
-- integrated into other programs as a turn-key solution to many
-- search problems.
-- 
-- It is well-known that the boolean satisfiability problem is
-- NP-complete, and therefore, no SAT solver can be efficient for all
-- inputs. However, there are many search problems that can be
-- naturally expressed as satisfiability problems and are nevertheless
-- tractable. Writing custom code to traverse the search space is
-- tedious and rarely efficient, especially in cases where the size of
-- the search space is sensitive to the order of traversal. This is
-- where using a general-purpose SAT solver can often be a time saver.
--
-- We provide a convenient high-level interface to the SAT solver,
-- hiding the complexity of the underlying C implementation.  The main
-- API function is 'solve_all', which inputs a boolean formula, and
-- outputs a list of all satisfying assignments (which are modelled as
-- maps from variables to truth values). The list is generated lazily,
-- so that the underlying low-level C code only does as much work as
-- necessary.
--
-- Two example programs illustrating the use of this library can be
-- found in the \"examples\" directory of the Cabal package.

module SAT.MiniSat (
  -- * Boolean formulas
  Formula(..),
  -- * SAT solver interface
  satisfiable,
  solve,
  solve_all
  ) where

import SAT.MiniSat.Formula
