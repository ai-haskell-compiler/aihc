-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | A functional (non-monadic) interface to the functions of
-- "SAT.MiniSat.Monadic". This layer:
--
-- * hides the existence of a solver object by providing a function
--   mapping SAT instances to a lazy list of solutions.

module SAT.MiniSat.Functional where

import SAT.MiniSat.Monadic
import SAT.MiniSat.Literals

-- | Input a CNF and output the a (lazily generated) list of
-- solutions. Variables are represented as integers, which should be
-- consecutive and start from 0.
m_solve :: [[Lit Int]] -> [[Maybe Bool]]
m_solve clauses = m_run $ do
  r <- m_solver_addclauses clauses
  if r == False then do
    return []
    else do
    m_solver_solve_start
    sols <- m_solver_next_solutions
    return sols
