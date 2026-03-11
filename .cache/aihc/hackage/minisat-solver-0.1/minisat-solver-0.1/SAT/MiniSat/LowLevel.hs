-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

{-# LANGUAGE ForeignFunctionInterface #-}

-- | A low-level Haskell wrapper around the MiniSat-All library.

module SAT.MiniSat.LowLevel where

import Foreign
import Foreign.C.Types

-- ----------------------------------------------------------------------
-- * Some types

-- | An abstract type for the solver.
data C_Solver = C_Solver

-- | The marshalled \"int\" type from C.
type C_Int = Int32

-- | The marshalled \"lit\" type from the MiniSat library.
type C_Lit = C_Int

-- | The marshalled \"bool\" type from the MiniSat library.
type C_Bool = C_Int

-- | The marshalled \"lbool\" type from the MiniSat library.
type C_LBool = CChar

-- ----------------------------------------------------------------------
-- * Conversions

-- | Low-level wrapper around the C function /lit_new/.
foreign import ccall unsafe "solver.h lit_new"
  c_lit_new :: C_Int -> C_Bool -> C_Lit

-- ----------------------------------------------------------------------
-- * C API functions

-- | Low-level wrapper around the C function /solver_new/.
foreign import ccall unsafe "solver.h solver_new"
  c_solver_new :: IO (Ptr C_Solver)

-- | Low-level wrapper around the C function /solver_delete/.
foreign import ccall unsafe "solver.h solver_delete"
  c_solver_delete :: Ptr C_Solver -> IO ()

-- | Free a solver, callable by the garbage collector.
foreign import ccall unsafe "solver.h &solver_delete"
  c_solver_delete_ptr :: FinalizerPtr C_Solver

-- | Low-level wrapper around the C function /solver_addclause/.
foreign import ccall unsafe "solver.h solver_addclause"
  c_solver_addclause :: Ptr C_Solver -> Ptr C_Lit -> C_Int -> IO C_Bool

-- | Low-level wrapper around the C function /solver_solve_start/.
foreign import ccall unsafe "solver.h solver_solve_start"
  c_solver_solve_start :: Ptr C_Solver -> IO ()

-- | Low-level wrapper around the C function /solver_solve_next/.
foreign import ccall unsafe "solver.h solver_solve_next"
  c_solver_solve_next :: Ptr C_Solver -> IO C_Bool

-- | Low-level wrapper around the C function /solver_solve_finish/.
foreign import ccall unsafe "solver.h solver_solve_finish"
  c_solver_solve_finish :: Ptr C_Solver -> IO ()

-- | Low-level wrapper around the C function /solver_nvars/.
foreign import ccall unsafe "solver.h solver_nvars"
  c_solver_nvars :: Ptr C_Solver -> IO C_Int

-- | Low-level wrapper around the C function /solver_solution/.
foreign import ccall unsafe "solver.h solver_solution"
  c_solver_solution :: Ptr C_Solver -> IO (Ptr C_LBool)
