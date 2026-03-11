-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | A monadic interface to the functions of
-- "SAT.MiniSat.LowLevel". This layer:
--
-- * hooks into garbage collection;
--
-- * converts low-level types to Haskell types;
--
-- * replaces the 'IO' monad by a state monad;
-- 
-- * converts low-level errors to I/O exceptions.
--
-- All of the exported functions may potentially raise an exception.

module SAT.MiniSat.Monadic where

import SAT.MiniSat.LowLevel
import SAT.MiniSat.Literals

import Foreign
import Foreign.C.Error
import Control.Monad.Trans.State
import System.IO.Unsafe

-- ----------------------------------------------------------------------
-- * Error handling

-- | Check an assertion, and raise an 'IO' exception (using the current
-- errno and the supplied location string) if the assertion fails.
ensure :: String -> Bool -> IO ()
ensure s True = return ()
ensure s False = throwErrno s

-- ----------------------------------------------------------------------
-- * Conversions

-- | Marshall a C boolean to a Haskell boolean.
bool_of_cbool :: C_Bool -> Bool
bool_of_cbool 0 = False
bool_of_cbool n = True

-- | Marshall a Haskell boolean to a C boolean.
cbool_of_bool :: Bool -> C_Bool
cbool_of_bool True = 1
cbool_of_bool False = 0

-- | Marshall a Haskell literal to a MiniSat literal.
clit_of_lit :: Lit Int -> C_Lit
clit_of_lit (Pos n) = c_lit_new (fromIntegral n) (cbool_of_bool False)
clit_of_lit (Neg n) = c_lit_new (fromIntegral n) (cbool_of_bool True)

-- | Marshall an \"lbool\" to a 'Maybe' 'Bool'.
maybebool_of_lbool :: C_LBool -> Maybe Bool
maybebool_of_lbool 0 = Nothing
maybebool_of_lbool 1 = Just True
maybebool_of_lbool (-1) = Just False
maybebool_of_lbool _ = error "maybebool_of_lbool"

-- ----------------------------------------------------------------------
-- * High-level Solver type

-- | The type of solvers.
newtype Solver = Solver (ForeignPtr C_Solver)

-- | Convert a low-level pointer to a garbage-collected 'Solver'.
make_Solver :: Ptr C_Solver -> IO Solver
make_Solver p = do
  fptr <- newForeignPtr c_solver_delete_ptr p
  return (Solver fptr)

-- | Marshall a 'Solver' to a 'Ptr' 'C_Solver'.
with_Solver :: Solver -> (Ptr C_Solver -> IO a) -> IO a
with_Solver (Solver fptr) = withForeignPtr fptr

-- | Create a new solver.
solver_new :: IO Solver
solver_new = do
  p <- c_solver_new
  ensure "solver_new" (p /= nullPtr)
  make_Solver p

-- ----------------------------------------------------------------------
-- * State monad

-- $ The low-level C functions live in the 'IO' monad. However, the only
-- actual side effect of these functions is to update the 'Solver'
-- object. Therefore, it is safe to lift these functions to a state
-- monad. Moreover, in doing so, we are able to exploit laziness in a
-- way that is not possible in the 'IO' monad.

-- | The 'SolverState' monad is for functions whose only side effect
-- is to update the 'Solver' object.
type SolverState a = State Solver a

-- | Marshall a function from the 'IO' monad to the state monad. The
-- call to 'unsafePerformIO' is safe, provided that the 'IO'
-- computation's only side effect is to update the 'Solver' object.
stateful :: (Solver -> IO a) -> SolverState a
stateful body = state $ \s -> unsafePerformIO $ do
  r <- body s
  return (r, s)

-- | Run a 'SolverState' computation in the context of a newly created
-- solver. The use of 'unsafePerformIO' is safe because the only side
-- effect of 'solver_new' is to create a new solver state.
m_run :: SolverState a -> a
m_run body = unsafePerformIO $ do
  s <- solver_new
  let (a, s') = runState body s
  return a

-- ----------------------------------------------------------------------
-- * Marshalled API functions

-- | Add a clause to the solver. May return 'False' if the clause is
-- unsatisfiable, in which case the Solver should not be searched.
-- The clause is represented as a list of literals.
m_solver_addclause :: [Lit Int] -> SolverState Bool
m_solver_addclause lits = stateful $ \s -> do
  with_Solver s $ \s_ptr -> do
    withArrayLen clits $ \n clits_ptr -> do
      r <- c_solver_addclause s_ptr clits_ptr (fromIntegral n)
      return (bool_of_cbool r)
  where
    clits = map clit_of_lit lits

-- | Initialize the solver for searching.
m_solver_solve_start :: SolverState ()
m_solver_solve_start = stateful $ \s -> do
  with_Solver s $ \s_ptr -> do
    c_solver_solve_start s_ptr

-- | Search the next solution.
m_solver_solve_next :: SolverState Bool
m_solver_solve_next = stateful $ \s -> do
  with_Solver s $ \s_ptr -> do
    r <- c_solver_solve_next s_ptr
    return (bool_of_cbool r)

-- | Finalizer to run after search.
m_solver_solve_finish :: SolverState ()
m_solver_solve_finish = stateful $ \s -> do
  with_Solver s $ \s_ptr -> do
    c_solver_solve_finish s_ptr

-- | Retrieve the most recent solution (only valid if
-- 'm_solver_solve_next' returned 'True', and no other API functions
-- have been called since then). The solution is represented as a
-- list, where the /n/th element of the list represents the assignment
-- ('True', 'False', or 'Nothing') of variable /n/.
m_solver_solution :: SolverState [Maybe Bool]
m_solver_solution = stateful $ \s -> do
  with_Solver s $ \s_ptr -> do
    lbools_ptr <- c_solver_solution s_ptr
    n <- c_solver_nvars s_ptr
    lbools <- peekArray (fromIntegral n) lbools_ptr
    let mbools = map maybebool_of_lbool lbools
    return mbools

-- ----------------------------------------------------------------------
-- * Derived API functions

-- | Add a list of clauses to the solver. May return 'False' if the
-- clauses are unsatisfiable, in which case the Solver should not be
-- searched.
m_solver_addclauses :: [[Lit Int]] -> SolverState Bool
m_solver_addclauses [] = return True
m_solver_addclauses (h:t) = do
  r <- m_solver_addclause h
  if r == False then do
    return False
    else do
    m_solver_addclauses t

-- | Search and return the next solution.
m_solver_next_solution :: SolverState (Maybe [Maybe Bool])
m_solver_next_solution = do
  r <- m_solver_solve_next
  if r == False then do
    return Nothing
    else do
    sol <- m_solver_solution
    return (Just sol)

-- | Return all remaining solutions.
m_solver_next_solutions :: SolverState [[Maybe Bool]]
m_solver_next_solutions = do
  r <- m_solver_next_solution
  case r of
   Nothing -> do
     return []
   Just h -> do
     t <- m_solver_next_solutions
     return (h:t)

