/**********************************************************************
MiniSat -- Copyright (c) 2005, Niklas Sorensson
http://www.cs.chalmers.se/Cs/Research/FormalMethods/MiniSat/

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**********************************************************************/
// Modified to compile with MS Visual Studio 6.0 by Alan Mishchenko

#ifndef solver_h
#define solver_h

#ifdef _WIN32
#define inline __inline		// compatible with MS VS 6.0
#endif

#include <stdio.h>
#include <time.h>
#include "vec.h"

#ifdef GMP
#include <gmp.h>
#endif

// ======================================================================
// Simple types:

// does not work for c++
typedef int bool;
static const bool true = 1;
static const bool false = 0;

typedef int lit;
typedef char lbool;

#ifdef _WIN32
typedef signed __int64 uint64;	// compatible with MS VS 6.0
#else
typedef unsigned long long uint64;
#endif

static const int var_Undef = -1;
static const lit lit_Undef = -2;

static const lbool l_Undef = 0;
static const lbool l_True = 1;
static const lbool l_False = -1;

static inline lit toLit(int v) {
  return v + v;
}

static inline lit lit_neg(lit l) {
  return l ^ 1;
}

static inline int lit_var(lit l) {
  return l >> 1;
}

static inline int lit_sign(lit l) {
  return (l & 1);
}


// ======================================================================
// Public interface:

// An API function for converting a variable and sign (false='+',
// true='-') to a literal.
extern lit lit_new(int v, bool sign);

struct solver_t;
typedef struct solver_t solver;

// Allocate a new empty solver.
extern solver *solver_new(void);
// Free a solver.
extern void solver_delete(solver *s);

// Add a clause to the solver. The clause consists of an array of n
// literals starting at begin. Literals are created from non-negative
// integers (0,1,2,...) with toLit and lit_neg.
//
// Returns false if the clause is empty, or if it is unsatisfiable in
// the current assignment.
//
// There might be implicit assumptions made about the variables being
// given in increasing order? Not sure, since original code is not
// documented.
extern bool solver_addclause(solver *s, lit *begin, int n);

// Simplify the solver state. Return false if unsatisfiable?
extern bool solver_simplify(solver *s);

// Prepare for solving. Must call this before solver_solve_next().
extern void solver_solve_start(solver *s);

// Search for the next solution. Return true if a solution was found,
// else false. This can be called repeatedly until false. Must call
// solver_solve_start() first, and solver_solve_finish() at the end.
// The solution, if any, can be retrieved with solver_solution.
extern bool solver_solve_next(solver *s);

// Reset solver after searching is finished.
extern void solver_solve_finish(solver *s);

// Solve the constraints and print out all solutions.
extern bool solver_solve(solver *s);

// Get the number of variables.
extern int solver_nvars(solver *s);

// Get the current solution (valid if the most recent call to
// solver_solve_next returned true). The solution is of length
// solver_nvars(s). Note that the returned pointer may be invalidated
// upon further API calls.
extern lbool *solver_solution(solver *s);

extern int solver_nclauses(solver *s);
extern int solver_norigclauses(solver *s);
extern int solver_nconflicts(solver *s);

extern void solver_setnvars(solver *s, int n);

struct stats_t {
  uint64 starts, decisions, propagations, inspects, conflicts;
  uint64 clauses, clauses_literals, learnts, learnts_literals, max_literals, tot_literals;
#ifdef GMP
  mpz_t tot_solutions, par_solutions, cnt;
#else
  uint64 tot_solutions, par_solutions;
#endif
  clock_t clk;
};
typedef struct stats_t stats;

// ======================================================================
// Solver representation:

struct clause_t;
typedef struct clause_t clause;

struct solver_t {
  int size;			// nof variables
  int cap;			// size of varmaps
  int qhead;			// Head index of queue.
  int qtail;			// Tail index of queue.

#ifdef FIXEDORDER
  int nextvar;			// variable to be considered next
#endif
  // clauses
  vecp clauses;			// List of problem constraints. (contains: clause*)
  vecp learnts;			// List of learnt clauses. (contains: clause*)

  // activities
  double var_inc;		// Amount to bump next variable with.
  double var_decay;		// INVERSE decay factor for variable activity: stores 1/decay. 
  float cla_inc;		// Amount to bump next clause with.
  float cla_decay;		// INVERSE decay factor for clause activity: stores 1/decay.

  vecp *wlists;			// 
  double *activity;		// A heuristic measurement of the activity of a variable.
  lbool *assigns;		// Current values of variables.
  int *orderpos;		// Index in variable order.
  clause **reasons;		//
  int *levels;			//
  lit *trail;

  int norigclauses;		// number of original clauses, meaning those not added as blocking clauses. this number dynamically changes due to simplification.
  lbool *chosen;		// 
  FILE *out;			//
  veci blkcls;			// holds a blocking clause

  clause *binary;		// A temporary binary clause
  lbool *tags;			//
  veci tagged;			// (contains: var)
  veci stack;			// (contains: var)

  veci order;			// Variable order. (heap) (contains: var)
  veci trail_lim;		// Separator indices for different decision levels in 'trail'. (contains: int)


  int root_level;		// Level of first proper decision.
  int simpdb_assigns;		// Number of top-level assignments at last 'simplifyDB()'.
  int simpdb_props;		// Number of propagations before next 'simplifyDB()'.
  double random_seed;
  double progress_estimate;
  int verbosity;		// Verbosity level. 0=silent, 1=some progress report, 2=everything

  int nof_learnts;              // Upper limit on number of learnt clauses during searching
  bool done;                    // Used by solver_solve_next to remember end of solutions
  lbool *solution;              // Used by solver_solve_next to return solution
  
  stats stats;
};

#endif				/* solver_h */
