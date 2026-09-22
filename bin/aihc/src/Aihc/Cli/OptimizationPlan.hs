-- | What an optimization level asks for, as a plan.
--
-- A level names a purpose: finish quickly, optimize each module, produce
-- fast code, or produce a small executable. 'optimizationPlan' expands
-- it once, at the command line, into the scope of the build and the
-- System FC passes to run. Everything downstream reads the plan, and no
-- pass reads the level. The level itself still reaches Clang, and it is
-- part of the identity of an installed package.
--
-- See @docs/optimization.md@.
module Aihc.Cli.OptimizationPlan
  ( OptimizationPlan (..),
    optimizationPlan,
  )
where

import Aihc.Fc qualified as Fc
import Aihc.Native (OptimizationLevel (..))

data OptimizationPlan = OptimizationPlan
  { -- | Whether the modules of the program stop at System FC, to be
    -- merged and compiled once as one program.
    planWholeProgram :: !Bool,
    -- | The System FC passes, in order. They run on each module at a
    -- per-module scope, and on the merged program at a whole-program
    -- scope.
    planPasses :: ![Fc.Pass]
  }
  deriving (Eq, Show)

-- | The plan of a level, under the @--lto@ flag.
--
-- @-O0@ runs nothing. @-Os@ runs the shrinking inliner. @-O1@ and @-O2@
-- run the shrinking inliner and then the growing one, so that @-Os@ is
-- a prefix of @-O2@ and the growing phase starts from the program that
-- @-Os@ would have produced. Eta expansion runs before the inliner, so
-- that a value it turns into a function is a saturated call for the
-- inliner to take, and again after it, because a call of a class method
-- hides the arity of the method until the selection is inlined. One
-- simplifying walk then reduces the applications and casts the second
-- expansion leaves behind.
--
-- @-O2@ and @-Os@ compile the whole program; @--lto@ asks for the same
-- scope at the other levels without changing their passes.
optimizationPlan :: Bool -> OptimizationLevel -> OptimizationPlan
optimizationPlan lto level =
  case level of
    O0 -> OptimizationPlan {planWholeProgram = lto, planPasses = []}
    O1 -> OptimizationPlan {planWholeProgram = lto, planPasses = shrink <> grow}
    O2 -> OptimizationPlan {planWholeProgram = True, planPasses = shrink <> grow}
    Os -> OptimizationPlan {planWholeProgram = True, planPasses = shrink <> finish}
  where
    -- The phases count down as GHC's do: the shrinking inliner is phase
    -- 2, the growing one phase 1 and the final walk phase 0, so a rule
    -- with a phase control fires where its author expects.
    shrink = [Fc.PassEtaExpand, Fc.PassInline Fc.shrinkPolicy rounds 2]
    grow = [Fc.PassInline Fc.growPolicy rounds 1] <> finish
    finish = [Fc.PassEtaExpand, Fc.PassSimplify 0]
    rounds = 4
