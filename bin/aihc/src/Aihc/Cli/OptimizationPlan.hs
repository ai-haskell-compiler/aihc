-- | What an optimization level asks for, as a plan.
--
-- A level names a purpose: finish quickly, optimize each module, produce
-- fast code, or produce a small executable. 'optimizationPlan' expands
-- it once, at the command line, into the scope of the build, the System FC
-- passes to run, and the GRIN analyses to run. Everything downstream reads the plan, and no
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
    planPasses :: ![Fc.Pass],
    -- | Whether the whole program gets the heap points-to analysis of GRIN
    -- and the rewrites that its result permits.
    planGrinPointsTo :: !Bool,
    -- | Whether each module publishes its optimizer interface, and reads
    -- the interfaces of the modules it imports. Only a plan that
    -- optimizes each module on its own has a use for them: a
    -- whole-program plan already sees every module, and a plan without
    -- passes copies nothing. See "Aihc.Fc.Interface".
    planModuleInterfaces :: !Bool
  }
  deriving (Eq, Show)

-- | The plan of a level, under the @--lto@ flag.
--
-- @-O0@ runs nothing. @-Os@ runs the shrinking inliner. @-O1@ and @-O2@
-- share their initial shrinking passes with @-Os@.
-- The growing inliner exposes calls before specialization in phase 2.
-- Eta expansion runs before the inliner, so
-- that a value it turns into a function is a saturated call for the
-- inliner to take, and again after it, because a call of a class method
-- hides the arity of the method until the selection is inlined. One
-- simplifying walk then reduces the applications and casts the second
-- expansion leaves behind. The state token pass and the common
-- subexpression pass then remove redundant reads, @touch#@ calls and
-- calculations, and constant lifting shares closed constructors.
--
-- @-O2@ and @-Os@ compile the whole program; @--lto@ asks for the same
-- scope at the other levels without changing their passes. @-O2@ and @-Os@
-- also run the heap points-to analysis of GRIN on the whole program. Each of
-- its rewrites removes code or replaces a runtime dispatch with a direct
-- jump, so it serves a small executable as well as a fast one. Only @-O2@
-- adds 'superSpecialisation', which makes copies of functions for known
-- dictionaries and function arguments.
optimizationPlan :: Bool -> OptimizationLevel -> OptimizationPlan
optimizationPlan lto level =
  withInterfaces $ case level of
    O0 -> plan {planWholeProgram = lto, planPasses = []}
    O1 -> plan {planWholeProgram = lto, planPasses = shrink <> specialise <> split <> grow}
    O2 -> plan {planWholeProgram = True, planPasses = superSpecialisation (shrink <> specialise <> split <> grow), planGrinPointsTo = True}
    Os -> plan {planWholeProgram = True, planPasses = shrink <> finish, planGrinPointsTo = True}
  where
    plan = OptimizationPlan {planWholeProgram = False, planPasses = [], planGrinPointsTo = False, planModuleInterfaces = False}
    -- The interfaces serve the plans that optimize each module on its
    -- own. A whole-program plan sees every module already, and a plan
    -- without passes copies nothing.
    withInterfaces current = current {planModuleInterfaces = not (planWholeProgram current) && not (null (planPasses current))}
    -- The phases count down from 2 to 0.
    -- Phase 2 keeps producer rules active until specialization finishes.
    shrink = [Fc.PassListFusion, Fc.PassEtaExpand, Fc.PassSpecialise, Fc.PassInline Fc.shrinkPolicy rounds 2, Fc.PassDemand Fc.StrictLetsOnly]
    -- First expose dictionary calls while producer rules remain active.
    -- Specialize before the final fusion and worker/wrapper passes.
    specialise = [Fc.PassInline Fc.growPolicy rounds 2, Fc.PassTopSpecialise, Fc.PassInline Fc.shrinkPolicy rounds 2, Fc.PassDemand Fc.StrictLetsOnly]
    -- The wrappers are small INLINE values, which the growing inliner
    -- copies at each call. The shrinking inliner would keep them as calls,
    -- so -Os does not split. A worker body is new, and the growing
    -- inliner does not walk a body that calls no candidate, so a
    -- simplifying walk in the phase of the growing inliner follows.
    split = [Fc.PassWorkerWrapper Fc.SplitAllFunctions, Fc.PassSimplify 1]
    -- The growing inliner makes new local loops, such as a fused take of
    -- an iterate, whose counter is still boxed. The eta expansion gives
    -- such a loop all its lambdas, the local split then takes the box
    -- off, and the simplifying walk reduces the copies of the wrapper at
    -- the calls. The late split also splits each top-level INLINE value
    -- that the growing inliner did not copy at all its calls, because a
    -- large one is only a candidate. One round of the growing inliner in
    -- phase 0 copies its wrapper at those calls, and also copies the
    -- values whose INLINE pragma names phase 0, which no earlier pass may
    -- copy; a strict let that such a copy exposes gets its case from the
    -- demand pass that follows. This pass also evaluates strict arguments
    -- with local value references. Call-pattern specialisation then copies
    -- the loops whose calls give a constructor in a position that the
    -- worker/wrapper split left boxed, because the loop does not always
    -- evaluate it. Its simplifying walks copy a local function at a call
    -- that the copy of a loop gives a constructor, within the site limit
    -- of the growing policy.
    grow = [Fc.PassSpecialise, Fc.PassInline Fc.growPolicy rounds 1, Fc.PassEtaExpand, Fc.PassWorkerWrapper Fc.SplitLateFunctions, Fc.PassInline Fc.growPolicy 1 0, Fc.PassDemand Fc.StrictLetsAndArguments, Fc.PassUnroll, Fc.PassSimplify 0, Fc.PassCallPatterns 0 (Fc.policySiteLimit Fc.growPolicy), Fc.PassStateTokens, Fc.PassCse, Fc.PassFullLaziness, Fc.PassLiftConstants]
    finish = [Fc.PassEtaExpand, Fc.PassSimplify 0, Fc.PassStateTokens, Fc.PassCse, Fc.PassFullLaziness, Fc.PassLiftConstants]
    rounds = 4

-- | The super-specialisation of @-O2@. It runs after the first shrinking
-- inliner and before the first demand analysis. The passes at the end
-- clean up after it: the specialised copies expose new strict calls, so
-- demand analysis runs again, and sharing is restored after it.
superSpecialisation :: [Fc.Pass] -> [Fc.Pass]
superSpecialisation passes = insert passes <> [Fc.PassDemand Fc.StrictLetsAndArguments, Fc.PassSimplify 0, Fc.PassCse, Fc.PassFullLaziness, Fc.PassLiftConstants]
  where
    insert (pass@(Fc.PassDemand _) : rest) = Fc.PassSuperSpecialise : Fc.PassSpecialise : Fc.PassInline Fc.shrinkPolicy 2 2 : pass : rest
    insert (pass : rest) = pass : insert rest
    insert [] = []
