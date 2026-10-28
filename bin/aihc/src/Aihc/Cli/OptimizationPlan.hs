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
    -- | The System FC passes of each module, in order. They run on one
    -- module at a time, in the order the modules import each other, with
    -- the optimizer interfaces of the modules each one imports.
    planModulePasses :: ![Fc.Pass],
    -- | The System FC passes of the whole program, in order. They run
    -- once, on the merged program of a whole-program plan, after the
    -- passes of each module.
    planProgramPasses :: ![Fc.Pass],
    -- | Whether the whole program gets the heap points-to analysis of GRIN
    -- and the rewrites that its result permits.
    planGrinPointsTo :: !Bool,
    -- | Whether each module publishes its optimizer interface, and reads
    -- the interfaces of the modules it imports. A plan whose module
    -- passes copy nothing has no use for them. See "Aihc.Fc.Interface".
    planModuleInterfaces :: !Bool
  }
  deriving (Eq, Show)

-- | The plan of a level, under the @--lto@ flag.
--
-- A plan has two layers. The module layer runs on one module at a time,
-- in the order the modules import each other, and each module reads the
-- optimizer interfaces of the modules it imports. The program layer runs
-- once, on the merged program of a whole-program plan, after the module
-- layer of every module.
--
-- @-O2@ is @-O1@ and one layer above it: the two share their module
-- layer, and only @-O2@ has a program layer. The program layer holds the
-- passes that need the whole program, and the passes that clean up after
-- them. The module layer already made the copies that the growing
-- inliner makes, so the program layer does not run it again.
--
-- @-O0@ runs nothing. @-Os@ runs the shrinking inliner on the whole
-- program. @-O1@ and @-O2@ share their initial shrinking passes with
-- @-Os@.
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
-- jump, so it serves a small executable as well as a fast one.
optimizationPlan :: Bool -> OptimizationLevel -> OptimizationPlan
optimizationPlan lto level =
  withInterfaces $ case level of
    O0 -> plan {planWholeProgram = lto}
    -- With @--lto@ the modules of @-O1@ merge, and the merged program
    -- gets the same passes, as it did before the plan had two layers.
    O1 -> plan {planWholeProgram = lto, planModulePasses = modulePasses, planProgramPasses = if lto then modulePasses else []}
    O2 -> plan {planWholeProgram = True, planModulePasses = modulePasses, planProgramPasses = programPasses, planGrinPointsTo = True}
    Os -> plan {planWholeProgram = True, planProgramPasses = shrink <> finish, planGrinPointsTo = True}
  where
    plan =
      OptimizationPlan
        { planWholeProgram = False,
          planModulePasses = [],
          planProgramPasses = [],
          planGrinPointsTo = False,
          planModuleInterfaces = False
        }
    -- A plan whose module layer is empty copies nothing from an
    -- interface, so it reads none and writes none.
    withInterfaces current = current {planModuleInterfaces = not (null (planModulePasses current))}
    -- The module layer of @-O1@ and @-O2@.
    modulePasses = shrink <> specialise <> split <> grow
    -- The program layer of @-O2@: the module layer again, on the whole
    -- program, and super-specialisation, which makes copies for known
    -- dictionaries and function arguments.
    --
    -- Each pass of the module layer sees more in the whole program than
    -- in one module: the interface of a module offers the bodies within
    -- its limits, and the merged program holds every body. The layer thus
    -- runs again rather than only once per module, and the module layer
    -- below it leaves it less to do.
    programPasses = superSpecialisation modulePasses
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

-- | The super-specialisation of the program layer of @-O2@. It runs after
-- the first shrinking inliner and before the first demand analysis. The
-- passes at the end clean up after it: the specialised copies expose new
-- strict calls, so demand analysis runs again, and sharing is restored
-- after it.
superSpecialisation :: [Fc.Pass] -> [Fc.Pass]
superSpecialisation passes = insert passes <> [Fc.PassDemand Fc.StrictLetsAndArguments, Fc.PassSimplify 0, Fc.PassCse, Fc.PassFullLaziness, Fc.PassLiftConstants]
  where
    insert (pass@(Fc.PassDemand _) : rest) = Fc.PassSuperSpecialise : Fc.PassSpecialise : Fc.PassInline Fc.shrinkPolicy 2 2 : pass : rest
    insert (pass : rest) = pass : insert rest
    insert [] = []
