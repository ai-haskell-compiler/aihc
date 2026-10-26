{-# LANGUAGE OverloadedStrings #-}

-- | The System FC passes, as data.
--
-- A plan of optimization is a list of passes. Each pass is a pure
-- function from a program to a program and a report, and 'runPass' runs
-- one with the roots of the scope it runs in: the public values of a
-- module, or the entry of a whole program. No pass reads the
-- optimization level; the level chooses the list, once, at the command
-- line.
module Aihc.Fc.Pass
  ( Pass (..),
    PassReport (..),
    passName,
    passPhase,
    runPass,
    runPasses,
  )
where

import Aihc.Fc.Arity (EtaReport (..), etaExpandProgram)
import Aihc.Fc.CallPattern (CallPatternReport (..), callPatternProgram)
import Aihc.Fc.Canonical (canonicalForeignCalls)
import Aihc.Fc.ConstantLift (liftConstants)
import Aihc.Fc.Cse (CseReport (..), cseProgram)
import Aihc.Fc.Demand (DemandReport (..), DemandRewrites (..), demandProgram)
import Aihc.Fc.FullLaziness (fullLazinessProgram)
import Aihc.Fc.Inline (InlineConfig (..), InlinePolicy (..), InlineReport (..), inlineProgram)
import Aihc.Fc.ListFusion (listFusionProgram)
import Aihc.Fc.Name (Name)
import Aihc.Fc.Simplify (SimplifyReport (..), simplifyProgram)
import Aihc.Fc.Size (programSize)
import Aihc.Fc.Specialise (SpecialiseReport (..), specialiseProgram, specialiseTopProgram)
import Aihc.Fc.StateToken (StateTokenReport (..), stateTokenProgram)
import Aihc.Fc.SuperSpecialise (SuperReport (..), superSpecialiseProgram)
import Aihc.Fc.Syntax (Program)
import Aihc.Fc.Unroll (UnrollReport (..), unrollProgram)
import Aihc.Fc.WorkerWrapper (SplitScope (..), WorkerWrapperReport (..), workerWrapperProgram)
import Aihc.Resolve (PackageId)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T

data Pass
  = -- | Arity analysis, then eta expansion of every top-level value to
    -- the arity it finds.
    PassEtaExpand
  | -- | Express local list producers with foldr/build.
    PassListFusion
  | -- | The inliner under a policy, for at most the given number of
    -- rounds, in a phase.
    PassInline !InlinePolicy !Int !Int
  | -- | One walk over every body with the local rewrites and no copy of
    -- any callee, in a phase.
    PassSimplify !Int
  | -- | Share closed constructor expressions through private constants.
    PassLiftConstants
  | -- | Share closed lifted computations across function calls.
    PassFullLaziness
  | -- | Copy the body of a small local loop at each call whose literal
    -- arguments decide its recursion, until no call is left.
    -- @Aihc.Fc.Unroll@.
    PassUnroll
  | -- | Remove a read of a 'MutVar#' that an earlier read makes redundant,
    -- and a @touch#@ that a later @touch#@ of the same object makes
    -- redundant. @Aihc.Fc.StateToken@.
    PassStateTokens
  | -- | Use a let in scope for an equal pure expression, and share a safe
    -- primitive call that occurs more than once. @Aihc.Fc.Cse@.
    PassCse
  | -- | Demand analysis, then a case for every strict let, and with
    -- 'StrictLetsAndArguments' for every strict argument of a saturated
    -- call. @Aihc.Fc.Demand@.
    PassDemand !DemandRewrites
  | -- | Split each function in the scope that takes apart a strict
    -- parameter of a type with one constructor into a worker that takes
    -- the fields and an @INLINE@ wrapper. @Aihc.Fc.WorkerWrapper@.
    PassWorkerWrapper !SplitScope
  | -- | Copy each local recursive function whose calls give a constant
    -- dictionary, with the dictionary in place of the parameter.
    -- @Aihc.Fc.Specialise@.
    PassSpecialise
  | -- | Specialize known dictionaries and function arguments within a code budget.
    PassSuperSpecialise
  | -- | Specialize top-level dictionary calls within a code growth limit.
    PassTopSpecialise
  | -- | Copy each local loop whose calls give a constructor in a position,
    -- with the fields in place of the parameter, in rounds with a
    -- simplifying walk in a phase. The walk copies a local function at a
    -- reducing site within the site limit. @Aihc.Fc.CallPattern@.
    PassCallPatterns !Int !Int
  deriving (Eq, Show)

-- | The phase a pass runs in. Phases count down as GHC's do, from 2 to
-- 0, and a rewrite rule names the phases it fires in.
passPhase :: Pass -> Maybe Int
passPhase pass =
  case pass of
    PassListFusion -> Nothing
    PassFullLaziness -> Nothing
    PassLiftConstants -> Nothing
    PassUnroll -> Nothing
    PassStateTokens -> Nothing
    PassCse -> Nothing
    PassEtaExpand -> Nothing
    PassDemand _ -> Nothing
    PassWorkerWrapper _ -> Nothing
    PassSuperSpecialise -> Nothing
    PassTopSpecialise -> Nothing
    PassSpecialise -> Nothing
    PassCallPatterns phase _ -> Just phase
    PassInline _ _ phase -> Just phase
    PassSimplify phase -> Just phase

-- | What one pass did to a program.
data PassReport = PassReport
  { reportPass :: !Text,
    reportBefore :: !Int,
    reportAfter :: !Int,
    -- | What the pass counts besides the size, for a log line.
    reportDetail :: !Text
  }
  deriving (Eq, Show)

passName :: Pass -> Text
passName pass =
  case pass of
    PassListFusion -> "list producers"
    PassFullLaziness -> "full laziness"
    PassLiftConstants -> "lift constants"
    PassUnroll -> "unroll"
    PassStateTokens -> "state tokens"
    PassCse -> "cse"
    PassDemand StrictLetsOnly -> "demand"
    PassDemand StrictLetsAndArguments -> "demand arguments"
    PassWorkerWrapper SplitAllFunctions -> "worker/wrapper"
    PassWorkerWrapper SplitLateFunctions -> "worker/wrapper late"
    PassSuperSpecialise -> "super-specialise"
    PassTopSpecialise -> "specialise top"
    PassSpecialise -> "specialise"
    PassCallPatterns phase _ -> "call patterns [" <> T.pack (show phase) <> "]"
    PassEtaExpand -> "eta expand"
    PassInline policy _ phase -> "inline " <> policyName policy <> " [" <> T.pack (show phase) <> "]"
    PassSimplify phase -> "simplify [" <> T.pack (show phase) <> "]"

-- | Run one pass. The roots are the values the program must keep, or
-- 'Nothing' to keep every public value.
runPass :: PackageId -> Maybe [Name] -> Pass -> Program -> (Program, PassReport)
runPass primPackage roots pass program =
  case pass of
    PassListFusion ->
      let (rewritten, sites) = listFusionProgram primPackage program
       in (rewritten, PassReport (passName pass) (programSize primPackage program) (programSize primPackage rewritten) (count sites "producers"))
    PassFullLaziness ->
      let (floated, sites) = fullLazinessProgram primPackage program
       in (floated, PassReport (passName pass) (programSize primPackage program) (programSize primPackage floated) (count sites "floats"))
    PassLiftConstants ->
      let (lifted, constants, sites) = liftConstants primPackage program
       in ( lifted,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage lifted,
                reportDetail = count constants "constants" <> ", " <> count sites "sites"
              }
          )
    PassUnroll ->
      let (unrolled, report) = unrollProgram primPackage program
       in ( unrolled,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage unrolled,
                reportDetail = count (reportUnrolledLoops report) "loops" <> ", " <> count (reportUnrollCopies report) "copies"
              }
          )
    PassStateTokens ->
      let (rewritten, report) = stateTokenProgram primPackage program
       in ( rewritten,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage rewritten,
                reportDetail = count (reportReusedReads report) "reads reused" <> ", " <> count (reportMergedTouches report) "touches merged"
              }
          )
    PassCse ->
      let (rewritten, report) = cseProgram primPackage program
       in ( rewritten,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage rewritten,
                reportDetail = count (reportReusedExpressions report) "expressions reused" <> ", " <> count (reportSharedCalls report) "calls shared"
              }
          )
    PassDemand rewrites ->
      let (rewritten, report) = demandProgram primPackage rewrites program
       in ( rewritten,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage rewritten,
                reportDetail = count (reportStrictValues report) "strict values" <> ", " <> count (reportStrictLets report) "strict lets" <> ", " <> count (reportStrictArguments report) "strict arguments"
              }
          )
    PassWorkerWrapper scope ->
      let (split, report) = workerWrapperProgram primPackage scope program
       in ( split,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage split,
                reportDetail = count (reportWorkers report) "workers" <> ", " <> count (reportUnboxedParameters report) "unboxed parameters" <> ", " <> count (reportConstructedResults report) "constructed results"
              }
          )
    PassSuperSpecialise ->
      let (specialised, report) = superSpecialiseProgram program
       in ( specialised,
            PassReport
              (passName pass)
              (programSize primPackage program)
              (programSize primPackage specialised)
              (count (superCopies report) "copies" <> ", " <> count (superCalls report) "calls" <> ", " <> count (superLimits report) "limits")
          )
    PassSpecialise ->
      let (specialised, report) = specialiseProgram program
       in ( specialised,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage specialised,
                reportDetail = count (reportSpecialisedBindings report) "bindings" <> ", " <> count (reportCopies report) "copies" <> ", " <> count (reportRewrittenCalls report) "calls"
              }
          )
    PassTopSpecialise ->
      let (specialised, report) = specialiseTopProgram primPackage program
       in ( specialised,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage specialised,
                reportDetail = count (reportSpecialisedBindings report) "bindings" <> ", " <> count (reportCopies report) "copies" <> ", " <> count (reportRewrittenCalls report) "calls"
              }
          )
    PassCallPatterns phase siteLimit ->
      let (specialised, report) = callPatternProgram primPackage phase siteLimit program
       in ( specialised,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage specialised,
                reportDetail = count (reportCallPatternLoops report) "loops" <> ", " <> count (reportCallPatternCalls report) "calls" <> ", " <> count (reportCallPatternRounds report) "rounds"
              }
          )
    PassEtaExpand ->
      let (expanded, report) = etaExpandProgram primPackage roots program
       in ( expanded,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize primPackage program,
                reportAfter = programSize primPackage expanded,
                reportDetail = count (reportExpandedValues report) "values" <> ", " <> count (reportExpandedLocals report) "locals" <> ", " <> count (reportAddedLambdas report) "lambdas added"
              }
          )
    PassInline policy rounds phase ->
      let config = InlineConfig {inlinePolicy = policy, inlineRoots = roots, inlineRounds = rounds, inlinePhase = phase}
          (inlined, report) = inlineProgram primPackage config program
       in ( inlined,
            PassReport
              { reportPass = passName pass,
                reportBefore = reportSizeBefore report,
                reportAfter = reportSizeAfter report,
                reportDetail = count (reportInlinedSites report) "sites" <> ", " <> count (reportDroppedValues report) "values dropped" <> ", " <> count (reportRulesFired report) "rules fired"
              }
          )
    PassSimplify phase ->
      let (simplified, report) = simplifyProgram primPackage phase 0 program
       in ( simplified,
            PassReport
              { reportPass = passName pass,
                reportBefore = simplifySizeBefore report,
                reportAfter = simplifySizeAfter report,
                reportDetail = count (simplifyRulesFired report) "rules fired"
              }
          )
  where
    count n what = T.pack (show n) <> " " <> what

-- | Run the passes in order, and collect the report of each.
runPasses :: PackageId -> Maybe [Name] -> [Pass] -> Program -> (Program, [PassReport])
runPasses primPackage roots passes program0 =
  let (final, reports) = List.foldl' step (program0, []) passes
   in (canonicalForeignCalls final, reverse reports)
  where
    step (program, reports) pass =
      let (program', report) = runPass primPackage roots pass program
       in (program', report : reports)
