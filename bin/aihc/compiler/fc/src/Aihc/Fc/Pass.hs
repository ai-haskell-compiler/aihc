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
import Aihc.Fc.ConstantLift (liftConstants)
import Aihc.Fc.Demand (DemandReport (..), DemandRewrites (..), demandProgram)
import Aihc.Fc.Inline (InlineConfig (..), InlinePolicy (..), InlineReport (..), inlineProgram)
import Aihc.Fc.Name (Name)
import Aihc.Fc.Simplify (SimplifyReport (..), simplifyProgram)
import Aihc.Fc.Size (programSize)
import Aihc.Fc.Syntax (Program)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T

data Pass
  = -- | Arity analysis, then eta expansion of every top-level value to
    -- the arity it finds.
    PassEtaExpand
  | -- | The inliner under a policy, for at most the given number of
    -- rounds, in a phase.
    PassInline !InlinePolicy !Int !Int
  | -- | One walk over every body with the local rewrites and no copy of
    -- any callee, in a phase.
    PassSimplify !Int
  | -- | Share closed constructor expressions through private constants.
    PassLiftConstants
  | -- | Demand analysis, then a case for every strict let, and with
    -- 'StrictLetsAndArguments' for every strict argument of a saturated
    -- call. @Aihc.Fc.Demand@.
    PassDemand !DemandRewrites
  deriving (Eq, Show)

-- | The phase a pass runs in. Phases count down as GHC's do, from 2 to
-- 0, and a rewrite rule names the phases it fires in.
passPhase :: Pass -> Maybe Int
passPhase pass =
  case pass of
    PassLiftConstants -> Nothing
    PassEtaExpand -> Nothing
    PassDemand _ -> Nothing
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
    PassLiftConstants -> "lift constants"
    PassDemand StrictLetsOnly -> "demand"
    PassDemand StrictLetsAndArguments -> "demand arguments"
    PassEtaExpand -> "eta expand"
    PassInline policy _ phase -> "inline " <> policyName policy <> " [" <> T.pack (show phase) <> "]"
    PassSimplify phase -> "simplify [" <> T.pack (show phase) <> "]"

-- | Run one pass. The roots are the values the program must keep, or
-- 'Nothing' to keep every public value.
runPass :: Maybe [Name] -> Pass -> Program -> (Program, PassReport)
runPass roots pass program =
  case pass of
    PassLiftConstants ->
      let (lifted, constants, sites) = liftConstants program
       in ( lifted,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize program,
                reportAfter = programSize lifted,
                reportDetail = count constants "constants" <> ", " <> count sites "sites"
              }
          )
    PassDemand rewrites ->
      let (rewritten, report) = demandProgram rewrites program
       in ( rewritten,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize program,
                reportAfter = programSize rewritten,
                reportDetail = count (reportStrictValues report) "strict values" <> ", " <> count (reportStrictLets report) "strict lets" <> ", " <> count (reportStrictArguments report) "strict arguments"
              }
          )
    PassEtaExpand ->
      let (expanded, report) = etaExpandProgram program
       in ( expanded,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize program,
                reportAfter = programSize expanded,
                reportDetail = count (reportExpandedValues report) "values" <> ", " <> count (reportExpandedLocals report) "locals" <> ", " <> count (reportAddedLambdas report) "lambdas added"
              }
          )
    PassInline policy rounds phase ->
      let config = InlineConfig {inlinePolicy = policy, inlineRoots = roots, inlineRounds = rounds, inlinePhase = phase}
          (inlined, report) = inlineProgram config program
       in ( inlined,
            PassReport
              { reportPass = passName pass,
                reportBefore = reportSizeBefore report,
                reportAfter = reportSizeAfter report,
                reportDetail = count (reportInlinedSites report) "sites" <> ", " <> count (reportDroppedValues report) "values dropped" <> ", " <> count (reportRulesFired report) "rules fired"
              }
          )
    PassSimplify phase ->
      let (simplified, report) = simplifyProgram phase program
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
runPasses :: Maybe [Name] -> [Pass] -> Program -> (Program, [PassReport])
runPasses roots passes program0 =
  let (final, reports) = List.foldl' step (program0, []) passes
   in (final, reverse reports)
  where
    step (program, reports) pass =
      let (program', report) = runPass roots pass program
       in (program', report : reports)
