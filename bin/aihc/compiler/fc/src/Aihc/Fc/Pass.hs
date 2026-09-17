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
    runPass,
    runPasses,
  )
where

import Aihc.Fc.Arity (EtaReport (..), etaExpandProgram)
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
    -- rounds.
    PassInline !InlinePolicy !Int
  | -- | One walk over every body with the local rewrites and no copy of
    -- any callee.
    PassSimplify
  deriving (Eq, Show)

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
    PassEtaExpand -> "eta expand"
    PassInline policy _ -> "inline " <> policyName policy
    PassSimplify -> "simplify"

-- | Run one pass. The roots are the values the program must keep, or
-- 'Nothing' to keep every public value.
runPass :: Maybe [Name] -> Pass -> Program -> (Program, PassReport)
runPass roots pass program =
  case pass of
    PassEtaExpand ->
      let (expanded, report) = etaExpandProgram program
       in ( expanded,
            PassReport
              { reportPass = passName pass,
                reportBefore = programSize program,
                reportAfter = programSize expanded,
                reportDetail = count (reportExpandedValues report) "values" <> ", " <> count (reportAddedLambdas report) "lambdas added"
              }
          )
    PassInline policy rounds ->
      let config = InlineConfig {inlinePolicy = policy, inlineRoots = roots, inlineRounds = rounds}
          (inlined, report) = inlineProgram config program
       in ( inlined,
            PassReport
              { reportPass = passName pass,
                reportBefore = reportSizeBefore report,
                reportAfter = reportSizeAfter report,
                reportDetail = count (reportInlinedSites report) "sites" <> ", " <> count (reportDroppedValues report) "values dropped"
              }
          )
    PassSimplify ->
      let (simplified, report) = simplifyProgram program
       in ( simplified,
            PassReport
              { reportPass = passName pass,
                reportBefore = simplifySizeBefore report,
                reportAfter = simplifySizeAfter report,
                reportDetail = ""
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
