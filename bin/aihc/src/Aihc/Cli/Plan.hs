-- | The @plan@ command.
--
-- It solves the dependency plan of a package as @build@ does, and prints
-- the plan instead of a build. It does not read or write @aihc.lock@. Each line is one package, and a package
-- comes after all of its dependencies:
--
-- > NAME<TAB>VERSION<TAB>SOURCE<TAB>DEPENDENCIES
--
-- The source is @core@ for a core library, @hackage:REVISION@ for a Hackage
-- release at a cabal file revision, or @local:PATH@ for a directory. The
-- dependencies are a comma-separated list of names, or @-@ for none.
module Aihc.Cli.Plan
  ( PlanRow (..),
    PlanRowSource (..),
    planPackageRows,
    renderPlanRow,
    runPlan,
  )
where

import Aihc.Cli.Install (cabalPlatformForTarget, installTargetRoot, planRequestFor)
import Aihc.Cli.Options (PlanCommandOptions (..), PlanOptions (..), defaultPlanOptions)
import Aihc.Hackage.IndexCache (defaultIndexOptions, newHackageIndex)
import Aihc.PackagePlan (PlanRequest (..), PlannedPackages (..), planPackages)
import Aihc.PackagePlan.Solver (Assignment (..), CandidateSource (..))
import Control.Monad (when)
import Data.List (intercalate, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Distribution.Package (PackageName, unPackageName)
import Distribution.Pretty (prettyShow)

-- | One package of a plan.
data PlanRow = PlanRow
  { planRowName :: !String,
    planRowVersion :: !String,
    planRowSource :: !PlanRowSource,
    -- | The packages of the plan it depends on, by name, in name order.
    planRowDependencies :: ![String]
  }
  deriving (Eq, Show)

data PlanRowSource
  = PlanRowCore
  | -- | A Hackage release at a cabal file revision.
    PlanRowHackage !Int
  | PlanRowLocal !FilePath
  deriving (Eq, Show)

runPlan :: PlanCommandOptions -> IO ()
runPlan options = planPackageRows options >>= mapM_ (putStrLn . renderPlanRow)

-- | Solve the plan of the package the options name, and give its packages
-- in dependency order. Packages that are ready at the same time come in
-- name order, so the order does not change from one run to the next.
planPackageRows :: PlanCommandOptions -> IO [PlanRow]
planPackageRows options = do
  hackageIndex <- newHackageIndex defaultIndexOptions
  (root, _, _) <- installTargetRoot (planCommandInput options)
  let verbose message = when (planCommandVerbose options) (putStrLn message)
      executables = planCommandExecutables options
  request <-
    planRequestFor
      hackageIndex
      defaultPlanOptions {planConstraints = planCommandConstraints options}
      (cabalPlatformForTarget (planCommandTarget options))
      (maybe [] pure (planCommandWorkspace options))
      -- The command only reports the plan, so it keeps no lock.
      Nothing
      verbose
  planned <-
    planPackages
      request
        { requestRoots = [root],
          requestExecutables = if null executables then Nothing else Just (nub executables),
          -- A build tool the host cannot run stops the build of one
          -- package, not the plan.
          requestCheckBuildTools = False
        }
  pure (dependencyOrder (map (uncurry planRow) (Map.toList (plannedSolution planned))))

planRow :: PackageName -> Assignment -> PlanRow
planRow name assignment =
  PlanRow
    { planRowName = unPackageName name,
      planRowVersion = prettyShow (assignmentVersion assignment),
      planRowSource =
        case assignmentSource assignment of
          CandidateCore _ -> PlanRowCore
          CandidateHackage -> PlanRowHackage (assignmentRevision assignment)
          CandidateLocal path -> PlanRowLocal path,
      planRowDependencies = map unPackageName (Map.keys (assignmentDependencies assignment))
    }

-- | Kahn's algorithm. The solver gives a plan without cycles. A row whose
-- dependencies never become ready still comes out, at the end.
dependencyOrder :: [PlanRow] -> [PlanRow]
dependencyOrder = go Set.empty . sortOn planRowName
  where
    go done rows =
      case break (all (`Set.member` done) . planRowDependencies) rows of
        (before, ready : after) -> ready : go (Set.insert (planRowName ready) done) (before <> after)
        (_, []) -> rows

renderPlanRow :: PlanRow -> String
renderPlanRow row =
  intercalate
    "\t"
    [ planRowName row,
      planRowVersion row,
      renderSource (planRowSource row),
      if null (planRowDependencies row) then "-" else intercalate "," (planRowDependencies row)
    ]
  where
    renderSource source =
      case source of
        PlanRowCore -> "core"
        PlanRowHackage revision -> "hackage:" <> show revision
        PlanRowLocal path -> "local:" <> path
