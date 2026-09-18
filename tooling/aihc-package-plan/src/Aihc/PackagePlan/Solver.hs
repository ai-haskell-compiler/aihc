-- |
-- Module      : Aihc.PackagePlan.Solver
-- Description : Choose one version and one flag assignment per package
--
-- The solver turns the @build-depends@ of a set of root packages into an
-- assignment of exactly one version and one flag assignment to every
-- package the build needs. It is a function of its inputs alone: the
-- candidates of each package and their cabal files arrive through
-- 'SolverInputs', which a test instantiates with pure maps and the compiler
-- with the Hackage index.
--
-- The search is plain chronological backtracking over goals. A goal is a
-- package name with the range its dependents demand; the goal with the
-- fewest candidates is decided first, its candidates are tried newest
-- first with deprecated versions last, and within a version the flag
-- assignments are tried default first and then in increasing number of
-- flips. A candidate whose dependency list contradicts a package already
-- assigned is rejected, and a goal with no candidates left returns to the
-- previous choice. There are no conflict sets: at the scale aihc plans this
-- is fast, and the search is deterministic, so the same inputs give the
-- same plan.
--
-- Only automatic flags that guard a @build-depends@ clause are searched.
-- Every other flag takes its default or its constraint, since no assignment
-- of it can make the plan fail.
module Aihc.PackagePlan.Solver
  ( -- * Inputs
    SolverInputs (..),
    Candidate (..),
    CandidateSource (..),
    SolverConfig (..),
    Stanzas (..),
    noStanzas,
    Constraint (..),
    Preference (..),

    -- * Outputs
    Solution,
    Assignment (..),
    SolveFailure (..),
    CandidateFailure (..),
    Rejection (..),
    Dependent (..),
    renderSolveFailure,
    solve,
    verifySolution,

    -- * Cabal file inspection
    candidateDependencies,
    searchableFlags,
    unknownBuildTools,
  )
where

import Aihc.Hackage.Cabal (BuildContext (..), collectCondTreeData, conditionEvaluatorIn, targetFlagOverrides)
import Aihc.Hackage.Preprocessor (Preprocessor, preprocessorToolName)
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, gets, modify', put)
import Data.List (intercalate, sortBy, sortOn, subsequences)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set qualified as Set
import Distribution.Package (PackageName, packageName, unPackageName)
import Distribution.PackageDescription
  ( BuildInfo,
    Executable,
    FlagName,
    Library,
    benchmarkBuildInfo,
    buildInfo,
    buildToolDepends,
    buildTools,
    buildable,
    condBenchmarks,
    condExecutables,
    condLibrary,
    condSubLibraries,
    condTestSuites,
    flagDefault,
    flagManual,
    flagName,
    libBuildInfo,
    packageDescription,
    testBuildInfo,
    unFlagName,
  )
import Distribution.Pretty (prettyShow)
import Distribution.System (Arch, OS)
import Distribution.Types.BuildInfo (targetBuildDepends)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.Flag (FlagAssignment, lookupFlagAssignment, mkFlagAssignment, unFlagAssignment)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, genPackageFlags)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, anyVersion, intersectVersionRanges, withinRange)
import Distribution.Version (simplifyVersionRange)

-- | Where a candidate's sources come from.
data CandidateSource
  = -- | A Hackage release, fetched when chosen.
    CandidateHackage
  | -- | A directory the user works in.
    CandidateLocal FilePath
  | -- | A standin under @core-libs@.
    CandidateCore FilePath
  deriving (Eq, Ord, Show)

-- | One version of one package the solver may choose.
data Candidate = Candidate
  { candidateName :: !PackageName,
    candidateVersion :: !Version,
    -- | The revision of the cabal file to read. Zero for anything but a
    -- Hackage release.
    candidateRevision :: !Int,
    -- | The maintainer's @preferred-versions@ exclude this version, so it
    -- is tried after every version they do not.
    candidateDeprecated :: !Bool,
    candidateSource :: !CandidateSource
  }
  deriving (Eq, Ord, Show)

-- | Everything the solver reads. A package with no candidates is unknown.
data SolverInputs m = SolverInputs
  { -- | The candidates of a package, in any order. The preference, when
    -- there is one, names the revision to read for the version it prefers.
    inputsCandidates :: PackageName -> Maybe Preference -> m [Candidate],
    -- | The cabal file of a candidate at its revision.
    inputsDescription :: Candidate -> m GenericPackageDescription
  }

-- | The optional components of a root package that contribute
-- dependencies.
data Stanzas = Stanzas
  { stanzasTests :: !Bool,
    stanzasBenchmarks :: !Bool
  }
  deriving (Eq, Show)

noStanzas :: Stanzas
noStanzas = Stanzas False False

-- | A restriction from the command line, or from the lock file when the
-- lock is being kept.
data Constraint
  = ConstraintVersion !PackageName !VersionRange
  | ConstraintFlag !PackageName !FlagName !Bool
  deriving (Eq, Show)

-- | What an earlier solve chose for a package, tried first so that a plan
-- that changes for one package keeps the versions of the others.
data Preference = Preference
  { preferredVersion :: !Version,
    preferredRevision :: !(Maybe Int),
    preferredFlags :: !FlagAssignment
  }
  deriving (Eq, Show)

data SolverConfig = SolverConfig
  { configPlatform :: !(OS, Arch),
    -- | Package names that stand for another package: @base@ for
    -- @aihc-base@. A dependency on the left is a dependency on the right.
    configAliases :: !(Map PackageName PackageName),
    configConstraints :: ![Constraint],
    configPreferences :: !(Map PackageName Preference),
    -- | The packages to plan, and which of their optional components
    -- contribute dependencies. A root's executables always do.
    configRoots :: !(Map PackageName Stanzas),
    -- | Further packages to plan, with the range each must satisfy. A main
    -- module built against a set of packages has these and no root.
    configGoals :: ![(PackageName, VersionRange)],
    -- | Give up after this many goals ran out of candidates.
    configMaxBacktracks :: !Int
  }

-- | What the solver decided for one package.
data Assignment = Assignment
  { assignmentVersion :: !Version,
    assignmentRevision :: !Int,
    -- | The flags the solver searched or a constraint fixed. Every other
    -- flag takes its default.
    assignmentFlags :: !FlagAssignment,
    assignmentSource :: !CandidateSource,
    -- | The packages it depends on, after aliasing, with the range each
    -- must satisfy.
    assignmentDependencies :: !(Map PackageName VersionRange)
  }
  deriving (Eq, Show)

type Solution = Map PackageName Assignment

-- | Who demanded a range of a package.
data Dependent
  = DependentRoot
  | DependentConstraint
  | DependentPackage !PackageName !Version !FlagAssignment
  deriving (Eq, Show)

-- | Why a candidate was not chosen.
data Rejection
  = -- | The candidate needs a package in a range that the version already
    -- assigned to it does not satisfy.
    RejectedDependency !PackageName !VersionRange !Version
  | -- | The candidate was assigned, and the search under it failed.
    RejectedSubtree !SolveFailure
  deriving (Eq, Show)

data CandidateFailure = CandidateFailure
  { failedCandidate :: !Candidate,
    failedFlags :: !FlagAssignment,
    failedRejection :: !Rejection
  }
  deriving (Eq, Show)

data SolveFailure
  = -- | No candidate of the goal is known at all.
    UnknownPackage !PackageName ![(Dependent, VersionRange)]
  | -- | Every candidate of the goal was rejected. The versions outside the
    -- goal's range are listed newest first; the candidates inside it each
    -- carry their rejection.
    NoCandidates !PackageName !VersionRange ![(Dependent, VersionRange)] ![Version] ![CandidateFailure]
  | -- | The search stopped at the backtrack limit; the failure is the last
    -- one seen.
    BacktrackLimit !Int !SolveFailure
  deriving (Eq, Show)

-- | The failure as a flat log a user can act on: the package that ran out
-- of candidates, who demanded what of it, and what was wrong with each
-- candidate.
renderSolveFailure :: SolveFailure -> String
renderSolveFailure = intercalate "\n" . render
  where
    render failure =
      case failure of
        BacktrackLimit limit inner ->
          ("Gave up after " <> show limit <> " backtracks; the last failure was:") : render inner
        -- A goal with one candidate that was assigned and failed below it
        -- adds nothing the inner failure does not say, so the log starts
        -- where the choice was.
        NoCandidates _ _ _ _ [CandidateFailure _ _ (RejectedSubtree inner)] -> render inner
        UnknownPackage name dependents ->
          ["Unknown package " <> unPackageName name <> ", needed by " <> renderDependents dependents]
        NoCandidates name range dependents outside failures ->
          ( if range == anyVersion
              then "Every version of " <> unPackageName name <> " was rejected; it is needed by " <> renderDependents dependents
              else "No version of " <> unPackageName name <> " satisfies " <> prettyShow range <> ", needed by " <> renderDependents dependents
          )
            : [ "  versions outside the range: " <> intercalate ", " (map prettyShow (take listedVersions outside)) <> more
              | not (null outside),
                let more = if length outside > listedVersions then ", and " <> show (length outside - listedVersions) <> " more" else ""
              ]
              <> concatMap renderCandidate failures

    renderCandidate (CandidateFailure candidate flags rejection) =
      let label = "  " <> renderCandidateName candidate flags
       in case rejection of
            RejectedDependency dependency range assigned ->
              [label <> " needs " <> unPackageName dependency <> " " <> prettyShow range <> ", but " <> unPackageName dependency <> "-" <> prettyShow assigned <> " is chosen"]
            RejectedSubtree inner ->
              (label <> " was tried, and then:") : map ("    " <>) (render inner)

    renderDependents dependents =
      intercalate ", " [renderDependent dependent <> rangeSuffix range | (dependent, range) <- dependents]
    rangeSuffix range
      | range == anyVersion = ""
      | otherwise = " (" <> prettyShow range <> ")"
    renderDependent dependent =
      case dependent of
        DependentRoot -> "the root"
        DependentConstraint -> "a constraint"
        DependentPackage name version flags -> renderCandidateName (Candidate name version 0 False CandidateHackage) flags

    renderCandidateName candidate flags =
      unPackageName (candidateName candidate)
        <> "-"
        <> prettyShow (candidateVersion candidate)
        <> concat [" " <> (if value then "+" else "-") <> unFlagName flag | (flag, value) <- unFlagAssignment flags]

    listedVersions = 8 :: Int

-- | A pending goal: the range its dependents agree on, and who they are.
data Goal = Goal
  { goalRange :: !VersionRange,
    goalDependents :: ![(Dependent, VersionRange)]
  }

data Search = Search
  { searchAssigned :: !(Map PackageName Assignment),
    searchGoals :: !(Map PackageName Goal)
  }

-- | What the search remembers across backtracking: the candidates and
-- cabal files it already fetched, and how often it backtracked.
data Memo = Memo
  { memoCandidates :: !(Map PackageName [Candidate]),
    memoDescriptions :: !(Map (PackageName, Version, Int, CandidateSource) GenericPackageDescription),
    memoBacktracks :: !Int
  }

type Solve m = StateT Memo m

-- | Solve the roots and goals of the configuration.
solve :: (Monad m) => SolverInputs m -> SolverConfig -> m (Either SolveFailure Solution)
solve inputs config =
  evalStateT (search inputs config initial) (Memo Map.empty Map.empty 0)
  where
    initial =
      Search
        { searchAssigned = Map.empty,
          searchGoals =
            foldr
              (\(name, range) -> addGoal config name DependentRoot range)
              Map.empty
              ([(alias config name, anyVersion) | name <- Map.keys (configRoots config)] <> [(alias config name, range) | (name, range) <- configGoals config])
        }

alias :: SolverConfig -> PackageName -> PackageName
alias config name = fromMaybe name (Map.lookup name (configAliases config))

-- | The range the constraints put on a package.
constraintRange :: SolverConfig -> PackageName -> VersionRange
constraintRange config name =
  simplifyVersionRange (foldr intersectVersionRanges anyVersion [range | ConstraintVersion constrained range <- configConstraints config, alias config constrained == name])

-- | The flags the constraints fix for a package.
constrainedFlags :: SolverConfig -> PackageName -> Map FlagName Bool
constrainedFlags config name =
  Map.fromList [(flag, value) | ConstraintFlag constrained flag value <- configConstraints config, alias config constrained == name]

-- | Narrow a goal by one more dependent, or start it. A new goal starts
-- from the range the constraints allow.
addGoal :: SolverConfig -> PackageName -> Dependent -> VersionRange -> Map PackageName Goal -> Map PackageName Goal
addGoal config name dependent range goals =
  Map.insert name narrowed goals
  where
    narrowed =
      case Map.lookup name goals of
        Nothing ->
          Goal
            { goalRange = simplifyVersionRange (intersectVersionRanges (constraintRange config name) range),
              goalDependents = [(dependent, range)] <> [(DependentConstraint, constraintRange config name) | constraintRange config name /= anyVersion]
            }
        Just goal ->
          Goal
            { goalRange = simplifyVersionRange (intersectVersionRanges (goalRange goal) range),
              goalDependents = goalDependents goal <> [(dependent, range)]
            }

candidatesOf :: (Monad m) => SolverInputs m -> SolverConfig -> PackageName -> Solve m [Candidate]
candidatesOf inputs config name = do
  known <- gets (Map.lookup name . memoCandidates)
  case known of
    Just candidates -> pure candidates
    Nothing -> do
      candidates <- lift (inputsCandidates inputs name (Map.lookup name (configPreferences config)))
      modify' (\memo -> memo {memoCandidates = Map.insert name candidates (memoCandidates memo)})
      pure candidates

descriptionOf :: (Monad m) => SolverInputs m -> Candidate -> Solve m GenericPackageDescription
descriptionOf inputs candidate = do
  let key = (candidateName candidate, candidateVersion candidate, candidateRevision candidate, candidateSource candidate)
  known <- gets (Map.lookup key . memoDescriptions)
  case known of
    Just gpd -> pure gpd
    Nothing -> do
      gpd <- lift (inputsDescription inputs candidate)
      modify' (\memo -> memo {memoDescriptions = Map.insert key gpd (memoDescriptions memo)})
      pure gpd

-- | The candidates of a goal in the order they are tried: the preferred
-- version first, then newest first with deprecated versions last.
orderedCandidates :: SolverConfig -> PackageName -> [Candidate] -> [Candidate]
orderedCandidates config name =
  sortBy (comparing preferredFirst <> comparing candidateDeprecated <> comparing (Down . candidateVersion))
  where
    preferredFirst candidate =
      case Map.lookup name (configPreferences config) of
        Just preference | preferredVersion preference == candidateVersion candidate -> False
        _ -> True

search :: (Monad m) => SolverInputs m -> SolverConfig -> Search -> Solve m (Either SolveFailure Solution)
search inputs config state = do
  goals <- mapM (\(name, goal) -> (,) (name, goal) <$> goalCandidates name goal) (Map.toAscList (searchGoals state))
  case sortOn (length . snd) goals of
    [] -> pure (Right (searchAssigned state))
    ((name, goal), candidates) : _ -> do
      allCandidates <- candidatesOf inputs config name
      if null allCandidates
        then failWith (UnknownPackage name (goalDependents goal))
        else do
          let outside = sortOn Down [candidateVersion candidate | candidate <- allCandidates, not (withinRange (candidateVersion candidate) (goalRange goal))]
          tryCandidates name goal outside candidates []
  where
    goalCandidates name goal = do
      candidates <- candidatesOf inputs config name
      pure (orderedCandidates config name [candidate | candidate <- candidates, withinRange (candidateVersion candidate) (goalRange goal)])

    failWith failure = do
      memo <- get
      let backtracks = memoBacktracks memo + 1
      put memo {memoBacktracks = backtracks}
      pure
        ( Left
            ( if backtracks > configMaxBacktracks config
                then BacktrackLimit (configMaxBacktracks config) failure
                else failure
            )
        )

    tryCandidates name goal outside [] failures =
      failWith (NoCandidates name (goalRange goal) (goalDependents goal) outside (reverse failures))
    tryCandidates name goal outside (candidate : rest) failures = do
      gpd <- descriptionOf inputs candidate
      let root = Map.lookup name (configRoots config)
          flagChoices = flagAssignments config name gpd root
      tryFlags name goal outside candidate gpd root flagChoices rest failures

    tryFlags name goal outside _ _ _ [] rest failures =
      tryCandidates name goal outside rest failures
    tryFlags name goal outside candidate gpd root (flags : moreFlags) rest failures = do
      let dependencies = candidateDependencies (configPlatform config) (configAliases config) flags root gpd
          conflict =
            [ RejectedDependency dependency range (assignmentVersion assigned)
            | (dependency, range) <- Map.toAscList dependencies,
              Just assigned <- [Map.lookup dependency (searchAssigned state)],
              not (withinRange (assignmentVersion assigned) range)
            ]
      case conflict of
        rejection : _ ->
          tryFlags name goal outside candidate gpd root moreFlags rest (CandidateFailure candidate flags rejection : failures)
        [] -> do
          let assignment =
                Assignment
                  { assignmentVersion = candidateVersion candidate,
                    assignmentRevision = candidateRevision candidate,
                    assignmentFlags = flags,
                    assignmentSource = candidateSource candidate,
                    assignmentDependencies = dependencies
                  }
              dependent = DependentPackage name (candidateVersion candidate) flags
              goals' =
                foldr
                  (\(dependency, range) -> addGoal config dependency dependent range)
                  (Map.delete name (searchGoals state))
                  [(dependency, range) | (dependency, range) <- Map.toAscList dependencies, not (Map.member dependency (searchAssigned state))]
              state' = Search {searchAssigned = Map.insert name assignment (searchAssigned state), searchGoals = goals'}
          result <- search inputs config state'
          case result of
            Right solution -> pure (Right solution)
            Left failure@(BacktrackLimit _ _) -> pure (Left failure)
            Left failure ->
              tryFlags name goal outside candidate gpd root moreFlags rest (CandidateFailure candidate flags (RejectedSubtree failure) : failures)

-- | The flag assignments of a candidate in the order they are tried: the
-- preferred one first when the lock has one for this version, then the
-- default, then every assignment in increasing number of flips. Each
-- assignment covers the searched flags and the constrained ones, so it is
-- exactly what the plan decided.
flagAssignments :: SolverConfig -> PackageName -> GenericPackageDescription -> Maybe Stanzas -> [FlagAssignment]
flagAssignments config name gpd root =
  dedupe (preferred <> [mkFlagAssignment (Map.toAscList (Map.union fixed (Map.fromList flips))) | flips <- flipOrders])
  where
    fixed = Map.union (constrainedFlags config name) (Map.fromList (targetFlagOverrides (snd (configPlatform config)) (packageName (packageDescription gpd))))
    searched = [flag | flag <- searchableFlags (configPlatform config) root gpd, not (Map.member flag fixed)]
    defaults = Map.fromList [(flagName flag, flagDefault flag) | flag <- genPackageFlags gpd]
    flipOrders =
      [ [(flag, if flag `elem` flipped then not (defaults Map.! flag) else defaults Map.! flag) | flag <- searched]
      | flipped <- sortOn length (subsequences searched)
      ]
    preferred =
      case Map.lookup name (configPreferences config) of
        Just preference
          | any ((`elem` searched) . fst) (unFlagAssignment (preferredFlags preference)) ->
              [ mkFlagAssignment
                  ( Map.toAscList
                      ( Map.union
                          fixed
                          (Map.fromList [(flag, fromMaybe (defaults Map.! flag) (lookupFlagAssignment flag (preferredFlags preference))) | flag <- searched])
                      )
                  )
              ]
        _ -> []
    dedupe = go Set.empty
      where
        go _ [] = []
        go seen (assignment : rest)
          | Set.member (unFlagAssignment assignment) seen = go seen rest
          | otherwise = assignment : go (Set.insert (unFlagAssignment assignment) seen) rest

-- | The automatic flags of a package that guard a @build-depends@ clause
-- in a component that contributes dependencies. Only these can change the
-- plan, so only these are searched.
searchableFlags :: (OS, Arch) -> Maybe Stanzas -> GenericPackageDescription -> [FlagName]
searchableFlags _ root gpd =
  [ flagName flag
  | flag <- genPackageFlags gpd,
    not (flagManual flag),
    flagName flag `Set.member` guarding
  ]
  where
    guarding =
      Set.unions
        ( map (guardingFlags libBuildInfo) (libraryTrees gpd)
            <> concat
              [ map (guardingFlags buildInfo) (executableTrees gpd)
                  <> [guardingFlags testBuildInfo tree | stanzasTests stanzas, tree <- map snd (condTestSuites gpd)]
                  <> [guardingFlags benchmarkBuildInfo tree | stanzasBenchmarks stanzas, tree <- map snd (condBenchmarks gpd)]
              | Just stanzas <- [root]
              ]
        )

-- | The flags in the conditions of the branches under which some node
-- states a dependency.
guardingFlags :: (a -> BuildInfo) -> CondTree ConfVar c a -> Set.Set FlagName
guardingFlags toBuildInfo tree =
  Set.unions (map branch (condTreeComponents tree))
  where
    branch (CondBranch condition thenTree elseTree) =
      Set.unions
        ( [conditionFlags condition | hasDependencies thenTree || maybe False hasDependencies elseTree]
            <> [guardingFlags toBuildInfo thenTree]
            <> [guardingFlags toBuildInfo subtree | Just subtree <- [elseTree]]
        )
    hasDependencies subtree =
      not (null (targetBuildDepends (toBuildInfo (condTreeData subtree))))
        || any (\(CondBranch _ t e) -> hasDependencies t || maybe False hasDependencies e) (condTreeComponents subtree)

conditionFlags :: Condition ConfVar -> Set.Set FlagName
conditionFlags condition =
  case condition of
    Var (PackageFlag flag) -> Set.singleton flag
    Var _ -> Set.empty
    Lit _ -> Set.empty
    CNot inner -> conditionFlags inner
    COr left right -> Set.union (conditionFlags left) (conditionFlags right)
    CAnd left right -> Set.union (conditionFlags left) (conditionFlags right)

libraryTrees :: GenericPackageDescription -> [CondTree ConfVar [Dependency] Library]
libraryTrees gpd = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd)

executableTrees :: GenericPackageDescription -> [CondTree ConfVar [Dependency] Executable]
executableTrees gpd = map snd (condExecutables gpd)

-- | The build infos of the components that contribute dependencies under
-- one flag assignment: the buildable libraries, and for a root package its
-- executables and requested stanzas as well.
contributingBuildInfos :: (OS, Arch) -> FlagAssignment -> Maybe Stanzas -> GenericPackageDescription -> [BuildInfo]
contributingBuildInfos (os, arch) flags root gpd =
  filter
    buildable
    ( map (merged libBuildInfo) (libraryTrees gpd)
        <> concat
          [ map (merged buildInfo) (executableTrees gpd)
              <> [merged testBuildInfo tree | stanzasTests stanzas, tree <- map snd (condTestSuites gpd)]
              <> [merged benchmarkBuildInfo tree | stanzasBenchmarks stanzas, tree <- map snd (condBenchmarks gpd)]
          | Just stanzas <- [root]
          ]
    )
  where
    evalCond = conditionEvaluatorIn (BuildContext os arch flags) gpd
    merged :: (a -> BuildInfo) -> CondTree ConfVar c a -> BuildInfo
    merged toBuildInfo tree = mconcat (map toBuildInfo (collectCondTreeData evalCond tree))

-- | The packages a candidate needs under one flag assignment, after
-- aliasing, each with the intersection of the ranges the components
-- demand. A dependency on the package itself, which a sub-library or an
-- executable states, is not a dependency of the plan.
candidateDependencies :: (OS, Arch) -> Map PackageName PackageName -> FlagAssignment -> Maybe Stanzas -> GenericPackageDescription -> Map PackageName VersionRange
candidateDependencies platform aliases flags root gpd =
  Map.map
    simplifyVersionRange
    ( Map.fromListWith
        intersectVersionRanges
        [ (resolved, range)
        | build <- contributingBuildInfos platform flags root gpd,
          Dependency name range _ <- targetBuildDepends build,
          let resolved = fromMaybe name (Map.lookup name aliases),
          resolved /= self
        ]
    )
  where
    self = fromMaybe (packageName (packageDescription gpd)) (Map.lookup (packageName (packageDescription gpd)) aliases)

-- | The build tools the contributing components ask for that the host
-- cannot run. Tools run on the build host, so they are checked against the
-- preprocessor table rather than solved against the target plan.
unknownBuildTools :: (OS, Arch) -> FlagAssignment -> Maybe Stanzas -> GenericPackageDescription -> [String]
unknownBuildTools platform flags root gpd =
  Set.toAscList
    ( Set.fromList
        [ tool
        | build <- contributingBuildInfos platform flags root gpd,
          tool <-
            [unUnqualComponentName exe | ExeDependency _ exe _ <- buildToolDepends build]
              <> [tool | LegacyExeDependency tool _ <- buildTools build],
          tool `notElem` knownTools
        ]
    )
  where
    knownTools = map preprocessorToolName [minBound .. maxBound :: Preprocessor]

-- | Check that an assignment, from a lock file say, still solves the
-- configuration: every root and goal is present in the demanded range,
-- every constraint holds, the dependencies of every package are present in
-- range, and nothing is listed that the roots do not reach. The result
-- carries the dependencies each package has under its recorded flags.
verifySolution :: (Monad m) => SolverInputs m -> SolverConfig -> Map PackageName (Version, Maybe Int, FlagAssignment) -> m (Either String Solution)
verifySolution inputs config recorded = do
  candidates <- foldM candidateFor (Right Map.empty) (Map.toAscList recorded)
  case candidates of
    Left problem -> pure (Left problem)
    Right chosen -> do
      solution <- Map.traverseWithKey assignmentFor chosen
      pure (checks solution)
  where
    candidateFor acc (name, (version, revision, flags)) =
      case acc of
        Left problem -> pure (Left problem)
        Right chosen -> do
          available <- inputsCandidates inputs name (Just (Preference version revision flags))
          case [candidate | candidate <- available, candidateVersion candidate == version] of
            candidate : _
              | maybe True (== candidateRevision candidate) revision ->
                  pure (Right (Map.insert name (candidate, flags) chosen))
              | otherwise -> pure (Left (unPackageName name <> "-" <> prettyShow version <> " has no revision " <> maybe "" show revision))
            [] -> pure (Left ("no candidate " <> unPackageName name <> "-" <> prettyShow version))

    assignmentFor name (candidate, flags) = do
      gpd <- inputsDescription inputs candidate
      let root = Map.lookup name (configRoots config)
          decided = Map.union (constrainedFlags config name) (Map.fromList (targetFlagOverrides (snd (configPlatform config)) (packageName (packageDescription gpd))))
          full = mkFlagAssignment (Map.toAscList (Map.union decided (Map.fromList (unFlagAssignment flags))))
      pure
        Assignment
          { assignmentVersion = candidateVersion candidate,
            assignmentRevision = candidateRevision candidate,
            assignmentFlags = full,
            assignmentSource = candidateSource candidate,
            assignmentDependencies = candidateDependencies (configPlatform config) (configAliases config) full root gpd
          }

    checks solution =
      case catMaybes (rootChecks solution <> constraintChecks solution <> dependencyChecks solution <> reachabilityChecks solution) of
        problem : _ -> Left problem
        [] -> Right solution

    rootChecks solution =
      [ satisfied solution "the root" name range
      | (name, range) <- [(alias config root, anyVersion) | root <- Map.keys (configRoots config)] <> [(alias config name, range) | (name, range) <- configGoals config]
      ]
    constraintChecks solution =
      [ satisfied solution "a constraint" (alias config name) range
      | ConstraintVersion name range <- configConstraints config
      ]
        <> [ if lookupFlagAssignment flag (assignmentFlags assignment) == Just value
               then Nothing
               else Just ("the constraint on the flag " <> unFlagName flag <> " of " <> unPackageName name <> " is not met")
           | ConstraintFlag name flag value <- configConstraints config,
             Just assignment <- [Map.lookup (alias config name) solution]
           ]
    dependencyChecks solution =
      [ satisfied solution (unPackageName name <> "-" <> prettyShow (assignmentVersion assignment)) dependency range
      | (name, assignment) <- Map.toAscList solution,
        (dependency, range) <- Map.toAscList (assignmentDependencies assignment)
      ]
    reachabilityChecks solution =
      [ Just (unPackageName name <> " is no longer needed")
      | name <- Map.keys solution,
        not (Set.member name (reachable solution))
      ]
    reachable solution = go Set.empty (map (alias config) (Map.keys (configRoots config)) <> map (alias config . fst) (configGoals config))
      where
        go seen [] = seen
        go seen (name : rest)
          | Set.member name seen = go seen rest
          | otherwise = go (Set.insert name seen) (maybe [] (Map.keys . assignmentDependencies) (Map.lookup name solution) <> rest)

    satisfied solution who name range =
      case Map.lookup name solution of
        Nothing -> Just (who <> " needs " <> unPackageName name <> ", which is not listed")
        Just assignment
          | withinRange (assignmentVersion assignment) range -> Nothing
          | otherwise -> Just (who <> " needs " <> unPackageName name <> " " <> prettyShow range <> ", but " <> prettyShow (assignmentVersion assignment) <> " is listed")
