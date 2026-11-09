-- |
-- Module      : Aihc.PackagePlan.Solver
-- Description : Choose one version and one flag assignment per package
--
-- The solver turns the @build-depends@ of a set of root packages into an
-- assignment of exactly one version and one flag assignment to every
-- package the build needs. It is a function of its inputs alone: the
-- candidates of each package and their cabal files arrive through
-- 'SolverInputs', which a test instantiates with pure maps and the compiler
-- with the Hackage queries.
--
-- The search is PubGrub, the version solving algorithm of Dart's @pub@. A
-- search variable is a package, an automatic flag of a package, or the
-- root that stands for the roots and goals. A term says that a variable
-- is in a set of versions, or that it is not. An incompatibility is a set
-- of terms that cannot all hold. Every dependency is one: @tls-2.4.14@
-- needs @serialise >=0.2 && <0.3@ is the incompatibility
-- @{tls == 2.4.14, not serialise >=0.2 && <0.3}@. A dependency under a
-- flag carries the flag as a term: @{pandoc == 3.13, pandoc +http, not
-- tls >=2.0.1 && <2.5}@.
--
-- The partial solution is a list of assignments, each a decision or a
-- derivation at a decision level. Unit propagation derives the negation
-- of the one unsatisfied term of an incompatibility whose other terms
-- hold. A decision picks the undecided variable with the fewest versions
-- left and its preferred version, newest first with deprecated versions
-- last. When every term of an incompatibility holds, conflict resolution
-- combines it with the cause of its most recent satisfier until one term
-- is at the most recent decision level, then returns to the level that
-- makes that term the only open one, and keeps the combined
-- incompatibility. A conflict is thus derived once. Without this the
-- @time-1.15@ conflict of @pandoc@ was derived again under each of the
-- thousands of combinations of its dependents.
--
-- A failed solve returns the derivation of the final incompatibility,
-- which the renderer writes as an explanation.
--
-- The search is deterministic, so the same inputs give the same plan.
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
    Dependent (..),
    renderSolveFailure,
    renderDependents,
    solve,
    unavailablePackages,
    verifySolution,

    -- * Cabal file inspection
    candidateDependencies,
    searchableFlags,
    unknownBuildTools,
  )
where

import Aihc.Cabal
  ( Branch (..),
    BuildInfo,
    Component (..),
    ComponentKind (..),
    Condition (..),
    Conditional (..),
    Dependency (..),
    Package,
    ToolDependency (..),
    buildTools,
    dependencies,
    flagDefault,
    flagManual,
    flagName,
    packageComponents,
    packageFlags,
  )
import Aihc.Hackage.Cabal (BuildContext (..), collectMergedBuildInfo, conditionEvaluatorIn, installedLibraryTrees, isBuildable, targetFlagOverrides)
import Aihc.Hackage.Package
  ( Arch,
    FlagAssignment,
    FlagName,
    OS,
    PackageName,
    Version,
    VersionRange,
    anyVersion,
    intersectVersionRanges,
    lookupFlagAssignment,
    mkFlagAssignment,
    mkPackageName,
    packageNameOf,
    showVersion,
    showVersionRange,
    simplifyVersionRange,
    unFlagAssignment,
    unFlagName,
    unPackageName,
    versionFromList,
    withinRange,
  )
import Aihc.Hackage.Preprocessor (Preprocessor, preprocessorToolName)
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, execState, get, gets, modify', put, runStateT)
import Data.Char (toUpper)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (intercalate, nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

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
    -- | The revision of the cabal file to read, or 'Nothing' for the
    -- newest revision there is. A lock file names a revision; nothing else
    -- does, since learning the newest revision of a Hackage release costs a
    -- request for its cabal file. 'inputsDescription' says which revision
    -- it read, and that is the revision the solution records.
    candidateRevision :: !(Maybe Int),
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
    -- | The cabal file of a candidate at its revision, with the revision
    -- that was read. The revision is zero for anything but a Hackage
    -- release.
    inputsDescription :: Candidate -> m (Int, Package)
  }

-- | The optional components of a root package that contribute
-- dependencies.
data Stanzas = Stanzas
  { stanzasTests :: !Bool,
    stanzasBenchmarks :: !Bool,
    -- | The executables that contribute. 'Nothing' selects every
    -- executable.
    stanzasExecutables :: !(Maybe (Set.Set Text))
  }
  deriving (Eq, Show)

-- | No test suites, no benchmarks, and every executable.
noStanzas :: Stanzas
noStanzas = Stanzas False False Nothing

-- | The executables of a root package that its stanzas select.
selectedExecutableTrees :: Stanzas -> Package -> [Conditional BuildInfo]
selectedExecutableTrees stanzas gpd =
  [ tree
  | Component (Executable name) tree <- packageComponents gpd,
    maybe True (Set.member name) (stanzasExecutables stanzas)
  ]

-- | The trees of the test suites of a package.
testSuiteTrees :: Package -> [Conditional BuildInfo]
testSuiteTrees gpd = [tree | Component (TestSuite _) tree <- packageComponents gpd]

-- | The trees of the benchmarks of a package.
benchmarkTrees :: Package -> [Conditional BuildInfo]
benchmarkTrees gpd = [tree | Component (Benchmark _) tree <- packageComponents gpd]

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
    -- contribute dependencies.
    configRoots :: !(Map PackageName Stanzas),
    -- | Further packages to plan, with the range each must satisfy. A main
    -- module built against a set of packages has these and no root.
    configGoals :: ![(PackageName, VersionRange)],
    -- | Give up after this many conflicts. A normal solve resolves a few
    -- conflicts at most, so the limit is a guard against a search that
    -- does not end.
    configMaxConflicts :: !Int
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

-- | A search variable.
data Var
  = -- | Stands for the roots and the goals. It has the single version
    -- 'rootVersion', and its dependencies are the roots and goals.
    VarRoot
  | VarPackage !PackageName
  | -- | An automatic flag of a package that guards a dependency. Its
    -- versions are 'flagOff' and 'flagOn'.
    VarFlag !PackageName !FlagName
  deriving (Eq, Ord, Show)

rootVersion, flagOff, flagOn :: Version
rootVersion = versionFromList [0]
flagOff = versionFromList [0]
flagOn = versionFromList [1]

flagVersions :: Set Version
flagVersions = Set.fromList [flagOff, flagOn]

flagVersion :: Bool -> Version
flagVersion on = if on then flagOn else flagOff

-- | A statement about one variable. A positive term says the variable
-- takes a version in the set; a negative term says it does not, which a
-- variable outside the plan also satisfies.
data Term
  = Positive !(Set Version)
  | Negative !(Set Version)
  deriving (Eq, Ord, Show)

negateTerm :: Term -> Term
negateTerm (Positive versions) = Negative versions
negateTerm (Negative versions) = Positive versions

intersectTerms :: Term -> Term -> Term
intersectTerms (Positive a) (Positive b) = Positive (Set.intersection a b)
intersectTerms (Positive a) (Negative b) = Positive (Set.difference a b)
intersectTerms (Negative a) (Positive b) = Positive (Set.difference b a)
intersectTerms (Negative a) (Negative b) = Negative (Set.union a b)

-- | Every value that satisfies the first term satisfies the second.
subsetOf :: Term -> Term -> Bool
subsetOf (Positive a) (Positive b) = Set.isSubsetOf a b
subsetOf (Positive a) (Negative b) = Set.disjoint a b
subsetOf (Negative _) (Positive _) = False
subsetOf (Negative a) (Negative b) = Set.isSubsetOf b a

-- | No value satisfies both terms.
disjointTerms :: Term -> Term -> Bool
disjointTerms (Positive a) (Positive b) = Set.disjoint a b
disjointTerms (Positive a) (Negative b) = Set.isSubsetOf a b
disjointTerms (Negative a) (Positive b) = Set.isSubsetOf b a
disjointTerms (Negative _) (Negative _) = False

-- | A positive term with no version: nothing satisfies it.
isEmptyTerm :: Term -> Bool
isEmptyTerm (Positive versions) = Set.null versions
isEmptyTerm (Negative _) = False

type IncompatId = Int

-- | A set of terms that cannot all hold.
data Incompat = Incompat
  { incTerms :: !(Map Var Term),
    incKind :: !Kind
  }
  deriving (Eq, Show)

-- | Where an incompatibility comes from. The root and the dependencies
-- are the external facts; a derived incompatibility names the two it was
-- combined from.
data Kind
  = -- | The root must be in the plan.
    KindRoot
  | -- | The root needs a package in a range. The flag says that the
    -- package has no candidate at all.
    KindRootNeeds !PackageName !VersionRange !Bool
  | -- | A constraint forbids the versions of a package outside a range.
    KindConstraint !PackageName !VersionRange
  | -- | The positive terms, a package with its flags, need a package in a
    -- range. The flag says that the package has no candidate at all.
    KindDependency !PackageName !VersionRange !Bool
  | -- | A version of a package has a searchable flag.
    KindFlag !PackageName !FlagName
  | KindDerived !IncompatId !IncompatId
  deriving (Eq, Show)

-- | The incompatibilities a failed solve derived, and the one that shows
-- that no plan exists.
data Proof = Proof
  { proofIncompatibilities :: !(IntMap Incompat),
    proofRoot :: !IncompatId,
    -- | The versions of every package the proof mentions, in increasing
    -- order, so that a set of versions renders as a range.
    proofUniverses :: !(Map PackageName [Version])
  }
  deriving (Eq, Show)

data SolveFailure
  = -- | The derivation of an incompatibility that no plan satisfies.
    NoSolution !Proof
  | -- | The search stopped at the conflict limit; the proof is of the
    -- last conflict it saw.
    ConflictLimit !Int !Proof
  deriving (Eq, Show)

-- | The failure as an explanation a user can act on: the chain of facts
-- from the cabal files to the incompatibility that no plan satisfies.
renderSolveFailure :: SolveFailure -> String
renderSolveFailure failure =
  intercalate "\n" $
    case failure of
      NoSolution proof -> explain proof
      ConflictLimit limit proof ->
        ("Gave up after " <> show limit <> " conflicts; the last conflict was:") : explain proof

-- | The packages that demanded a range of a goal, each with its range.
renderDependents :: [(Dependent, VersionRange)] -> String
renderDependents dependents =
  intercalate ", " [renderDependent dependent <> rangeSuffix range | (dependent, range) <- dependents]
  where
    rangeSuffix range
      | range == anyVersion = ""
      | otherwise = " (" <> showVersionRange range <> ")"
    renderDependent dependent =
      case dependent of
        DependentRoot -> "the root"
        DependentConstraint -> "a constraint"
        DependentPackage name version flags -> renderCandidateName name version flags

renderCandidateName :: PackageName -> Version -> FlagAssignment -> String
renderCandidateName name version flags =
  unPackageName name
    <> "-"
    <> showVersion version
    <> concat [" " <> (if value then "+" else "-") <> unFlagName flag | (flag, value) <- unFlagAssignment flags]

-- | Every package that the roots and goals reach and that has no
-- candidate, with the packages that need it. The walk follows each
-- candidate of a known package under its default flag assignment. It
-- ignores the version ranges, so the result names only the packages that
-- are absent, not the ones in the wrong version. A caller uses it to
-- explain a failed solve when the candidates come from a restricted
-- source.
unavailablePackages :: (Monad m) => SolverInputs m -> SolverConfig -> m [(PackageName, [(Dependent, VersionRange)])]
unavailablePackages inputs config =
  evalStateT (walk Set.empty Map.empty start) (Memo Map.empty Map.empty)
  where
    start =
      [(alias config name, DependentRoot, anyVersion) | name <- Map.keys (configRoots config)]
        <> [(alias config name, DependentRoot, range) | (name, range) <- configGoals config]

    walk _ missing [] = pure (Map.toAscList (Map.map reverse missing))
    walk seen missing ((name, dependent, range) : rest) = do
      candidates <- memoCandidatesOf inputs config name
      if null candidates
        then walk (Set.insert name seen) (Map.insertWith (<>) name [(dependent, range)] missing) rest
        else
          if Set.member name seen
            then walk seen missing rest
            else do
              next <- concat <$> mapM (dependenciesOf name) candidates
              walk (Set.insert name seen) missing (rest <> next)

    dependenciesOf name candidate = do
      (_, gpd) <- memoDescriptionOf inputs candidate
      let root = Map.lookup name (configRoots config)
          setup = flagSetup config name root gpd
          flags = completeFlags setup [(flag, setupDefaults setup Map.! flag) | flag <- setupSearched setup]
          dependent = DependentPackage name (candidateVersion candidate) flags
      pure
        [ (dependency, dependent, dependencyRange)
        | (dependency, dependencyRange) <- Map.toAscList (candidateDependencies (configPlatform config) (configAliases config) flags root gpd)
        ]

-- | The candidates and cabal files already fetched.
data Memo = Memo
  { memoCandidates :: !(Map PackageName [Candidate]),
    memoDescriptions :: !(Map (PackageName, Version, Maybe Int, CandidateSource) (Int, Package))
  }

memoCandidatesOf :: (Monad m) => SolverInputs m -> SolverConfig -> PackageName -> StateT Memo m [Candidate]
memoCandidatesOf inputs config name = do
  known <- gets (Map.lookup name . memoCandidates)
  case known of
    Just candidates -> pure candidates
    Nothing -> do
      candidates <- lift (inputsCandidates inputs name (Map.lookup name (configPreferences config)))
      modify' (\memo -> memo {memoCandidates = Map.insert name candidates (memoCandidates memo)})
      pure candidates

memoDescriptionOf :: (Monad m) => SolverInputs m -> Candidate -> StateT Memo m (Int, Package)
memoDescriptionOf inputs candidate = do
  let key = (candidateName candidate, candidateVersion candidate, candidateRevision candidate, candidateSource candidate)
  known <- gets (Map.lookup key . memoDescriptions)
  case known of
    Just described -> pure described
    Nothing -> do
      described <- lift (inputsDescription inputs candidate)
      modify' (\memo -> memo {memoDescriptions = Map.insert key described (memoDescriptions memo)})
      pure described

alias :: SolverConfig -> PackageName -> PackageName
alias config name = fromMaybe name (Map.lookup name (configAliases config))

-- | The flags the constraints fix for a package.
constrainedFlags :: SolverConfig -> PackageName -> Map FlagName Bool
constrainedFlags config name =
  Map.fromList [(flag, value) | ConstraintFlag constrained flag value <- configConstraints config, alias config constrained == name]

-- | The flags of one version of a package: the ones a constraint or the
-- target fixes, the automatic ones the search decides, and the defaults.
data FlagSetup = FlagSetup
  { setupFixed :: !(Map FlagName Bool),
    setupSearched :: ![FlagName],
    setupDefaults :: !(Map FlagName Bool)
  }

-- | More searchable flags than this leave the rest at their defaults, so
-- that the dependency clauses of a version are evaluated under at most
-- @2 ^ maxSearchedFlags@ assignments.
maxSearchedFlags :: Int
maxSearchedFlags = 8

flagSetup :: SolverConfig -> PackageName -> Maybe Stanzas -> Package -> FlagSetup
flagSetup config name root gpd =
  FlagSetup
    { setupFixed = fixed,
      setupSearched = take maxSearchedFlags [flag | flag <- searchableFlags (configPlatform config) root gpd, not (Map.member flag fixed)],
      setupDefaults = Map.fromList [(flagName flag, flagDefault flag) | flag <- packageFlags gpd]
    }
  where
    fixed = Map.union (constrainedFlags config name) (Map.fromList (targetFlagOverrides (snd (configPlatform config)) (packageNameOf gpd)))

-- | The flag assignment a plan records: the fixed flags and the searched
-- ones at the given values.
completeFlags :: FlagSetup -> [(FlagName, Bool)] -> FlagAssignment
completeFlags setup searched = mkFlagAssignment (Map.toAscList (Map.union (setupFixed setup) (Map.fromList searched)))

-- | One assignment of the partial solution: a decision when it has no
-- cause, else the derivation from the incompatibility it names.
data Assigned = Assigned
  { asgVar :: !Var,
    asgTerm :: !Term,
    asgLevel :: !Int,
    asgCause :: !(Maybe IncompatId)
  }

data Partial = Partial
  { -- | Newest first.
    partialHistory :: ![Assigned],
    -- | The intersection of the terms of each variable.
    partialTerms :: !(Map Var Term),
    partialDecisions :: !(Map Var Version),
    partialLevel :: !Int
  }

emptyPartial :: Partial
emptyPartial = Partial [] Map.empty Map.empty 0

assign :: Assigned -> Partial -> Partial
assign assigned partial =
  partial
    { partialHistory = assigned : partialHistory partial,
      partialTerms = Map.insertWith intersectTerms (asgVar assigned) (asgTerm assigned) (partialTerms partial),
      partialDecisions =
        case (asgCause assigned, asgTerm assigned) of
          (Nothing, Positive versions) | [version] <- Set.toList versions -> Map.insert (asgVar assigned) version (partialDecisions partial)
          _ -> partialDecisions partial
    }

decideIn :: Var -> Version -> Partial -> Partial
decideIn var version partial =
  (assign (Assigned var (Positive (Set.singleton version)) level Nothing) partial) {partialLevel = level}
  where
    level = partialLevel partial + 1

-- | Drop every assignment above a decision level.
backtrackTo :: Int -> Partial -> Partial
backtrackTo level partial =
  (foldl (flip assign) emptyPartial (reverse (filter ((<= level) . asgLevel) (partialHistory partial)))) {partialLevel = level}

-- | How the partial solution stands to an incompatibility.
data Relation
  = -- | Every term holds: a conflict.
    Satisfied
  | -- | Every term but one holds, so the negation of that one follows.
    AlmostSatisfied !Var !Term
  | Inconclusive
  | Contradicted
  deriving (Eq, Show)

relationOf :: Partial -> Incompat -> Relation
relationOf partial inc = go Satisfied (Map.toAscList (incTerms inc))
  where
    go relation [] = relation
    go relation ((var, term) : rest) =
      case Map.lookup var (partialTerms partial) of
        Just assigned
          | subsetOf assigned term -> go relation rest
          | disjointTerms assigned term -> Contradicted
        _ -> case relation of
          Satisfied -> go (AlmostSatisfied var term) rest
          _ -> go Inconclusive rest

-- | The key under which the dependency incompatibilities of the versions
-- of one package merge: the package, its flag terms, the dependency and
-- its term, and the range as written, for the explanation.
type MergeKey = (PackageName, Map Var Term, PackageName, Maybe Term, String)

data St = St
  { stIncompats :: !(IntMap Incompat),
    stNextId :: !IncompatId,
    -- | The live incompatibilities that mention each variable. A merged
    -- incompatibility leaves the index and stays in the store, since a
    -- derivation can name it.
    stIndex :: !(Map Var IntSet),
    stMerge :: !(Map MergeKey IncompatId),
    stPartial :: !Partial,
    stMemo :: !Memo,
    -- | The versions whose incompatibilities are in the store.
    stExpanded :: !(Set (Var, Version)),
    stConflicts :: !Int
  }

type Solve m = StateT St m

-- | Solve the roots and goals of the configuration.
solve :: (Monad m) => SolverInputs m -> SolverConfig -> m (Either SolveFailure Solution)
solve inputs config =
  evalStateT start (St IntMap.empty 0 Map.empty Map.empty emptyPartial (Memo Map.empty Map.empty) Set.empty 0)
  where
    start = do
      _ <- addIncompat (Incompat (Map.singleton VarRoot (Negative (Set.singleton rootVersion))) KindRoot)
      forM_ [(alias config constrained, range) | ConstraintVersion constrained range <- configConstraints config] $ \(name, range) -> do
        universe <- universeOf inputs config (VarPackage name)
        let outside = Set.filter (\version -> not (withinRange version range)) universe
        unless (Set.null outside) $
          void (addIncompat (Incompat (Map.singleton (VarPackage name) (Positive outside)) (KindConstraint name range)))
      loop VarRoot

    loop var = do
      propagated <- propagate config var
      case propagated of
        Left failure -> pure (Left failure)
        Right () -> do
          next <- decide inputs config
          case next of
            Nothing -> Right <$> extractSolution inputs config
            Just var' -> loop var'

lookupIncompat :: (Monad m) => IncompatId -> Solve m Incompat
lookupIncompat incId = gets (\st -> stIncompats st IntMap.! incId)

-- | Put an incompatibility in the store without indexing it.
store :: (Monad m) => Incompat -> Solve m IncompatId
store inc = do
  st <- get
  let incId = stNextId st
  put st {stIncompats = IntMap.insert incId inc (stIncompats st), stNextId = incId + 1}
  pure incId

indexIncompat :: (Monad m) => IncompatId -> Solve m ()
indexIncompat incId = do
  inc <- lookupIncompat incId
  modify' (\st -> st {stIndex = foldr (\var -> Map.insertWith IntSet.union var (IntSet.singleton incId)) (stIndex st) (Map.keys (incTerms inc))})

unindexIncompat :: (Monad m) => IncompatId -> Solve m ()
unindexIncompat incId = do
  inc <- lookupIncompat incId
  modify' (\st -> st {stIndex = foldr (Map.adjust (IntSet.delete incId)) (stIndex st) (Map.keys (incTerms inc))})

addIncompat :: (Monad m) => Incompat -> Solve m IncompatId
addIncompat inc = do
  incId <- store inc
  indexIncompat incId
  pure incId

candidatesOf :: (Monad m) => SolverInputs m -> SolverConfig -> PackageName -> Solve m [Candidate]
candidatesOf inputs config name = withMemo (memoCandidatesOf inputs config name)

descriptionOf :: (Monad m) => SolverInputs m -> Candidate -> Solve m (Int, Package)
descriptionOf inputs candidate = withMemo (memoDescriptionOf inputs candidate)

-- | Run a fetch against the memo of the search.
withMemo :: (Monad m) => StateT Memo m a -> Solve m a
withMemo action = do
  memo <- gets stMemo
  (result, memo') <- lift (runStateT action memo)
  modify' (\st -> st {stMemo = memo'})
  pure result

-- | The versions a variable can take: one for the root, two for a flag,
-- the candidates of a package.
universeOf :: (Monad m) => SolverInputs m -> SolverConfig -> Var -> Solve m (Set Version)
universeOf inputs config var =
  case var of
    VarRoot -> pure (Set.singleton rootVersion)
    VarFlag _ _ -> pure flagVersions
    VarPackage name -> Set.fromList . map candidateVersion <$> candidatesOf inputs config name

-- | The candidate of a package at a version. The first one when a
-- version is listed twice, as a local package before a Hackage release.
candidateAt :: (Monad m) => SolverInputs m -> SolverConfig -> PackageName -> Version -> Solve m Candidate
candidateAt inputs config name version = do
  candidates <- candidatesOf inputs config name
  case [candidate | candidate <- candidates, candidateVersion candidate == version] of
    candidate : _ -> pure candidate
    [] -> error ("solver: no candidate " <> unPackageName name <> "-" <> showVersion version)

-- | Unit propagation from a variable whose terms changed. A conflict goes
-- through resolution, and propagation continues from the variable the
-- learned incompatibility decides.
propagate :: (Monad m) => SolverConfig -> Var -> Solve m (Either SolveFailure ())
propagate config start = go (Set.singleton start)
  where
    go changed =
      case Set.minView changed of
        Nothing -> pure (Right ())
        Just (var, rest) -> do
          ids <- gets (maybe [] IntSet.toDescList . Map.lookup var . stIndex)
          step rest ids

    step changed [] = go changed
    step changed (incId : ids) = do
      inc <- lookupIncompat incId
      partial <- gets stPartial
      case relationOf partial inc of
        Satisfied -> do
          resolved <- resolveConflict config incId
          case resolved of
            Left failure -> pure (Left failure)
            Right rootCause -> do
              cause <- lookupIncompat rootCause
              partial' <- gets stPartial
              case relationOf partial' cause of
                AlmostSatisfied var term -> do
                  derive var term rootCause
                  go (Set.singleton var)
                relation -> error ("solver: the learned incompatibility is " <> show relation)
        AlmostSatisfied var term -> do
          derive var term incId
          step (Set.insert var changed) ids
        _ -> step changed ids

derive :: (Monad m) => Var -> Term -> IncompatId -> Solve m ()
derive var term cause =
  modify' (\st -> st {stPartial = assign (Assigned var (negateTerm term) (partialLevel (stPartial st)) (Just cause)) (stPartial st)})

-- | An incompatibility that no plan satisfies: no term, or the root alone.
isFailure :: Incompat -> Bool
isFailure inc =
  case Map.toList (incTerms inc) of
    [] -> True
    [(VarRoot, Positive _)] -> True
    _ -> False

-- | Combine the conflict with the causes of its satisfiers until it has one
-- term at the current decision level, go back to the level that leaves
-- that term open, and keep the result.
resolveConflict :: (Monad m) => SolverConfig -> IncompatId -> Solve m (Either SolveFailure IncompatId)
resolveConflict config conflictId = do
  conflicts <- gets ((+ 1) . stConflicts)
  modify' (\st -> st {stConflicts = conflicts})
  if conflicts > configMaxConflicts config
    then Left . ConflictLimit (configMaxConflicts config) <$> proofOf conflictId
    else go False conflictId
  where
    go isNew incId = do
      inc <- lookupIncompat incId
      if isFailure inc
        then Left . NoSolution <$> proofOf incId
        else do
          history <- gets (zip [0 :: Int ..] . reverse . partialHistory . stPartial)
          let satisfiedAt = firstSatisfying history
              satisfiers = [(each, eachTerm, fromMaybe (error "solver: a conflict with an open term") (satisfiedAt each eachTerm)) | (each, eachTerm) <- Map.toAscList (incTerms inc)]
              (var, term, (_, satisfier)) = last (sortOn (\(_, _, (index, _)) -> index) satisfiers)
              difference = intersectTerms (asgTerm satisfier) (negateTerm term)
              previousLevel =
                maximum
                  ( 1
                      : [asgLevel other | (otherVar, _, (_, other)) <- satisfiers, otherVar /= var]
                        <> [asgLevel earlier | not (isEmptyTerm difference), Just (_, earlier) <- [satisfiedAt var (negateTerm difference)]]
                  )
          case asgCause satisfier of
            Just causeId | previousLevel == asgLevel satisfier -> do
              cause <- lookupIncompat causeId
              let others = Map.unionWith intersectTerms (Map.delete var (incTerms inc)) (Map.delete var (incTerms cause))
                  terms = if isEmptyTerm difference then others else Map.insert var (negateTerm difference) others
              newId <- store (Incompat (dropRoot terms) (KindDerived incId causeId))
              go True newId
            _ -> do
              modify' (\st -> st {stPartial = backtrackTo previousLevel (stPartial st)})
              when isNew (indexIncompat incId)
              pure (Right incId)

    -- The root is always in the plan, so a positive root term adds
    -- nothing to an incompatibility with other terms.
    dropRoot terms
      | Map.size terms > 1, Just (Positive _) <- Map.lookup VarRoot terms = Map.delete VarRoot terms
      | otherwise = terms

-- | The first assignment at which the terms of a variable so far satisfy
-- a term, with its position in the history.
firstSatisfying :: [(Int, Assigned)] -> Var -> Term -> Maybe (Int, Assigned)
firstSatisfying history var term = go Nothing history
  where
    go _ [] = Nothing
    go acc ((index, assigned) : rest)
      | asgVar assigned /= var = go acc rest
      | otherwise =
          let acc' = maybe (asgTerm assigned) (intersectTerms (asgTerm assigned)) acc
           in if subsetOf acc' term then Just (index, assigned) else go (Just acc') rest

-- | Pick the undecided variable with the fewest versions left and a version
-- for it, add the incompatibilities of that version, and decide it unless
-- one of them already rules it out. The result is the variable to
-- propagate from, or 'Nothing' when every variable in the plan is decided.
--
-- The flags of a package are decided before any other package, as part
-- of the choice of the package. A conflict between a flag at its default
-- and a package decided later then changes the package, and the flag only
-- when no version of the later packages helps.
decide :: (Monad m) => SolverInputs m -> SolverConfig -> Solve m (Maybe Var)
decide inputs config = do
  partial <- gets stPartial
  let pending =
        [ ((isPackage var, Set.size allowed, var), var, allowed)
        | (var, Positive allowed) <- Map.toAscList (partialTerms partial),
          not (Map.member var (partialDecisions partial))
        ]
      isPackage var = case var of
        VarPackage _ -> True
        _ -> False
  case sortOn (\(key, _, _) -> key) pending of
    [] -> pure Nothing
    (_, var, allowed) : _ -> do
      version <- chooseVersion inputs config var allowed
      expand inputs config var version
      ids <- gets (maybe [] IntSet.toDescList . Map.lookup var . stIndex)
      partial' <- gets stPartial
      let trial = decideIn var version partial'
      satisfied <- or <$> mapM (fmap ((== Satisfied) . relationOf trial) . lookupIncompat) ids
      unless satisfied $
        modify' (\st -> st {stPartial = decideIn var version (stPartial st)})
      pure (Just var)

-- | The version to try first: the preferred one when the lock has one,
-- else the newest that is not deprecated, else the newest.
chooseVersion :: (Monad m) => SolverInputs m -> SolverConfig -> Var -> Set Version -> Solve m Version
chooseVersion inputs config var allowed =
  case var of
    VarRoot -> pure rootVersion
    VarFlag name flag -> do
      decisions <- gets (partialDecisions . stPartial)
      preferred <- case Map.lookup name (configPreferences config) >>= lookupFlagAssignment flag . preferredFlags of
        Just value -> pure value
        Nothing -> do
          -- The default of the flag in the decided version of the package.
          case Map.lookup (VarPackage name) decisions of
            Just version -> do
              candidate <- candidateAt inputs config name version
              (_, gpd) <- descriptionOf inputs candidate
              pure (fromMaybe True (lookup flag [(flagName f, flagDefault f) | f <- packageFlags gpd]))
            Nothing -> pure True
      pure (if Set.member (flagVersion preferred) allowed then flagVersion preferred else flagVersion (not preferred))
    VarPackage name -> do
      candidates <- candidatesOf inputs config name
      let deprecated = Set.fromList [candidateVersion candidate | candidate <- candidates, candidateDeprecated candidate]
          fresh = Set.difference allowed deprecated
          preferred = [version | Just preference <- [Map.lookup name (configPreferences config)], let version = preferredVersion preference, Set.member version allowed]
      pure $ case preferred of
        version : _ -> version
        []
          | not (Set.null fresh) -> Set.findMax fresh
          | not (Set.null allowed) -> Set.findMax allowed
          | otherwise -> error ("solver: no version left for " <> unPackageName name)

-- | Put the incompatibilities of one version of a variable in the store,
-- once.
expand :: (Monad m) => SolverInputs m -> SolverConfig -> Var -> Version -> Solve m ()
expand inputs config var version = do
  done <- gets (Set.member (var, version) . stExpanded)
  unless done $ do
    modify' (\st -> st {stExpanded = Set.insert (var, version) (stExpanded st)})
    case var of
      VarFlag _ _ -> pure ()
      VarRoot ->
        forM_ ([(alias config name, anyVersion) | name <- Map.keys (configRoots config)] <> [(alias config name, range) | (name, range) <- configGoals config]) $ \(name, range) -> do
          universe <- universeOf inputs config (VarPackage name)
          let wanted = Set.filter (`withinRange` range) universe
          addIncompat
            ( Incompat
                (Map.fromList ((VarRoot, Positive (Set.singleton rootVersion)) : [(VarPackage name, Negative wanted) | not (Set.null wanted)]))
                (KindRootNeeds name range (Set.null universe))
            )
      VarPackage name -> expandPackage inputs config name version

expandPackage :: (Monad m) => SolverInputs m -> SolverConfig -> PackageName -> Version -> Solve m ()
expandPackage inputs config name version = do
  candidate <- candidateAt inputs config name version
  (_, gpd) <- descriptionOf inputs candidate
  let root = Map.lookup name (configRoots config)
      setup = flagSetup config name root gpd
      self = (VarPackage name, Positive (Set.singleton version))
  forM_ (setupSearched setup) $ \flag ->
    addIncompat (Incompat (Map.fromList [self, (VarFlag name flag, Negative flagVersions)]) (KindFlag name flag))
  -- The dependencies under every assignment of the searched flags, grouped
  -- by dependency and range. A flag that no assignment of the group
  -- depends on is left out of its terms.
  let assignments = sequence [[(flag, False), (flag, True)] | flag <- setupSearched setup]
      grouped =
        Map.fromListWith
          (\(range, new) (_, old) -> (range, old <> new))
          [ ((dependency, showVersionRange range), (range, [Map.fromList flags]))
          | flags <- assignments,
            (dependency, range) <- Map.toAscList (candidateDependencies (configPlatform config) (configAliases config) (completeFlags setup flags) root gpd)
          ]
  forM_ (Map.toAscList grouped) $ \((dependency, _), (range, under)) -> do
    universe <- universeOf inputs config (VarPackage dependency)
    let wanted = Set.filter (`withinRange` range) universe
        relevant = [flag | flag <- setupSearched setup, not (all (\flags -> Map.adjust not flag flags `elem` under) under)]
        conditions = nub [Map.restrictKeys flags (Set.fromList relevant) | flags <- under]
    forM_ conditions $ \condition ->
      addDependency
        name
        version
        (Map.fromList [(VarFlag name flag, Positive (Set.singleton (flagVersion value))) | (flag, value) <- Map.toAscList condition])
        dependency
        range
        wanted
        (Set.null universe)

-- | Add a dependency incompatibility, or widen the versions of the one
-- that the other versions of the package added for the same dependency.
addDependency :: (Monad m) => PackageName -> Version -> Map Var Term -> PackageName -> VersionRange -> Set Version -> Bool -> Solve m ()
addDependency name version flagTerms dependency range wanted unknown = do
  let dependencyTerm = if Set.null wanted then Nothing else Just (Negative wanted)
      key = (name, flagTerms, dependency, dependencyTerm, showVersionRange range)
  existing <- gets (Map.lookup key . stMerge)
  newId <- case existing of
    Just oldId -> do
      old <- lookupIncompat oldId
      let versions = case Map.lookup (VarPackage name) (incTerms old) of
            Just (Positive known) -> known
            _ -> Set.empty
      unindexIncompat oldId
      addIncompat old {incTerms = Map.insert (VarPackage name) (Positive (Set.insert version versions)) (incTerms old)}
    Nothing ->
      addIncompat
        ( Incompat
            (Map.unions [Map.singleton (VarPackage name) (Positive (Set.singleton version)), flagTerms, maybe Map.empty (Map.singleton (VarPackage dependency)) dependencyTerm])
            (KindDependency dependency range unknown)
        )
  modify' (\st -> st {stMerge = Map.insert key newId (stMerge st)})

-- | The assignments of the decided packages.
extractSolution :: (Monad m) => SolverInputs m -> SolverConfig -> Solve m Solution
extractSolution inputs config = do
  decisions <- gets (partialDecisions . stPartial)
  assignments <- forM [(name, version) | (VarPackage name, version) <- Map.toAscList decisions] $ \(name, version) -> do
    candidate <- candidateAt inputs config name version
    (revision, gpd) <- descriptionOf inputs candidate
    let root = Map.lookup name (configRoots config)
        setup = flagSetup config name root gpd
        flags =
          completeFlags
            setup
            [ (flag, maybe (setupDefaults setup Map.! flag) (== flagOn) (Map.lookup (VarFlag name flag) decisions))
            | flag <- setupSearched setup
            ]
    pure
      ( name,
        Assignment
          { assignmentVersion = version,
            assignmentRevision = revision,
            assignmentFlags = flags,
            assignmentSource = candidateSource candidate,
            assignmentDependencies = candidateDependencies (configPlatform config) (configAliases config) flags root gpd
          }
      )
  pure (Map.fromList assignments)

proofOf :: (Monad m) => IncompatId -> Solve m Proof
proofOf incId = do
  st <- get
  pure
    Proof
      { proofIncompatibilities = stIncompats st,
        proofRoot = incId,
        proofUniverses = Map.map (Set.toAscList . Set.fromList . map candidateVersion) (memoCandidates (stMemo st))
      }

-- | Write the derivation of the proof's incompatibility, as Dart's @pub@
-- does: one line per derived incompatibility, with the external facts
-- inline, and a number for a derived incompatibility that two derivations
-- share.
explain :: Proof -> [String]
explain proof =
  case incKind rootInc of
    KindDerived _ _ -> reverse (writerLines (execState (visit (proofRoot proof) False) (Writer [] IntMap.empty 1)))
    _ -> [capitalise (external rootInc) <> "."]
  where
    rootInc = incompat (proofRoot proof)
    incompat incId = proofIncompatibilities proof IntMap.! incId

    -- How many derivations use each derived incompatibility.
    uses = countUses IntMap.empty (proofRoot proof)
    countUses counts incId =
      case IntMap.lookup incId counts of
        Just count -> IntMap.insert incId (count + 1) counts
        Nothing ->
          case incKind (incompat incId) of
            KindDerived left right -> countUses (countUses (IntMap.insert incId (1 :: Int) counts) left) right
            _ -> counts

    isDerived incId = case incKind (incompat incId) of
      KindDerived _ _ -> True
      _ -> False

    -- A derived incompatibility whose two causes are external.
    isSingleLine incId = case incKind (incompat incId) of
      KindDerived left right -> not (isDerived left) && not (isDerived right)
      _ -> False

    conclusionOf incId =
      let inc = incompat incId
       in if isFailure inc then "no plan exists" else describe proof inc

    numberOf incId = gets (IntMap.lookup incId . writerNumbers)

    visit incId isConclusion =
      case incKind (incompat incId) of
        KindDerived left right -> visitDerived incId isConclusion left right (conclusionOf incId)
        _ -> write incId isConclusion (capitalise (external (incompat incId)) <> ".")

    visitDerived incId isConclusion left right conclusion =
      case (isDerived left, isDerived right) of
        (False, False) ->
          write incId isConclusion ("Because " <> external (incompat left) <> " and " <> external (incompat right) <> ", " <> conclusion <> ".")
        (True, True) -> do
          leftNumber <- numberOf left
          rightNumber <- numberOf right
          case (leftNumber, rightNumber) of
            (Just a, Just b) ->
              write incId isConclusion ("Because " <> describe proof (incompat left) <> " (" <> show a <> ") and " <> describe proof (incompat right) <> " (" <> show b <> "), " <> conclusion <> ".")
            (Just a, Nothing) -> do
              visit right False
              write incId isConclusion ("And because " <> describe proof (incompat left) <> " (" <> show a <> "), " <> conclusion <> ".")
            (Nothing, Just b) -> do
              visit left False
              write incId isConclusion ("And because " <> describe proof (incompat right) <> " (" <> show b <> "), " <> conclusion <> ".")
            (Nothing, Nothing)
              | isSingleLine left -> do
                  visit right False
                  visit left False
                  write incId isConclusion ("Thus, " <> conclusion <> ".")
              | isSingleLine right -> do
                  visit left False
                  visit right False
                  write incId isConclusion ("Thus, " <> conclusion <> ".")
              | otherwise -> do
                  visit left True
                  visit right False
                  number <- numberOf left
                  write incId isConclusion ("And because " <> describe proof (incompat left) <> maybe "" (\n -> " (" <> show n <> ")") number <> ", " <> conclusion <> ".")
        (True, False) -> derivedAndExternal incId isConclusion left right conclusion
        (False, True) -> derivedAndExternal incId isConclusion right left conclusion

    derivedAndExternal incId isConclusion derived ext conclusion = do
      number <- numberOf derived
      case number of
        Just n ->
          write incId isConclusion ("Because " <> external (incompat ext) <> " and " <> describe proof (incompat derived) <> " (" <> show n <> "), " <> conclusion <> ".")
        Nothing ->
          -- A chain of one derived and one external cause reads as one
          -- line per step; the step before is folded into this line when
          -- it has no number of its own.
          case incKind (incompat derived) of
            KindDerived a b
              | isDerived a,
                not (isDerived b),
                IntMap.findWithDefault 0 derived uses < 2 -> do
                  visit a False
                  write incId isConclusion ("And because " <> external (incompat b) <> " and " <> external (incompat ext) <> ", " <> conclusion <> ".")
              | isDerived b,
                not (isDerived a),
                IntMap.findWithDefault 0 derived uses < 2 -> do
                  visit b False
                  write incId isConclusion ("And because " <> external (incompat a) <> " and " <> external (incompat ext) <> ", " <> conclusion <> ".")
            _ -> do
              visit derived False
              write incId isConclusion ("And because " <> external (incompat ext) <> ", " <> conclusion <> ".")

    write incId isConclusion line = do
      writer <- get
      let numbered = isConclusion || IntMap.findWithDefault 0 incId uses > 1
          number = writerNext writer
      put
        writer
          { writerLines = (if numbered then line <> " (" <> show number <> ")" else line) : writerLines writer,
            writerNumbers = if numbered then IntMap.insert incId number (writerNumbers writer) else writerNumbers writer,
            writerNext = if numbered then number + 1 else number
          }

    external inc =
      case incKind inc of
        KindRoot -> "the root must be in the plan"
        KindRootNeeds dependency range unknown -> "the root needs " <> dependencyText dependency range <> noneSuffix inc dependency unknown
        KindConstraint dependency range -> "a constraint needs " <> dependencyText dependency range
        KindDependency dependency range unknown -> dependentsText inc <> " needs " <> dependencyText dependency range <> noneSuffix inc dependency unknown
        KindFlag _ flag -> dependentsText inc <> " has the automatic flag " <> unFlagName flag
        KindDerived _ _ -> describe proof inc

    dependencyText dependency range
      | range == anyVersion = unPackageName dependency
      | otherwise = unPackageName dependency <> " " <> showVersionRange range

    noneSuffix inc dependency unknown
      | unknown = ", which is an unknown package"
      | Map.member (VarPackage dependency) (incTerms inc) = ""
      | otherwise = ", which no version of " <> unPackageName dependency <> " satisfies"

    -- The package and the flags of a dependency incompatibility.
    dependentsText inc =
      unwords
        ( [renderPositive proof var versions | (var@(VarPackage _), Positive versions) <- Map.toAscList (incTerms inc)]
            <> [(if versions == Set.singleton flagOn then "+" else "-") <> unFlagName flag | (VarFlag _ flag, Positive versions) <- Map.toAscList (incTerms inc)]
        )

data Writer = Writer
  { writerLines :: ![String],
    writerNumbers :: !(IntMap Int),
    writerNext :: !Int
  }

capitalise :: String -> String
capitalise [] = []
capitalise (c : rest) = toUpper c : rest

-- | A derived incompatibility as a sentence about its terms.
describe :: Proof -> Incompat -> String
describe proof inc =
  case (positives, negatives) of
    ([], []) -> "no plan exists"
    ([(VarRoot, _)], []) -> "no plan exists"
    ([positive], []) -> renderPos positive <> " is forbidden"
    ([], [negative]) -> renderNeg negative <> " is required"
    ([positive], [negative]) -> renderPos positive <> " requires " <> renderNeg negative
    ([first, second], []) -> renderPos first <> " is incompatible with " <> renderPos second
    (_, []) -> "one of " <> intercalate ", " (map renderPos positives) <> " must be false"
    ([], _) -> "one of " <> intercalate ", " (map renderNeg negatives) <> " must be true"
    _ -> "if " <> intercalate " and " (map renderPos positives) <> " then " <> intercalate " or " (map renderNeg negatives)
  where
    positives = [(var, versions) | (var, Positive versions) <- Map.toAscList (incTerms inc)]
    negatives = [(var, versions) | (var, Negative versions) <- Map.toAscList (incTerms inc)]
    renderPos = uncurry (renderPositive proof)
    renderNeg = uncurry (renderNegative proof)

-- | A positive term: the variable takes one of the versions.
renderPositive :: Proof -> Var -> Set Version -> String
renderPositive proof var versions =
  case var of
    VarRoot -> "the root"
    VarFlag name flag -> renderFlag name flag versions
    VarPackage name
      | Set.size versions > 1, Just universe <- Map.lookup name (proofUniverses proof), Set.fromList universe == versions -> "every version of " <> unPackageName name
      | otherwise -> renderPackageSet proof name versions

-- | A negative term as the thing it requires when negated.
renderNegative :: Proof -> Var -> Set Version -> String
renderNegative proof var versions =
  case var of
    VarRoot -> "the root"
    VarFlag name flag -> renderFlag name flag versions
    VarPackage name
      | Set.size versions > 1, Just universe <- Map.lookup name (proofUniverses proof), Set.fromList universe == versions -> unPackageName name
      | otherwise -> renderPackageSet proof name versions

renderFlag :: PackageName -> FlagName -> Set Version -> String
renderFlag name flag versions
  | versions == Set.singleton flagOn = unPackageName name <> " +" <> unFlagName flag
  | versions == Set.singleton flagOff = unPackageName name <> " -" <> unFlagName flag
  | otherwise = "the flag " <> unFlagName flag <> " of " <> unPackageName name

-- | A package at one version, or with a range over its known versions.
renderPackageSet :: Proof -> PackageName -> Set Version -> String
renderPackageSet proof name versions =
  case Set.toAscList versions of
    [version] -> unPackageName name <> "-" <> showVersion version
    _ -> unPackageName name <> " " <> renderVersionSet (Map.findWithDefault (Set.toAscList versions) name (proofUniverses proof)) versions

-- | A set of versions as a union of ranges over the known versions, so
-- that consecutive known versions join into one range.
renderVersionSet :: [Version] -> Set Version -> String
renderVersionSet universe versions =
  intercalate " || " (map run (runs universe) <> ["==" <> showVersion version | version <- Set.toAscList versions, version `notElem` universe])
  where
    runs [] = []
    runs (version : rest)
      | Set.member version versions =
          let (inside, outside) = span (`Set.member` versions) rest
           in (version : inside) : runs outside
      | otherwise = runs rest
    run inside =
      case (inside, reverse inside) of
        ([version], _) -> "==" <> showVersion version
        (first : _, final : _) -> ">=" <> showVersion first <> " && <=" <> showVersion final
        _ -> ""

-- | The automatic flags of a package that guard a @build-depends@ clause
-- in a component that contributes dependencies. Only these can change the
-- plan, so only these are searched.
searchableFlags :: (OS, Arch) -> Maybe Stanzas -> Package -> [FlagName]
searchableFlags _ root gpd =
  [ flagName flag
  | flag <- packageFlags gpd,
    not (flagManual flag),
    flagName flag `Set.member` guarding
  ]
  where
    guarding =
      Set.unions
        ( map guardingFlags (allLibraryTrees gpd)
            <> concat
              [ map guardingFlags (selectedExecutableTrees stanzas gpd)
                  <> [guardingFlags tree | stanzasTests stanzas, tree <- testSuiteTrees gpd]
                  <> [guardingFlags tree | stanzasBenchmarks stanzas, tree <- benchmarkTrees gpd]
              | Just stanzas <- [root]
              ]
        )

-- | The flags in the conditions of the branches under which some node
-- states a dependency.
guardingFlags :: Conditional BuildInfo -> Set.Set FlagName
guardingFlags tree =
  Set.unions (map branch (branches tree))
  where
    branch (Branch condition thenTree elseTree) =
      Set.unions
        ( [conditionFlags condition | hasDependencies thenTree || maybe False hasDependencies elseTree]
            <> [guardingFlags thenTree]
            <> [guardingFlags subtree | Just subtree <- [elseTree]]
        )
    hasDependencies subtree =
      not (null (dependencies (unconditional subtree)))
        || any (\(Branch _ t e) -> hasDependencies t || maybe False hasDependencies e) (branches subtree)

conditionFlags :: Condition -> Set.Set FlagName
conditionFlags condition =
  case condition of
    FlagValue flag -> Set.singleton flag
    Literal _ -> Set.empty
    OS _ -> Set.empty
    Arch _ -> Set.empty
    Impl _ _ -> Set.empty
    Not inner -> conditionFlags inner
    Or left right -> Set.union (conditionFlags left) (conditionFlags right)
    And left right -> Set.union (conditionFlags left) (conditionFlags right)

-- | All library trees. A flag can change which sub-libraries an install
-- builds, so the flag search examines each of them.
allLibraryTrees :: Package -> [Conditional BuildInfo]
allLibraryTrees gpd = [tree | Component (Library _) tree <- packageComponents gpd]

-- | The build infos of the components that contribute dependencies under
-- one flag assignment: the buildable libraries that an install builds, and for a root package its
-- executables and requested stanzas as well.
contributingBuildInfos :: (OS, Arch) -> FlagAssignment -> Maybe Stanzas -> Package -> [BuildInfo]
contributingBuildInfos (os, arch) flags root gpd =
  filter
    isBuildable
    ( map (merged . snd) (installedLibraryTrees evalCond gpd)
        <> concat
          [ map merged (selectedExecutableTrees stanzas gpd)
              <> [merged tree | stanzasTests stanzas, tree <- testSuiteTrees gpd]
              <> [merged tree | stanzasBenchmarks stanzas, tree <- benchmarkTrees gpd]
          | Just stanzas <- [root]
          ]
    )
  where
    evalCond = conditionEvaluatorIn (BuildContext os arch flags) gpd
    merged = collectMergedBuildInfo evalCond

-- | The packages a candidate needs under one flag assignment, after
-- aliasing, each with the intersection of the ranges the components
-- demand. A dependency on the package itself, which a sub-library or an
-- executable states, is not a dependency of the plan.
candidateDependencies :: (OS, Arch) -> Map PackageName PackageName -> FlagAssignment -> Maybe Stanzas -> Package -> Map PackageName VersionRange
candidateDependencies platform aliases flags root gpd =
  Map.map
    simplifyVersionRange
    ( Map.fromListWith
        intersectVersionRanges
        [ (resolved, range)
        | build <- contributingBuildInfos platform flags root gpd,
          Dependency packageName range _ <- dependencies build,
          let name = mkPackageName (T.unpack packageName)
              resolved = fromMaybe name (Map.lookup name aliases),
          resolved /= self
        ]
    )
  where
    self = fromMaybe (packageNameOf gpd) (Map.lookup (packageNameOf gpd) aliases)

-- | The build tools the contributing components ask for that the host
-- cannot run. Tools run on the build host, so they are checked against the
-- preprocessor table rather than solved against the target plan.
unknownBuildTools :: (OS, Arch) -> FlagAssignment -> Maybe Stanzas -> Package -> [String]
unknownBuildTools platform flags root gpd =
  Set.toAscList
    ( Set.fromList
        [ tool
        | build <- contributingBuildInfos platform flags root gpd,
          tool <- [T.unpack (toolName dependency) | dependency <- buildTools build],
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
              -- A recorded revision must be the one the candidate reads, so
              -- that the plan is the cabal file the lock was solved with.
              | maybe True (\wanted -> candidateRevision candidate == Just wanted) revision ->
                  pure (Right (Map.insert name (candidate, flags) chosen))
              | otherwise -> pure (Left (unPackageName name <> "-" <> showVersion version <> " cannot be read at revision " <> maybe "" show revision))
            [] -> pure (Left ("no candidate " <> unPackageName name <> "-" <> showVersion version))

    assignmentFor name (candidate, flags) = do
      (revision, gpd) <- inputsDescription inputs candidate
      let root = Map.lookup name (configRoots config)
          decided = Map.union (constrainedFlags config name) (Map.fromList (targetFlagOverrides (snd (configPlatform config)) (packageNameOf gpd)))
          full = mkFlagAssignment (Map.toAscList (Map.union decided (Map.fromList (unFlagAssignment flags))))
      pure
        Assignment
          { assignmentVersion = candidateVersion candidate,
            assignmentRevision = revision,
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
    -- A constraint limits the versions of a package. It does not add the
    -- package to the plan, as in the solver.
    constraintChecks solution =
      [ satisfied solution "a constraint" (alias config name) range
      | ConstraintVersion name range <- configConstraints config,
        Map.member (alias config name) solution
      ]
        <> [ if lookupFlagAssignment flag (assignmentFlags assignment) == Just value
               then Nothing
               else Just ("the constraint on the flag " <> unFlagName flag <> " of " <> unPackageName name <> " is not met")
           | ConstraintFlag name flag value <- configConstraints config,
             Just assignment <- [Map.lookup (alias config name) solution]
           ]
    dependencyChecks solution =
      [ satisfied solution (unPackageName name <> "-" <> showVersion (assignmentVersion assignment)) dependency range
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
          | otherwise -> Just (who <> " needs " <> unPackageName name <> " " <> showVersionRange range <> ", but " <> showVersion (assignmentVersion assignment) <> " is listed")
