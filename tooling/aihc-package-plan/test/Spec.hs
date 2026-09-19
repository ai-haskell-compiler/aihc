module Main (main) where

import Aihc.Hackage.IndexCache (IndexOptions (..), defaultIndexOptions, newHackageIndex)
import Aihc.PackagePlan
import Aihc.PackagePlan.Lock
import Aihc.PackagePlan.Solver
import Control.Exception (IOException, bracket, try)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Functor.Identity (Identity (..))
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Distribution.Package (PackageName, mkPackageName, unPackageName)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.System (Arch (..), OS (..))
import Distribution.Types.Flag (mkFlagAssignment, mkFlagName, unFlagAssignment, unFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, anyVersion)
import Hedgehog (Property, property, success)
import System.Directory (createDirectory, createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain . testGroup "aihc-package-plan" $
    [ testCase "flips an automatic flag when its default branch cannot be satisfied" test_flipsFlagUnderConflict,
      testCase "backtracks to an older version when the newest conflicts" test_backtracksOnVersionConflict,
      testCase "backjumps past choices a failure does not blame" test_backjumpsPastIrrelevantChoices,
      testCase "tries deprecated versions last" test_deprecatedLast,
      testCase "keeps the preferred version of a lock" test_prefersLockedVersion,
      testCase "maps boot library names to their standins" test_aliasesBootLibraries,
      testCase "reports the goal that ran out of candidates" test_reportsExhaustedGoal,
      testCase "reports an unknown package" test_reportsUnknownPackage,
      testCase "applies version and flag constraints" test_appliesConstraints,
      testCase "only searches flags that guard build-depends" test_searchableFlags,
      testCase "round-trips the lock file" test_lockRoundTrip,
      testCase "verifies a lock against the roots" test_verifiesLock,
      testCase "parses constraint arguments" test_parsesConstraints,
      testCase "plans local packages and honours a lock" test_plansLocalPackages,
      testProperty "Hedgehog options" prop_dummy
    ]

-- | Keep the repository Hedgehog options accepted by this test suite.
prop_dummy :: Property
prop_dummy = property success

-- | A pure package universe: every candidate with its cabal file.
type Universe = Map.Map PackageName [(Candidate, String)]

universe :: [(String, String, Bool, CandidateSource, String)] -> Universe
universe entries =
  Map.fromListWith
    (flip (<>))
    [ (mkPackageName name, [(Candidate (mkPackageName name) (version versionText) 0 deprecated source, cabal)])
    | (name, versionText, deprecated, source, cabal) <- entries
    ]

version :: String -> Version
version text = fromMaybe (error ("invalid test version " <> text)) (simpleParsec text)

range :: String -> VersionRange
range text = fromMaybe (error ("invalid test range " <> text)) (simpleParsec text)

parseCabal :: String -> GenericPackageDescription
parseCabal source =
  case snd (runParseResult (parseGenericPackageDescription (BSC.pack source))) of
    Right parsed -> parsed
    Left (_, errs) -> error ("failed to parse test cabal file: " <> show errs)

inputsFor :: Universe -> SolverInputs Identity
inputsFor packages =
  SolverInputs
    { inputsCandidates = \name _ -> Identity (map fst (Map.findWithDefault [] name packages)),
      inputsDescription = \candidate ->
        Identity
          ( case [cabal | (known, cabal) <- Map.findWithDefault [] (candidateName candidate) packages, known == candidate] of
              cabal : _ -> parseCabal cabal
              [] -> error ("no cabal file for " <> show candidate)
          )
    }

configFor :: [String] -> SolverConfig
configFor roots =
  SolverConfig
    { configPlatform = (Linux, X86_64),
      configAliases = Map.fromList [(mkPackageName "base", mkPackageName "aihc-base")],
      configConstraints = [],
      configPreferences = Map.empty,
      configRoots = Map.fromList [(mkPackageName root, noStanzas) | root <- roots],
      configGoals = [],
      configMaxBacktracks = 100
    }

solveWith :: Universe -> SolverConfig -> Either SolveFailure Solution
solveWith packages config = runIdentity (solve (inputsFor packages) config)

cabalFile :: String -> String -> [String] -> [String] -> String
cabalFile name versionText flags library =
  unlines
    ( [ "cabal-version: 2.2",
        "name: " <> name,
        "version: " <> versionText,
        "build-type: Simple"
      ]
        <> flags
        <> ["library", "  default-language: Haskell2010"]
        <> map ("  " <>) library
    )

core :: CandidateSource
core = CandidateCore "/core/aihc-base"

hackage :: CandidateSource
hackage = CandidateHackage

local :: CandidateSource
local = CandidateLocal "/work/root"

baseEntry :: (String, String, Bool, CandidateSource, String)
baseEntry = ("aihc-base", "4.21.2.0", False, core, cabalFile "aihc-base" "4.21.2.0" [] [])

chosen :: Solution -> [(String, String)]
chosen solution = [(unPackageName name, prettyShow (assignmentVersion assignment)) | (name, assignment) <- Map.toAscList solution]

flagsOf :: Solution -> String -> [(String, Bool)]
flagsOf solution name =
  [(unFlagName flag, value) | (flag, value) <- unFlagAssignment (assignmentFlags (solution Map.! mkPackageName name))]

expectSolution :: Either SolveFailure Solution -> IO Solution
expectSolution = either (assertFailure . renderSolveFailure) pure

-- The unix and filepath case from the design: the default branch of
-- unix's automatic os-string flag needs filepath below 1.5, the root needs
-- 1.5 or newer, and flipping the flag adds os-string instead.
test_flipsFlagUnderConflict :: Assertion
test_flipsFlagUnderConflict = do
  let packages =
        universe
          [ baseEntry,
            ("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: base, unix, filepath >=1.5"]),
            ( "unix",
              "2.8.8.0",
              False,
              hackage,
              cabalFile
                "unix"
                "2.8.8.0"
                ["flag os-string", "  default: False", "  manual: False"]
                [ "build-depends: base",
                  "if flag(os-string)",
                  "  build-depends: os-string >=2.0.0",
                  "else",
                  "  build-depends: filepath >=1.4.100.0 && <1.5.0.0"
                ]
            ),
            ("filepath", "1.5.5.0", False, hackage, cabalFile "filepath" "1.5.5.0" [] ["build-depends: base"]),
            ("filepath", "1.4.300.0", False, hackage, cabalFile "filepath" "1.4.300.0" [] ["build-depends: base"]),
            ("os-string", "2.0.11", False, hackage, cabalFile "os-string" "2.0.11" [] ["build-depends: base"])
          ]
  solution <- expectSolution (solveWith packages (configFor ["root"]))
  assertEqual
    "chosen versions"
    [("aihc-base", "4.21.2.0"), ("filepath", "1.5.5.0"), ("os-string", "2.0.11"), ("root", "0.1"), ("unix", "2.8.8.0")]
    (chosen solution)
  assertEqual "unix flips os-string" [("os-string", True)] (flagsOf solution "unix")
  assertEqual "filepath has no decided flags" [] (flagsOf solution "filepath")
  assertEqual "the root is local" (CandidateLocal "/work/root") (assignmentSource (solution Map.! mkPackageName "root"))

-- The process case: the root's automatic os-string flag defaults to off,
-- its default branch pins filepath below 1.5, and directory needs 1.5 or
-- newer, so the whole default branch is doomed. directory has the most
-- candidates, so it is decided last, below two packages that have nothing
-- to do with the conflict. Chronological backtracking re-derives the same
-- directory failure once per pair of those irrelevant versions and runs
-- out of backtracks; blaming filepath and the root instead skips straight
-- back to the flag. The 100-backtrack budget of "configFor" is the test.
test_backjumpsPastIrrelevantChoices :: Assertion
test_backjumpsPastIrrelevantChoices = do
  let noise name index =
        (name, show index <> ".0", False, hackage, cabalFile name (show index <> ".0") [] ["build-depends: base"])
      directoryEntry index =
        ("directory", "1.3." <> show index, False, hackage, cabalFile "directory" ("1.3." <> show index) [] ["build-depends: base, filepath >=1.5"])
      packages =
        universe
          ( [ baseEntry,
              ( "root",
                "0.1",
                False,
                local,
                cabalFile
                  "root"
                  "0.1"
                  ["flag os-string", "  default: False", "  manual: False"]
                  [ "build-depends: base, directory, noise-a, noise-b",
                    "if flag(os-string)",
                    "  build-depends: filepath >=1.5.0.0",
                    "else",
                    "  build-depends: filepath >=1.4.100.0 && <1.5.0.0"
                  ]
              ),
              ("filepath", "1.5.5.0", False, hackage, cabalFile "filepath" "1.5.5.0" [] ["build-depends: base"]),
              ("filepath", "1.4.300.0", False, hackage, cabalFile "filepath" "1.4.300.0" [] ["build-depends: base"])
            ]
              <> map (noise "noise-a") [1 .. 10 :: Int]
              <> map (noise "noise-b") [1 .. 10 :: Int]
              <> map directoryEntry [1 .. 12 :: Int]
          )
  solution <- expectSolution (solveWith packages (configFor ["root"]))
  assertEqual
    "chosen versions"
    [ ("aihc-base", "4.21.2.0"),
      ("directory", "1.3.12"),
      ("filepath", "1.5.5.0"),
      ("noise-a", "10.0"),
      ("noise-b", "10.0"),
      ("root", "0.1")
    ]
    (chosen solution)
  assertEqual "the root flips os-string" [("os-string", True)] (flagsOf solution "root")

-- The newest a needs c at 1, but b needs c at 2, so a goes back a version.
test_backtracksOnVersionConflict :: Assertion
test_backtracksOnVersionConflict = do
  let packages =
        universe
          [ baseEntry,
            ("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: a, b"]),
            ("a", "2.0", False, hackage, cabalFile "a" "2.0" [] ["build-depends: c ==1.*"]),
            ("a", "1.0", False, hackage, cabalFile "a" "1.0" [] ["build-depends: c"]),
            ("b", "1.0", False, hackage, cabalFile "b" "1.0" [] ["build-depends: c ==2.*"]),
            ("c", "2.0", False, hackage, cabalFile "c" "2.0" [] []),
            ("c", "1.0", False, hackage, cabalFile "c" "1.0" [] [])
          ]
  solution <- expectSolution (solveWith packages (configFor ["root"]))
  assertEqual "chosen versions" [("a", "1.0"), ("b", "1.0"), ("c", "2.0"), ("root", "0.1")] (chosen solution)

test_deprecatedLast :: Assertion
test_deprecatedLast = do
  let packages =
        universe
          [ ("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: x"]),
            ("x", "2.0", True, hackage, cabalFile "x" "2.0" [] []),
            ("x", "1.0", False, hackage, cabalFile "x" "1.0" [] [])
          ]
  solution <- expectSolution (solveWith packages (configFor ["root"]))
  assertEqual "the non-deprecated version wins" [("root", "0.1"), ("x", "1.0")] (chosen solution)
  let onlyDeprecated = universe [("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: x"]), ("x", "2.0", True, hackage, cabalFile "x" "2.0" [] [])]
  deprecated <- expectSolution (solveWith onlyDeprecated (configFor ["root"]))
  assertEqual "a deprecated version is still a candidate" [("root", "0.1"), ("x", "2.0")] (chosen deprecated)

test_prefersLockedVersion :: Assertion
test_prefersLockedVersion = do
  let packages =
        universe
          [ ("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: x"]),
            ("x", "2.0", False, hackage, cabalFile "x" "2.0" [] []),
            ("x", "1.0", False, hackage, cabalFile "x" "1.0" [] [])
          ]
      preferences = Map.fromList [(mkPackageName "x", Preference (version "1.0") (Just 0) (mkFlagAssignment []))]
  solution <- expectSolution (solveWith packages (configFor ["root"]) {configPreferences = preferences})
  assertEqual "the locked version is kept" [("root", "0.1"), ("x", "1.0")] (chosen solution)
  fresh <- expectSolution (solveWith packages (configFor ["root"]))
  assertEqual "without the lock the newest wins" [("root", "0.1"), ("x", "2.0")] (chosen fresh)

test_aliasesBootLibraries :: Assertion
test_aliasesBootLibraries = do
  let packages =
        universe
          [ baseEntry,
            ("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: base >=4.21 && <4.22"])
          ]
  solution <- expectSolution (solveWith packages (configFor ["root"]))
  assertEqual "base resolves to the standin" [("aihc-base", "4.21.2.0"), ("root", "0.1")] (chosen solution)
  assertEqual "the dependency is recorded under the standin" [mkPackageName "aihc-base"] (Map.keys (assignmentDependencies (solution Map.! mkPackageName "root")))
  let tooOld = universe [baseEntry, ("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: base <4.20"])]
  case solveWith tooOld (configFor ["root"]) of
    Right _ -> assertFailure "a range that excludes the standin must fail"
    Left failure -> assertBool ("names the standin: " <> renderSolveFailure failure) ("No version of aihc-base satisfies <4.20" `isInfixOf` renderSolveFailure failure)

test_reportsExhaustedGoal :: Assertion
test_reportsExhaustedGoal = do
  let packages =
        universe
          [ ("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: a, c ==2.*"]),
            ("a", "2.0", False, hackage, cabalFile "a" "2.0" [] ["build-depends: c ==1.*"]),
            ("a", "1.0", False, hackage, cabalFile "a" "1.0" [] ["build-depends: c <1"]),
            ("c", "2.0", False, hackage, cabalFile "c" "2.0" [] []),
            ("c", "1.0", False, hackage, cabalFile "c" "1.0" [] [])
          ]
  case solveWith packages (configFor ["root"]) of
    Right solution -> assertFailure ("expected a failure, got " <> show (chosen solution))
    Left failure -> do
      let rendered = renderSolveFailure failure
      assertBool ("names the exhausted goal:\n" <> rendered) ("Every version of a was rejected; it is needed by root-0.1" `isInfixOf` rendered)
      assertBool ("names the rejected candidate:\n" <> rendered) ("a-2.0 needs c >=1 && <2, but c-2.0 is chosen" `isInfixOf` rendered)
      assertBool ("starts where the choice was:\n" <> rendered) (take 18 rendered == "Every version of a")
      assertBool ("names the other candidate:\n" <> rendered) ("a-1.0 needs c <1, but c-2.0 is chosen" `isInfixOf` rendered)

test_reportsUnknownPackage :: Assertion
test_reportsUnknownPackage = do
  let packages = universe [("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: nowhere"])]
  case solveWith packages (configFor ["root"]) of
    Right _ -> assertFailure "an unknown package must fail"
    Left failure -> assertBool (renderSolveFailure failure) ("Unknown package nowhere, needed by root-0.1" `isInfixOf` renderSolveFailure failure)

test_appliesConstraints :: Assertion
test_appliesConstraints = do
  let packages =
        universe
          [ ("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: x"]),
            ("x", "2.0", False, hackage, cabalFile "x" "2.0" ["flag fast", "  default: True", "  manual: False"] ["if flag(fast)", "  build-depends: y"]),
            ("x", "1.0", False, hackage, cabalFile "x" "1.0" [] []),
            ("y", "1.0", False, hackage, cabalFile "y" "1.0" [] [])
          ]
      constrained = (configFor ["root"]) {configConstraints = [ConstraintVersion (mkPackageName "x") (range "<2")]}
  solution <- expectSolution (solveWith packages constrained)
  assertEqual "the version constraint holds" [("root", "0.1"), ("x", "1.0")] (chosen solution)
  let flagged = (configFor ["root"]) {configConstraints = [ConstraintFlag (mkPackageName "x") (mkFlagName "fast") False]}
  withFlag <- expectSolution (solveWith packages flagged)
  assertEqual "the flag constraint drops y" [("root", "0.1"), ("x", "2.0")] (chosen withFlag)
  assertEqual "the constrained flag is recorded" [("fast", False)] (flagsOf withFlag "x")
  unconstrained <- expectSolution (solveWith packages (configFor ["root"]))
  assertEqual "by default the flag stays on" [("root", "0.1"), ("x", "2.0"), ("y", "1.0")] (chosen unconstrained)
  assertEqual "a searched flag at its default is recorded" [("fast", True)] (flagsOf unconstrained "x")

test_searchableFlags :: Assertion
test_searchableFlags = do
  let gpd =
        parseCabal
          ( cabalFile
              "x"
              "1.0"
              [ "flag deps",
                "  default: True",
                "  manual: False",
                "flag modules",
                "  default: True",
                "  manual: False",
                "flag manual-deps",
                "  default: True",
                "  manual: True",
                "flag tests",
                "  default: True",
                "  manual: False"
              ]
              [ "if flag(deps)",
                "  build-depends: y",
                "if flag(modules)",
                "  exposed-modules: X.Fast",
                "if flag(manual-deps)",
                "  build-depends: z"
              ]
              <> unlines
                [ "test-suite spec",
                  "  type: exitcode-stdio-1.0",
                  "  main-is: Spec.hs",
                  "  default-language: Haskell2010",
                  "  if flag(tests)",
                  "    build-depends: hspec"
                ]
          )
  assertEqual "only the automatic flag guarding library deps" ["deps"] (map unFlagName (searchableFlags (Linux, X86_64) Nothing gpd))
  assertEqual "a root with tests also searches the test flag" ["deps", "tests"] (map unFlagName (searchableFlags (Linux, X86_64) (Just (Stanzas True False)) gpd))
  assertEqual
    "the library dependencies under the defaults"
    ["y", "z"]
    (map unPackageName (Map.keys (candidateDependencies (Linux, X86_64) Map.empty (mkFlagAssignment []) Nothing gpd)))
  assertEqual
    "the dependencies with tests requested"
    ["hspec", "y", "z"]
    (map unPackageName (Map.keys (candidateDependencies (Linux, X86_64) Map.empty (mkFlagAssignment []) (Just (Stanzas True False)) gpd)))

test_lockRoundTrip :: Assertion
test_lockRoundTrip = do
  let unixEntry = LockEntry (mkPackageName "unix") (version "2.8.8.0") LockHackage (Just 1) (mkFlagAssignment [(mkFlagName "os-string", True)])
      entries =
        [ unixEntry,
          LockEntry (mkPackageName "aihc-base") (version "4.21.2.0") LockCore Nothing (mkFlagAssignment []),
          LockEntry (mkPackageName "root") (version "0.1") LockLocal Nothing (mkFlagAssignment [])
        ]
      lock =
        LockFile
          { lockCompiler = "ghc-9.12.4",
            lockIndexState = Just "2026-09-17T00:00:00Z",
            lockPlatforms = Map.fromList [(platformKey Linux X86_64, entries), (platformKey OSX AArch64, take 1 entries)]
          }
      rendered = renderLockFile lock
  assertEqual "the platform key" "linux-x86_64" (platformKey Linux X86_64)
  assertBool ("one package per line:\n" <> BLC.unpack rendered) ("      {\"name\": \"aihc-base\", \"version\": \"4.21.2.0\", \"source\": \"core\"}," `isInfixOf` BLC.unpack rendered)
  case parseLockFile rendered of
    Left problem -> assertFailure ("failed to parse the rendered lock: " <> problem)
    Right parsed -> do
      assertEqual "compiler" (lockCompiler lock) (lockCompiler parsed)
      assertEqual "index state" (lockIndexState lock) (lockIndexState parsed)
      assertEqual "entries sorted by name" (Map.map (map lockName) (lockPlatforms parsed)) (Map.fromList [(platformKey Linux X86_64, map mkPackageName ["aihc-base", "root", "unix"]), (platformKey OSX AArch64, [mkPackageName "unix"])])
      assertEqual "the unix entry" (Just unixEntry) (lookup (mkPackageName "unix") [(lockName entry, entry) | entry <- lockPlatforms parsed Map.! platformKey Linux X86_64])
  assertBool "an unsupported format is rejected" (either ("unsupported lock format" `isInfixOf`) (const False) (parseLockFile (BLC.pack "{\"format\": 99, \"compiler\": \"ghc-9.12.4\", \"platforms\": {}}")))

test_verifiesLock :: Assertion
test_verifiesLock = do
  let packages =
        universe
          [ baseEntry,
            ("root", "0.1", False, local, cabalFile "root" "0.1" [] ["build-depends: base, x >=1 && <2"]),
            ("x", "2.0", False, hackage, cabalFile "x" "2.0" [] ["build-depends: base"]),
            ("x", "1.0", False, hackage, cabalFile "x" "1.0" [] ["build-depends: base"]),
            ("y", "1.0", False, hackage, cabalFile "y" "1.0" [] [])
          ]
      config = configFor ["root"]
      recorded names = Map.fromList [(mkPackageName name, (version v, r, mkFlagAssignment [])) | (name, v, r) <- names]
      verify names = runIdentity (verifySolution (inputsFor packages) config (recorded names))
  case verify [("aihc-base", "4.21.2.0", Nothing), ("root", "0.1", Nothing), ("x", "1.0", Just 0)] of
    Left problem -> assertFailure ("a valid lock was rejected: " <> problem)
    Right solution -> assertEqual "the locked versions" [("aihc-base", "4.21.2.0"), ("root", "0.1"), ("x", "1.0")] (chosen solution)
  assertEqual
    "a missing root"
    (Left "the root needs root, which is not listed")
    (fmap chosen (verify [("aihc-base", "4.21.2.0", Nothing), ("x", "1.0", Just 0)]))
  assertEqual
    "a dependency out of range"
    (Left "root-0.1 needs x >=1 && <2, but 2.0 is listed")
    (fmap chosen (verify [("aihc-base", "4.21.2.0", Nothing), ("root", "0.1", Nothing), ("x", "2.0", Just 0)]))
  assertEqual
    "a version the index does not have"
    (Left "no candidate x-0.5")
    (fmap chosen (verify [("aihc-base", "4.21.2.0", Nothing), ("root", "0.1", Nothing), ("x", "0.5", Just 0)]))
  assertEqual
    "an entry nothing reaches"
    (Left "y is no longer needed")
    (fmap chosen (verify [("aihc-base", "4.21.2.0", Nothing), ("root", "0.1", Nothing), ("x", "1.0", Just 0), ("y", "1.0", Just 0)]))
  assertEqual
    "a constraint the lock contradicts"
    (Left "a constraint needs x >=2, but 1.0 is listed")
    (fmap chosen (runIdentity (verifySolution (inputsFor packages) config {configConstraints = [ConstraintVersion (mkPackageName "x") (range ">=2")]} (recorded [("aihc-base", "4.21.2.0", Nothing), ("root", "0.1", Nothing), ("x", "1.0", Just 0)]))))

test_parsesConstraints :: Assertion
test_parsesConstraints = do
  assertEqual "a version range" (Right [ConstraintVersion (mkPackageName "filepath") (range "==1.5.5.0")]) (parseConstraint "filepath ==1.5.5.0")
  assertEqual "a bare name is any version" (Right [ConstraintVersion (mkPackageName "filepath") anyVersion]) (parseConstraint "filepath")
  assertEqual
    "flags"
    (Right [ConstraintFlag (mkPackageName "unix") (mkFlagName "os-string") True, ConstraintFlag (mkPackageName "unix") (mkFlagName "old") False])
    (parseConstraint "unix +os-string -old")
  assertBool "garbage is rejected" (either (const True) (const False) (parseConstraint "unix >>> 1"))

-- A root beside a sibling package plans without the Hackage index: the
-- siblings and the core libraries are the only candidates. No lock is
-- written for such a plan, but a lock that is present is read, and
-- @--locked@ takes a valid one and refuses a stale one.
test_plansLocalPackages :: Assertion
test_plansLocalPackages =
  withTempDir "aihc-package-plan" $ \directory -> do
    let root = directory </> "root"
        dep = directory </> "dep"
        lockFile = root </> lockFileName
    createDirectoryIfMissing True root
    createDirectoryIfMissing True dep
    writeFile (root </> "root.cabal") (cabalFile "root" "0.1" [] ["build-depends: base >=4.21, dep >=1"])
    writeFile (dep </> "dep.cabal") (cabalFile "dep" "1.0" [] ["build-depends: base"])
    index <- newHackageIndex defaultIndexOptions {indexAllowNetwork = False, indexVerbose = False}
    let request mode =
          PlanRequest
            { requestRoots = [RootLocal root],
              requestGoals = [],
              requestWorkspaces = [],
              requestPlatform = (Linux, X86_64),
              requestConstraints = [],
              requestLockFile = lockFile,
              requestLockMode = mode,
              requestIndex = index,
              requestVerbose = const (pure ())
            }
    planned <- planPackages (request LockNormal)
    rootPlan <- case plannedRoots planned of
      [plan] -> pure plan
      plans -> assertFailure ("expected one root, got " <> show (length plans))
    assertEqual "the root is local" PlanLocal (planOrigin rootPlan)
    assertEqual
      "the dependencies of the root, with the implicit aihc-prim"
      ["aihc-base", "aihc-prim", "dep"]
      (map (unPackageName . planName) (planDependencyPlans rootPlan))
    assertEqual "the sibling is local" [PlanLocal] [planOrigin plan | plan <- planDependencyPlans rootPlan, planName plan == mkPackageName "dep"]
    assertEqual "core libraries come from core-libs" [PlanCore] [planOrigin plan | plan <- planDependencyPlans rootPlan, planName plan == mkPackageName "aihc-base"]
    written <- doesFileExist lockFile
    assertBool "no lock is written for a plan without Hackage packages" (not written)
    locked <- try (planPackages (request LockLocked))
    case locked of
      Right _ -> assertFailure "--locked must refuse an absent lock"
      Left err -> assertBool (show err) ("--locked forbids solving" `isInfixOf` show (err :: IOException))
    let entry name versionText source = LockEntry (mkPackageName name) (version versionText) source Nothing (mkFlagAssignment [])
        lock entries = LockFile "ghc-9.12.4" Nothing (Map.fromList [(platformKey Linux X86_64, entries)])
        coreEntries = [entry "aihc-base" "4.21.2.0" LockCore, entry "aihc-prim" "0.13.0" LockCore, entry "aihc-rts" "1.0.2" LockCore]
    writeLockFile lockFile (lock (coreEntries <> [entry "dep" "1.0" LockLocal, entry "root" "0.1" LockLocal]))
    fromLock <- planPackages (request LockLocked)
    assertEqual "the locked plan has the same packages" (Map.keys (plannedSolution planned)) (Map.keys (plannedSolution fromLock))
    writeLockFile lockFile (lock (coreEntries <> [entry "dep" "0.9" LockLocal, entry "root" "0.1" LockLocal]))
    stale <- try (planPackages (request LockLocked))
    case stale of
      Right _ -> assertFailure "--locked must refuse a stale lock"
      Left err -> assertBool (show err) ("is stale" `isInfixOf` show (err :: IOException))
    -- Without --locked the stale lock is solved around; the local plan is
    -- the same as before and still writes no lock.
    again <- planPackages (request LockNormal)
    assertEqual "the plan after a stale lock" (Map.keys (plannedSolution planned)) (Map.keys (plannedSolution again))

-- | A fresh directory under the system temporary directory, removed
-- afterwards. The temporary file only reserves a unique name.
withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tmp <- getTemporaryDirectory
  bracket
    ( do
        (path, handle) <- openTempFile tmp prefix
        hClose handle
        removeFile path
        createDirectory path
        pure path
    )
    removeDirectoryRecursive
    action
