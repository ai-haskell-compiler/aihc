module Main (main) where

import Aihc.Hackage.Package (Version, parseVersionString, showVersion)
import Aihc.Hackage.Query
  ( QueryOptions (..),
    QueryVersion (..),
    cabalFileRevision,
    defaultQueryOptions,
    getQueryCacheDir,
    newHackageQueries,
    parseVersionsJson,
    queryCabalFile,
    queryPackageVersions,
  )
import Control.Exception (IOException, bracket, try)
import Data.ByteString.Char8 qualified as BSC
import Data.List (isInfixOf)
import Hedgehog (Property, property, success)
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain . testGroup "aihc-hackage-fetch" $
    [ testCase "reads the versions Hackage lists for a package" test_readsVersionsJson,
      testCase "reads the revision a cabal file is at" test_readsCabalFileRevision,
      testCase "answers from the cache without the network" test_answersFromCache,
      testProperty "Hedgehog options" prop_dummy
    ]

-- | Keep the repository Hedgehog options accepted by this test suite.
prop_dummy :: Property
prop_dummy = property success

-- The body of @\/package\/NAME.json@: one field per version, whose value
-- says whether the maintainer deprecates it. The versions must come back
-- newest first, because that is the order the planner tries them in, and a
-- deprecated version must be marked, because the planner tries those last.
test_readsVersionsJson :: Assertion
test_readsVersionsJson = do
  let body = BSC.pack "{\"1.0\":\"normal\",\"1.10.0\":\"deprecated\",\"1.2\":\"normal\"}"
  versions <- either (assertFailure . ("failed to read the versions: " <>)) pure (parseVersionsJson body)
  assertEqual
    "newest first, with the deprecated version marked"
    [("1.10.0", True), ("1.2", False), ("1.0", False)]
    [(showVersion (queryVersionVersion listed), queryVersionDeprecated listed) | listed <- versions]
  assertBool
    "a body that is not an object of versions is refused"
    (either (const True) (const False) (parseVersionsJson (BSC.pack "[\"1.0\"]")))

-- The revision is what the lock file pins and what joins a package's
-- identity in the store, so reading it from the wrong field, or defaulting
-- a revised file to zero, pins the wrong cabal file.
test_readsCabalFileRevision :: Assertion
test_readsCabalFileRevision = do
  assertEqual
    "the first upload of a release carries no x-revision field"
    0
    (cabalFileRevision (BSC.pack "cabal-version:  1.12\nname:           dep\nversion:        1.0\n"))
  assertEqual
    "a revised file carries its number"
    2
    (cabalFileRevision (BSC.pack "cabal-version:  1.12\nname:           dep\nversion:        1.0\nx-revision: 2\n"))
  assertEqual
    "the field is read whatever its case and spacing"
    7
    (cabalFileRevision (BSC.pack "name: dep\nX-Revision:   7\n"))
  assertEqual
    "a field that only starts the same way is not the revision"
    0
    (cabalFileRevision (BSC.pack "name: dep\nx-revision-history: 4\n"))

-- A plan that a lock file pins reads cached answers and must make no
-- request, so every answer has to be readable from the cache alone. Reading
-- the newest revision of a cabal file also records it under the number it
-- turned out to be, so replaying a lock that names that number is free.
--
-- Without a cached answer and without the network there is nothing to say,
-- and the failure must name what was wanted rather than report the package
-- as one Hackage does not have: a plan that silently drops a package
-- resolves to the wrong thing. That case is here rather than in a test of
-- its own because both set @XDG_CACHE_HOME@, and tasty runs test cases at
-- the same time.
test_answersFromCache :: Assertion
test_answersFromCache =
  withIsolatedCache $ \cacheDir -> do
    createDirectoryIfMissing True (cacheDir </> "versions")
    createDirectoryIfMissing True (cacheDir </> "cabal")
    BSC.writeFile (cacheDir </> "versions" </> "dep.json") (BSC.pack "{\"1.0\":\"normal\",\"2.0\":\"deprecated\"}")
    BSC.writeFile (cacheDir </> "cabal" </> "dep-1.0-latest.cabal") (BSC.pack "name: dep\nversion: 1.0\nx-revision: 3\n")
    queries <- newHackageQueries offline
    versions <- queryPackageVersions queries "dep"
    assertEqual
      "the cached versions of dep"
      (Just [("2.0", True), ("1.0", False)])
      (map (\version' -> (showVersion (queryVersionVersion version'), queryVersionDeprecated version')) <$> versions)
    one <- version "1.0"
    newest <- queryCabalFile queries "dep" one Nothing
    assertEqual "the newest cached revision of dep-1.0" (Right 3) (fst <$> newest)
    numbered <- queryCabalFile queries "dep" one (Just 3)
    assertEqual
      "the same answer under the number it turned out to be"
      newest
      numbered
    -- Whether a release has a fourth revision is a question only Hackage
    -- can answer, so offline there is nothing to say about it.
    missing <- try (queryCabalFile queries "dep" one (Just 4))
    case missing of
      Right answer -> assertFailure ("expected a failure, got " <> show (fst <$> answer))
      Left err -> assertBool (show err) ("at revision 4" `isInfixOf` show (err :: IOException))
    unknown <- try (queryPackageVersions queries "no-such-package")
    case unknown of
      Right answer -> assertFailure ("expected a failure, got " <> show (fmap (map queryVersionVersion) answer))
      Left err -> do
        let message = show (err :: IOException)
        assertBool message ("the versions of no-such-package" `isInfixOf` message)
        assertBool message ("network is disabled" `isInfixOf` message)

offline :: QueryOptions
offline = defaultQueryOptions {queryAllowNetwork = False, queryVerbose = False}

version :: String -> IO Version
version text = maybe (assertFailure ("not a version: " <> text)) pure (parseVersionString text)

-- | Run with @XDG_CACHE_HOME@ in a temporary directory, so the tests never
-- read or write the cache of the machine they run on.
withIsolatedCache :: (FilePath -> IO a) -> IO a
withIsolatedCache action =
  withTempDir "aihc-hackage-query" $ \root ->
    bracket (lookupEnv "XDG_CACHE_HOME") (maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME")) $ \_ -> do
      setEnv "XDG_CACHE_HOME" root
      getQueryCacheDir >>= action

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tempRoot <- getTemporaryDirectory
  (tempFile, tempHandle) <- openTempFile tempRoot (prefix ++ "-XXXXXX")
  hClose tempHandle
  removeFile tempFile
  createDirectory tempFile
  bracket (pure tempFile) removeDirectoryRecursive action
