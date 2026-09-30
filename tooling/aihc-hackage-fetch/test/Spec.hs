module Main (main) where

import Aihc.Hackage.Index (IndexEntry (..), IndexScan (..), latestPreferredVersions, parseHackageIndex, parseHackageIndexUpdatedSince, parsePreferredRanges, readIndexEntry, scanIndex)
import Aihc.Hackage.IndexCache (IndexTable (..), IndexVersion (..), indexTableFromScan, indexTableVersions, parseIndexTable, renderIndexTable)
import Aihc.Hackage.Package (showVersion, showVersionRange)
import Aihc.Hackage.Types (PackageSpec (..))
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Hedgehog (Property, property, success)
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), hClose, openTempFile, withBinaryFile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain . testGroup "aihc-hackage-fetch" $
    [ testCase "parses latest package versions from Hackage index tarball" $ do
        parseHackageIndex testHackageIndex
          @?= Right
            [ PackageSpec "alpha" "1.2.0",
              PackageSpec "beta" "0.1"
            ],
      testCase "filters Hackage index packages by latest upload time" $ do
        parseHackageIndexUpdatedSince 100 testHackageIndex
          @?= Right
            [ PackageSpec "alpha" "1.2.0"
            ],
      testCase "reads preferred version ranges from the Hackage index" test_readsPreferredRanges,
      testCase "skips deprecated versions when resolving from the Hackage index" test_skipsDeprecatedVersions,
      testCase "scans every version and revision of the Hackage index" test_scansIndexEntries,
      testCase "round-trips the derived index table" test_indexTableRoundTrip,
      testProperty "Hedgehog options" prop_dummy
    ]

-- | Keep the repository Hedgehog options accepted by this test suite.
prop_dummy :: Property
prop_dummy = property success

(@?=) :: (Eq a, Show a) => Either String a -> Either String a -> Assertion
actual @?= expected =
  if actual == expected
    then pure ()
    else assertFailure ("expected: " <> show expected <> "\n but got: " <> show actual)

testHackageIndex :: LBS.ByteString
testHackageIndex =
  GZip.compress $
    Tar.write
      [ cabalEntryAt "alpha/1.0.0/alpha.cabal" 20,
        cabalEntryAt "alpha/1.2.0/alpha.cabal" 100,
        cabalEntryAt "alpha/1.1.0/alpha.cabal" 200,
        cabalEntryAt "beta/0.1/beta.cabal" 99,
        cabalEntry "beta/0.2/not-beta.cabal",
        cabalEntry "preferred-versions"
      ]
  where
    cabalEntry path =
      cabalEntryAt path 0

    cabalEntryAt path uploadedAt =
      case Tar.toTarPath False path of
        Left err -> error ("invalid test tar path: " <> show err)
        Right tarPath ->
          let contents = LBS.fromStrict (BSC.pack "name: ignored\n")
           in (Tar.simpleEntry tarPath (Tar.NormalFile contents (LBS.length contents))) {Tar.entryTime = uploadedAt}

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tempRoot <- getTemporaryDirectory
  (tempFile, tempHandle) <- openTempFile tempRoot (prefix ++ "-XXXXXX")
  hClose tempHandle
  removeFile tempFile
  createDirectory tempFile
  bracket
    (pure tempFile)
    removeDirectoryRecursive
    action

-- | An index whose packages exercise each way a preferred-versions entry can
-- constrain a package.
testPreferredIndex :: LBS.ByteString
testPreferredIndex =
  GZip.compress $
    Tar.write
      [ entry "alpha/1.0.0/alpha.cabal" "name: ignored\n",
        entry "alpha/1.1.0/alpha.cabal" "name: ignored\n",
        entry "alpha/1.2.0/alpha.cabal" "name: ignored\n",
        entry "alpha/preferred-versions" "alpha <1.2.0 || >1.2.0\n",
        entry "beta/0.1/beta.cabal" "name: ignored\n",
        -- The index is append-only, so the later entry is the one in force.
        entry "gamma/2.0/gamma.cabal" "name: ignored\n",
        entry "gamma/preferred-versions" "gamma <2.0\n",
        entry "gamma/preferred-versions" "gamma >=2.0\n",
        -- An emptied entry lifts an earlier restriction.
        entry "delta/1.0/delta.cabal" "name: ignored\n",
        entry "delta/preferred-versions" "delta <1.0\n",
        entry "delta/preferred-versions" ""
      ]
  where
    entry path contents =
      case Tar.toTarPath False path of
        Left err -> error ("invalid test tar path: " <> show err)
        Right tarPath ->
          let body = LBS.fromStrict (BSC.pack contents)
           in Tar.simpleEntry tarPath (Tar.NormalFile body (LBS.length body))

test_readsPreferredRanges :: Assertion
test_readsPreferredRanges =
  case parsePreferredRanges testPreferredIndex of
    Left err -> assertFailure ("failed to parse test index: " <> err)
    Right ranges -> do
      assertEqual
        "packages with a restriction"
        ["alpha", "gamma"]
        (Map.keys ranges)
      assertEqual
        "alpha range"
        (Just "<1.2.0 || >1.2.0")
        (showVersionRange <$> Map.lookup "alpha" ranges)
      assertEqual
        "the later gamma entry wins"
        (Just ">=2.0")
        (showVersionRange <$> Map.lookup "gamma" ranges)

test_skipsDeprecatedVersions :: Assertion
test_skipsDeprecatedVersions =
  case parsePreferredRanges testPreferredIndex >>= \ranges -> latestPreferredVersions ranges testPreferredIndex of
    Left err -> assertFailure ("failed to resolve test index: " <> err)
    Right versions ->
      assertEqual
        "newest preferred version of each package"
        [("alpha", "1.1.0"), ("beta", "0.1"), ("delta", "1.0"), ("gamma", "2.0")]
        (Map.toAscList (Map.map showVersion versions))

-- The scan records each cabal entry with its revision and where it sits in
-- the tarball, and the entry can be read back from that offset.
test_scansIndexEntries :: Assertion
test_scansIndexEntries = do
  let uncompressed = GZip.decompress testRevisionIndex
  scan <- either (assertFailure . ("failed to scan the test index: " <>)) pure (scanIndex uncompressed)
  assertEqual
    "alpha entries in index order"
    [("1.0.0", 0), ("1.1.0", 0), ("1.0.0", 1), ("1.0.0", 2)]
    [(showVersion (indexEntryVersion entry), indexEntryRevision entry) | entry <- scanEntries scan Map.! "alpha"]
  assertEqual "beta has one entry" [("0.1", 0)] [(showVersion (indexEntryVersion entry), indexEntryRevision entry) | entry <- scanEntries scan Map.! "beta"]
  assertEqual "the preferred range is kept" (Just "<1.1.0 || >1.1.0") (showVersionRange <$> Map.lookup "alpha" (scanPreferredRanges scan))
  assertEqual "the index state is the newest entry time" 300 (scanIndexState scan)
  withTempDir "aihc-hackage-index" $ \root -> do
    let tarball = root </> "01-index.tar"
    LBS.writeFile tarball uncompressed
    let alphaEntries = scanEntries scan Map.! "alpha"
    contents <- withBinaryFile tarball ReadMode $ \handle -> mapM (readIndexEntry handle) alphaEntries
    assertEqual
      "each revision reads back its own cabal file"
      ["alpha 1.0.0 r0", "alpha 1.1.0 r0", "alpha 1.0.0 r1", "alpha 1.0.0 r2"]
      (map (BSC.unpack . BSC.strip) contents)
  let table = indexTableFromScan scan
  versions <- maybe (assertFailure "alpha is missing from the table") pure (indexTableVersions table "alpha")
  assertEqual "versions newest first" ["1.1.0", "1.0.0"] (map (showVersion . indexVersionVersion) versions)
  assertEqual "the newest version is deprecated" [True, False] (map indexVersionDeprecated versions)
  assertEqual "revisions oldest first" [[0], [0, 1, 2]] (map (map indexEntryRevision . indexVersionRevisions) versions)
  assertEqual "an unknown package has no versions" Nothing (indexTableVersions table "omega")

test_indexTableRoundTrip :: Assertion
test_indexTableRoundTrip = do
  let table =
        IndexTable
          { indexTableEntries = Map.fromList [(BSC.pack "alpha", [(BSC.pack "1.0.0", 0, 0), (BSC.pack "1.1.0", 0, 4), (BSC.pack "1.0.0", 1, 8)]), (BSC.pack "beta", [(BSC.pack "0.1", 0, 12)])],
            indexTableRanges = Map.fromList [(BSC.pack "alpha", BSC.pack "<1.1.0 || >1.1.0")],
            indexTableState = 300
          }
  assertEqual "derived table round trip" (Right table) (parseIndexTable (renderIndexTable table))

-- An index with revisions: alpha 1.0.0 is uploaded, then 1.1.0, then 1.0.0
-- is revised twice, and 1.1.0 is deprecated.
testRevisionIndex :: LBS.ByteString
testRevisionIndex =
  GZip.compress $
    Tar.write
      [ entry "alpha/1.0.0/alpha.cabal" "alpha 1.0.0 r0\n" 100,
        entry "alpha/1.1.0/alpha.cabal" "alpha 1.1.0 r0\n" 200,
        entry "alpha/1.0.0/alpha.cabal" "alpha 1.0.0 r1\n" 250,
        entry "alpha/preferred-versions" "alpha <1.1.0 || >1.1.0\n" 260,
        entry "alpha/1.0.0/alpha.cabal" "alpha 1.0.0 r2\n" 300,
        entry "beta/0.1/beta.cabal" "beta 0.1 r0\n" 150,
        entry "alpha/1.0.0/package.json" "{}\n" 100
      ]
  where
    entry path contents time =
      case Tar.toTarPath False path of
        Left err -> error ("invalid test tar path: " <> show err)
        Right tarPath ->
          let body = LBS.fromStrict (BSC.pack contents)
           in (Tar.simpleEntry tarPath (Tar.NormalFile body (LBS.length body))) {Tar.entryTime = time}
