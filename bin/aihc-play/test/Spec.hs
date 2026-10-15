{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cli.PlayBundleStore (storeBundle)
import Aihc.Play
import Aihc.Play.Json (renderJson)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (XdgDirectory (XdgCache), doesDirectoryExist, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

main :: IO ()
main = do
  -- AIHC_PRIM_SRC names the aihc-prim package, as it does for the other
  -- test suites. The sources are under its src directory.
  primRoot <- maybe "../../core-libs/aihc-prim/src" (</> "src") <$> lookupEnv "AIHC_PRIM_SRC"
  sources <- loadSupportSources primRoot "prelude"
  session <- either fail pure (buildSession sources)
  bundled <- bundledSession
  defaultMain $
    testGroup
      "aihc-play"
      ( sessionTests "the small Prelude" "GHC.Prim.Base" session
          : [sessionTests "the core libraries bundle" "GHC.Base" bundle | Just bundle <- [bundled]]
      )

-- | A session over the bundle of a store. AIHC_PLAY_STORE names the
-- store; without it, the default store of the user is used when it holds
-- the core libraries. The bundle tests are left out without a store.
bundledSession :: IO (Maybe Session)
bundledSession = do
  named <- lookupEnv "AIHC_PLAY_STORE"
  store <- case named of
    Just path -> pure (Just path)
    Nothing -> do
      cache <- getXdgDirectory XdgCache "aihc"
      let path = cache </> "store"
      present <- doesDirectoryExist path
      pure (if present then Just path else Nothing)
  case store of
    Nothing -> do
      hPutStrLn stderr "aihc-play: no store; the bundle tests are left out"
      pure Nothing
    Just path -> do
      (_, bytes) <- storeBundle path
      either fail (pure . Just) (bundleSession (BL.toStrict bytes))

-- | The commands over one session. The module that defines @map@ names
-- the library the session holds.
sessionTests :: String -> Text -> Session -> TestTree
sessionTests label mapModule session =
  testGroup
    label
    [ testCase "parse gives an abridged tree" $ do
        let output = run CommandParse "x = ()"
        assertBool "the tree names the pattern binding" ("PatternBind" `T.isInfixOf` output)
        assertBool "the tree hides source spans" (not ("SourceSpan" `T.isInfixOf` output)),
      testCase "names gives the binder and the uses of a local" $ do
        let output = run CommandNames "f x = x"
        assertBool "the local binder is reported" ("\"binder\":true" `T.isInfixOf` output)
        assertBool "the local use is reported" ("\"kind\":\"local\"" `T.isInfixOf` output)
        assertBool "the use resolves" (not ("unresolved" `T.isInfixOf` output)),
      testCase "names reaches the Prelude" $ do
        let output = run CommandNames "double xs = map (\\x -> x + x) xs"
        assertBool "map comes from the Prelude" (("\"module\":\"" <> mapModule <> "\", \"name\":\"map\"") `T.isInfixOf` output),
      testCase "types adds the inferred signature" $ do
        let output = run CommandTypes "x = ()"
        assertBool "the signature goes above the binding" ("x :: ()\\nx = ()" `T.isInfixOf` output),
      testCase "types infers a function with a number" $ do
        let output = run CommandTypes "twice f x = f (f x)\nthree = twice (+ 1) 1"
        assertBool "the function is polymorphic" ("twice :: (a -> a) -> a -> a" `T.isInfixOf` output)
        assertBool "the number defaults" ("three :: " `T.isInfixOf` output),
      testCase "types reports a type error" $ do
        let output = run CommandTypes "x = True + ()"
        assertBool "the error names the stage" ("\"stage\":\"typecheck\"" `T.isInfixOf` output)
    ]
  where
    run command source = renderJson (runCommand session command source)
