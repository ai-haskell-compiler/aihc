{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cli.PlayBundleStore (storeBundle)
import Aihc.Play
import Aihc.Play.Json (renderJson)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import System.Directory (XdgDirectory (XdgCache), doesDirectoryExist, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, testCase)

-- | The suite runs over the bundle of a store. AIHC_PLAY_STORE names the
-- store; without it, the default store of the user must hold the core
-- libraries. The bundle is written when the first test runs, so a run
-- that selects no test needs no store.
main :: IO ()
main =
  defaultMain $
    withResource openSession (\_ -> pure ()) $ \getSession ->
      testGroup
        "aihc-play"
        [ testCase "parse gives an abridged tree" $ do
            output <- run getSession CommandParse "x = ()"
            assertBool "the tree names the pattern binding" ("PatternBind" `T.isInfixOf` output)
            assertBool "the tree hides source spans" (not ("SourceSpan" `T.isInfixOf` output)),
          testCase "names gives the binder and the uses of a local" $ do
            output <- run getSession CommandNames "f x = x"
            assertBool "the local binder is reported" ("\"binder\":true" `T.isInfixOf` output)
            assertBool "the local use is reported" ("\"kind\":\"local\"" `T.isInfixOf` output)
            assertBool "the use resolves" (not ("unresolved" `T.isInfixOf` output)),
          testCase "names reaches the Prelude of aihc-base" $ do
            output <- run getSession CommandNames "double xs = map (\\x -> x + x) xs"
            assertBool "map comes from GHC.Base" ("\"module\":\"GHC.Base\", \"name\":\"map\"" `T.isInfixOf` output),
          testCase "types adds the inferred signature" $ do
            output <- run getSession CommandTypes "x = ()"
            assertBool "the signature goes above the binding" ("x :: ()\\nx = ()" `T.isInfixOf` output),
          testCase "types infers a function with a number" $ do
            output <- run getSession CommandTypes "twice f x = f (f x)\nthree = twice (+ 1) 1"
            assertBool "the function is polymorphic" ("twice :: (a -> a) -> a -> a" `T.isInfixOf` output)
            assertBool "the number defaults" ("three :: " `T.isInfixOf` output),
          testCase "types reports a type error" $ do
            output <- run getSession CommandTypes "x = True + ()"
            assertBool "the error names the stage" ("\"stage\":\"typecheck\"" `T.isInfixOf` output)
        ]
  where
    run getSession command source = do
      session <- getSession
      pure (renderJson (runCommand session command source))

openSession :: IO Session
openSession = do
  named <- lookupEnv "AIHC_PLAY_STORE"
  store <- case named of
    Just path -> pure path
    Nothing -> do
      cache <- getXdgDirectory XdgCache "aihc"
      let path = cache </> "store"
      present <- doesDirectoryExist path
      if present then pure path else fail "no store: set AIHC_PLAY_STORE to a store with aihc-base"
  (_, bytes) <- storeBundle store
  either fail pure (bundleSession (BL.toStrict bytes))
