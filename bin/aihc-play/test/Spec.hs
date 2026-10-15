{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Play
import Aihc.Play.Json (renderJson)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

main :: IO ()
main = do
  -- AIHC_PRIM_SRC names the aihc-prim package, as it does for the other
  -- test suites. The sources are under its src directory.
  primRoot <- maybe "../../core-libs/aihc-prim/src" (</> "src") <$> lookupEnv "AIHC_PRIM_SRC"
  sources <- loadSupportSources primRoot "prelude"
  session <- either fail pure (buildSession sources)
  defaultMain $
    testGroup
      "aihc-play"
      [ testCase "parse gives an abridged tree" $ do
          let output = run session CommandParse "x = ()"
          assertBool "the tree names the pattern binding" ("PatternBind" `T.isInfixOf` output)
          assertBool "the tree hides source spans" (not ("SourceSpan" `T.isInfixOf` output)),
        testCase "names gives the binder and the uses of a local" $ do
          let output = run session CommandNames "f x = x"
          assertBool "the local binder is reported" ("\"binder\":true" `T.isInfixOf` output)
          assertBool "the local use is reported" ("\"kind\":\"local\"" `T.isInfixOf` output)
          assertBool "the use resolves" (not ("unresolved" `T.isInfixOf` output)),
        testCase "names reaches the Prelude" $ do
          let output = run session CommandNames "double xs = map (\\x -> x + x) xs"
          assertBool "map comes from the Prelude" ("\"module\":\"GHC.Prim.Base\", \"name\":\"map\"" `T.isInfixOf` output),
        testCase "types adds the inferred signature" $ do
          let output = run session CommandTypes "x = ()"
          assertBool "the signature goes above the binding" ("x :: ()\\nx = ()" `T.isInfixOf` output),
        testCase "types infers a function with a number" $ do
          let output = run session CommandTypes "twice f x = f (f x)\nthree = twice (+ 1) 1"
          assertBool "the function is polymorphic" ("twice :: (a -> a) -> a -> a" `T.isInfixOf` output)
          assertBool "the number defaults" ("three :: " `T.isInfixOf` output),
        testCase "types reports a type error" $ do
          let output = run session CommandTypes "x = True + ()"
          assertBool "the error names the stage" ("\"stage\":\"typecheck\"" `T.isInfixOf` output)
      ]
  where
    run session command source = renderJson (runCommand session command source)
