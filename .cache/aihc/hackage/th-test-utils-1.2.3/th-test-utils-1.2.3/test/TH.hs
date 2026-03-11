{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module TH where

import Control.Exception (SomeException, displayException, try)
import Data.Bifunctor (first)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Module (..))
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Language.Haskell.TH.TestUtils (runTestQ, unmockedState)

-- | Run an HUnit assertion in a Template Haskell splice.
testCaseTH :: String -> Q Assertion -> ExpQ
testCaseTH testName q = do
  q >>= runIO
  [|testCase testName $ return ()|]

-- | Check that the given action evalutes.
isSuccess :: Q a -> Q Assertion
isSuccess action = do
  _ <- forceM action
  return $ return ()

-- | Check that the given action results in the same thing in both Q and unmocked TestQ.
checkUnmockedMatches :: (Eq a, Show a) => Q a -> Q Assertion
checkUnmockedMatches action = do
  expected <- action
  actual <- runTestQ unmockedState action
  return $ actual @?= expected

-- | Capture any 'error' calls.
tryIO :: IO a -> IO (Either String a)
tryIO m = first showError <$> try m
  where
    -- strip out call stack
    showError e =
      case lines $ displayException @SomeException e of
        s : _ -> s
        [] -> error $ "Unexpectedly got empty string: " ++ show e

-- | Same as tryIO, except in the Q monad.
tryQ :: Q a -> Q (Either String a)
tryQ q = runIO . tryIO . forceM . pure =<< q

-- | Force the evaluation of @a@ when the monadic action is evaluated.
forceM :: (Monad m) => m a -> m a
forceM m = do
  a <- m
  a `seq` return a

thisModule :: ExpQ
thisModule = do
  Module pkgName modName <- Language.Haskell.TH.thisModule
  -- if only Module had a Lift instance
  [|Module pkgName modName|]
