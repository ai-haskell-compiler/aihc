{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Main where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 706)
import Prelude hiding (catch)
#endif

import Control.Monad.Exception
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.Error
#endif /* !MIN_VERSION_transformers(0,6,0) */
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.IORef
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, (@?=))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ exceptTests
#if !MIN_VERSION_transformers(0,6,0)
        , errorTests
#endif /* !MIN_VERSION_transformers(0,6,0) */
        ]

#if !MIN_VERSION_transformers(0,6,0)
errorTests :: Test
errorTests = testGroup "ErrorT tests"
    [testCase (conl ++ " " ++ whatl) (mkErrorTest con what) | (conl, con) <- cons, (whatl, what) <- whats]
  where
    whats :: [(String, ErrorT String IO ())]
    whats = [("return",     return ()),
             ("error",      error "error"),
             ("throwError", throwError "throwError")]

    cons :: [(String, ErrorT String IO () -> ErrorT String IO () -> ErrorT String IO ())]
    cons = [("finally",  \what sequel -> what `finally` sequel),
            ("bracket_", \what sequel -> bracket_ (return ()) sequel what)]

    mkErrorTest :: (ErrorT String IO () -> ErrorT String IO () -> ErrorT String IO ())
                -> ErrorT String IO ()
                -> Assertion
    mkErrorTest con what = do
        ref <- newIORef "sequel not called"
        let sequel = liftIO $ writeIORef ref expected
        _ <- runErrorT (con what sequel) `catch` \(e :: SomeException) -> return (Left (show e))
        actual <- readIORef ref
        expected @?= actual
      where
        expected :: String
        expected = "sequel called"
#endif /* !MIN_VERSION_transformers(0,6,0) */

exceptTests :: Test
exceptTests = testGroup "ExceptT tests"
    [testCase (conl ++ " " ++ whatl) (mkExceptTest con what) | (conl, con) <- cons, (whatl, what) <- whats]
  where
    whats :: [(String, ExceptT String IO ())]
    whats = [("return", return ()),
             ("error",  error "error"),
             ("throwE", throwE "throwE")]

    cons :: [(String, ExceptT String IO () -> ExceptT String IO () -> ExceptT String IO ())]
    cons = [("finally",  \what sequel -> what `finally` sequel),
            ("bracket_", \what sequel -> bracket_ (return ()) sequel what)]

    mkExceptTest :: (ExceptT String IO () -> ExceptT String IO () -> ExceptT String IO ())
                -> ExceptT String IO ()
                -> Assertion
    mkExceptTest con what = do
        ref <- newIORef "sequel not called"
        let sequel = liftIO $ writeIORef ref expected
        _ <- runExceptT (con what sequel) `catch` \(e :: SomeException) -> return (Left (show e))
        actual <- readIORef ref
        expected @?= actual
      where
        expected :: String
        expected = "sequel called"
