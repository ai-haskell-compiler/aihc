{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Test.Tasty.KAT
-- License     : MIT
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- Tasty support for KAT (Known Answer Tests)
--
module Test.Tasty.KAT
    (
    -- * Run tests
      testKatDetailed
    , testKatGrouped
    -- * Load KAT resources
    , testKatLoad
    , Resource(..)
    , katLoaderSimple
    , mapTestUnits
    ) where

import Control.Applicative
import Control.Exception
import Data.Typeable
import Test.Tasty (testGroup)
import Test.Tasty.Providers
import Test.Tasty.KAT.FileLoader

newtype Resource a = Resource [(String, [a])]

data TestKatSingle = TestKatSingle (IO Bool)
    deriving Typeable

data TestKatGroup = TestKatGroup [(Int, IO Bool)]
    deriving Typeable

data KatResult = KatFailed String | KatSuccess
    deriving (Show,Eq)

tryResult :: IO Bool -> IO KatResult
tryResult f = do
    er <- try f
    case er of
        Left (e :: SomeException)
            | show e == "<<timeout>>" -> throwIO e
            | otherwise               -> return $ KatFailed (show e)
        Right r                       -> return $ if r then KatSuccess else KatFailed "test failed"

instance IsTest TestKatSingle where
    run _ (TestKatSingle tst) _ = do
        r <- tryResult tst
        case r of
            KatSuccess  -> return $ testPassed ""
            KatFailed s -> return $ testFailed s
    testOptions = return []
instance IsTest TestKatGroup where
    run _ (TestKatGroup groupTests) _ = do
        (success, failed) <- summarize <$> mapM runGroup groupTests
        return $
            (if failed == 0 then testPassed else testFailed)
            (if failed > 0 then (show failed) ++ " tests failed on " ++ show (failed + success)
                           else (show success) ++ " tests succeed")
      where summarize :: [KatResult] -> (Int, Int)
            summarize = foldl (\(s,f) k -> if k == KatSuccess then (s+1,f) else (s,f+1)) (0,0)

            runGroup :: (Int, IO Bool) -> IO KatResult
            runGroup (_, tst) = tryResult tst
            --nbGroups = fromIntegral $ length groupTests
            --yieldProgress $ Progress { progressText = groupName, progressPercent = fromIntegral tstNb / nbGroups  }

    testOptions = return []

-- | run one tasty test per vectors in each groups
--
-- This is useful to have detailed output on what failed
-- and what succeeded. For a more concise output, use
-- 'testKatGrouped'
testKatDetailed :: TestName
                -> Resource a
                -> (String -> a -> IO Bool)
                -> TestTree
testKatDetailed name (Resource groups) test = -- singleTest name $ mkTestKat resource test
    testGroup name $ map groupToTests groups
  where groupToTests (groupName, vectors) =
            testGroup groupName $ map (\(i, v) -> singleTest (show (i :: Int)) (TestKatSingle $ test groupName v)) (zip [1..] vectors)

-- | run one tasty test per group
testKatGrouped :: TestName
               -> Resource a
               -> (String -> a -> IO Bool)
               -> TestTree
testKatGrouped name (Resource groups) test = -- singleTest name $ mkTestKat resource test
    testGroup name $ map groupToTests groups
  where groupToTests (groupName, vectors) =
            singleTest groupName $ TestKatGroup $ map (\(i, v) -> (i, test groupName v)) (zip [1..] vectors)

-- | Read a KAT file into values that will be used for KATs tests
testKatLoad :: FilePath
            -> ([String] -> [(String, [a])])
            -> IO (Resource a)
testKatLoad filepath transform = Resource . transform . lines <$> readFile filepath
