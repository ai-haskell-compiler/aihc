{-# LANGUAGE OverloadedStrings #-}

-- | The purpose of this test is to make sure that if we run lots of parses on
-- multiple threads, that they all give the correct answers.  This is important,
-- because this implementation is imperative code hidden inside an unsafePerformIO.
module Text.XML.Expat.ParallelTest where

import Text.XML.Expat.Tests  -- Arbitrary instance
import Text.XML.Expat.ParseFormat (normalizeText)
import Text.XML.Expat.Tree
import Text.XML.Expat.Format

import Control.Concurrent
import Control.Exception
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.HUnit hiding (Node)
import System.IO
import System.Random
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Prelude hiding (catch)


tests = hUnitTestToTests $
    TestList [
        TestLabel "parallel (forkIO)" $ TestCase (testParallel forkOS),
        TestLabel "parallel (forkOS)" $ TestCase (testParallel forkOS)
    ]

chunkSize = 512

breakUp :: B.ByteString -> L.ByteString
breakUp = L.fromChunks . bu
  where
    bu bs | B.length bs < chunkSize = [bs]
    bu bs = bs1:bu bs2
      where
        (bs1, bs2) = B.splitAt chunkSize bs

nthreads = 5
nloops = 500

testParallel :: (IO () -> IO ThreadId) -> IO ()
testParallel fork = do
    resultMVs <- replicateM nthreads $ do
        resultMV <- newEmptyMVar
        do
            replicateM_ nloops $ do
                g <- newQCGen
                let treeIn = normalizeText $ unGen (arbitrary :: Gen TNode) g 30
                    xml = breakUp $ format' treeIn
                    treeOut = normalizeText $ parseThrowing defaultParseOptions xml
                assertEqual "tree match" treeIn treeOut
                  `catch` \exc -> do
                      putStrLn $ "failing XML: "++concat (map B.unpack $ L.toChunks xml)
                      throwIO (exc :: SomeException)
            putMVar resultMV Nothing
          `catch` \exc -> do
            putMVar resultMV $ Just (exc :: SomeException)
        return resultMV

    forM_ resultMVs $ \resultMV -> do
        mExc <- takeMVar resultMV
        case mExc of
            Just exc -> throwIO exc
            Nothing  -> return ()

