{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Prelude

import Control.Concurrent.Async
import Control.Monad (replicateM_)
import Criterion.Main
import Data.Char (intToDigit)
import Data.IORef
import Data.Time.Clock.POSIX
import Network.AWS.XRayClient
import System.Random

main :: IO ()
main = do
  stdGenIORef <- newIORef =<< newStdGen

  let
    segmentIdNotShared = splitActionAcrossThreads
      (pure ())
      (const $ getStdRandom generateXRaySegmentId)
    segmentIdShared = splitActionAcrossThreads
      (newStdGen >>= newIORef)
      (`withRandomGenIORef` generateXRaySegmentId)
    parNewStdGen = splitActionAcrossThreads (pure ()) (const newStdGen)
  defaultMain
    [ bgroup
      "single-threaded timings"
      [ -- These ID generation functions use the machine's random number generator,
        -- which can be slow if used improperly.
        bench "generateTraceId" $ nfIO (generateXRayTraceId stdGenIORef)
      , bench "generateSegmentId"
        $ nfIO (withRandomGenIORef stdGenIORef generateXRaySegmentId)
      , bench "getPOSIXTime" $ nfIO getPOSIXTime
      , bench "intToDigit" $ nf intToDigit 0
      , bench "newStdGen" $ whnfIO newStdGen
      , bench "makeSegment" $ whnfIO (makeSegment stdGenIORef)
      ]
    , bgroup
      "generateXRaySegmentId not shared parallel"
      [ benchGenerateXRaySegmentId 100 segmentIdNotShared
      , benchGenerateXRaySegmentId 4 segmentIdNotShared
      , benchGenerateXRaySegmentId 2 segmentIdNotShared
      , benchGenerateXRaySegmentId 1 segmentIdNotShared
      ]
    , bgroup
      "generateXRaySegmentId shared parallel"
      [ benchGenerateXRaySegmentId 100 segmentIdShared
      , benchGenerateXRaySegmentId 4 segmentIdShared
      , benchGenerateXRaySegmentId 2 segmentIdShared
      , benchGenerateXRaySegmentId 1 segmentIdShared
      ]
    , bgroup
      "newStdGen parallel"
      [ bench "newStdGen parallel 100" $ whnfIO $ parNewStdGen 100 10000
      , bench "newStdGen parallel 4" $ whnfIO $ parNewStdGen 4 10000
      , bench "newStdGen parallel 2" $ whnfIO $ parNewStdGen 2 10000
      , bench "newStdGen parallel 1" $ whnfIO $ parNewStdGen 1 10000
      ]
    ]

benchGenerateXRaySegmentId :: Int -> (Int -> Int -> IO a) -> Benchmark
benchGenerateXRaySegmentId n segmentIdSharing =
  bench ("generateXRaySegmentId " ++ show n) $ whnfIO $ segmentIdSharing n 10000

makeSegment :: IORef StdGen -> IO XRaySegment
makeSegment stdGenIORef = do
  startTime <- getPOSIXTime
  endTime <- getPOSIXTime
  segmentId <- withRandomGenIORef stdGenIORef generateXRaySegmentId
  pure $ xraySubsegment "segment" segmentId startTime (Just endTime)

splitActionAcrossThreads :: IO a -> (a -> IO b) -> Int -> Int -> IO ()
splitActionAcrossThreads setup action numThreads totalActions =
  replicateConcurrently_ numThreads $ do
    setupArg <- setup
    replicateM_ (totalActions `div` numThreads) (action setupArg)
