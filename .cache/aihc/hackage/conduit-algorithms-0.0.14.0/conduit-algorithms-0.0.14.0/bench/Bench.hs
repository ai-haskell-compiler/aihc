{- Copyright 2018
 - Licence: MIT -}
import Criterion.Main (defaultMain, nfIO, bench, bgroup, Benchmarkable)
import Control.Monad.Trans.Resource

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Control.DeepSeq (NFData)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit as C
import           Data.Conduit ((.|))


import qualified Data.Conduit.Algorithms as CAlg
import qualified Data.Conduit.Algorithms.Storable as CAlg
import qualified Data.Conduit.Algorithms.Utils as CAlg
import qualified Data.Conduit.Algorithms.Async as CAlg
import qualified Data.Conduit.Algorithms.Async.ByteString as CAlg

progress :: Monad m => Int -> Int -> C.ConduitT () Int m ()
progress _ 0 = return ()
progress n i = C.yield n >> progress (n + 3) (i - 1)

nfConduit :: NFData a => C.ConduitT () C.Void (ResourceT IO) a -> Benchmarkable
nfConduit = nfIO . C.runConduitRes

countC = countC' (0 :: Int)
    where
        countC' !n = C.await >>= \case
            Nothing -> return n
            Just _ -> countC' (n+1)
sumC = sumC' (0 :: Int)
    where
        sumC' !n = C.await >>= \case
            Nothing -> return n
            Just v -> sumC' (n+v)

main = defaultMain
    [ bgroup "merge"
        [ bench "mergeC2" $ nfConduit (CAlg.mergeC2 (progress 1 1000) (progress 30 2000) .| CL.fold (+) (0 :: Int))
        , bench "mergeC_3" $ nfConduit (CAlg.mergeC [progress 1 1000, progress 30 2000, progress 7 9] .| CL.fold (+) (0 :: Int))
        , bench "mergeC_1000" $ nfConduit (CAlg.mergeC [progress 1 n | n <- [1 .. 2000]] .| CL.fold (+) (0 :: Int))
        ]
    , bgroup "async-compress"
        [ bench "asyncGzipToFile" $ nfConduit (CB.sourceFile "test_data/input.txt" .| CAlg.asyncGzipToFile "test_data/output.txt.gz")
        , bench "asyncBzip2ToFile" $ nfConduit (CB.sourceFile "test_data/input.txt" .| CAlg.asyncBzip2ToFile "test_data/output.txt.gz")
        , bench "asyncXzToFile" $ nfConduit (CB.sourceFile "test_data/input.txt" .| CAlg.asyncXzToFile "test_data/output.txt.gz")
        ]
    , bgroup "async-uncompress"
        [ bench "baseline" $ nfConduit (CB.sourceFile "test_data/input.txt" .| CB.sinkFile "test_data/output.txt")
        , bench "asyncGzipFromFile" $ nfConduit (CAlg.conduitPossiblyCompressedFile "test_data/input.txt.gz" .| CB.sinkFile "test_data/output.txt")
        , bench "asyncBzip2FromFile" $ nfConduit (CAlg.conduitPossiblyCompressedFile "test_data/input.txt.bz2" .| CB.sinkFile "test_data/output.txt")
        ]
    , bgroup "async-map"
        [ bench "filterlines" $ nfConduit (CB.sourceFile "test_data/input.txt" .| CB.lines .| CL.filter (\line -> (read . B8.unpack $ line) > (1000 :: Int)) .| countC)
        , bench "asyncFilterLinesC" $ nfConduit (CB.sourceFile "test_data/input.txt" .| CAlg.asyncFilterLinesC 8 (\line -> (read . B8.unpack $ line) > (1000 :: Int)) .| countC)
        , bench "asyncMapLineGroupsC" $ nfConduit (CB.sourceFile "test_data/input.txt" .| CAlg.asyncMapLineGroupsC 8 (length . filter (\line -> (read . B8.unpack $ line) > (1000 :: Int))) .| sumC)
        ]
    ]

