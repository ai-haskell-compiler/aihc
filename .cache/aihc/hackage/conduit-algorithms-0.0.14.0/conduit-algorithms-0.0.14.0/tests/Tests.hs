{- Copyright 2017-2024 Luis Pedro Coelho
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, CPP, QuasiQuotes, FlexibleContexts, OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Functor.Identity as FID
import qualified Data.Vector.Storable as VS
import           Data.Conduit ((.|))
import           Data.List (sort)
import           System.IO (withFile, IOMode(..))
import           System.Directory (removeFile)
import           Control.Exception (catch, ErrorCall)
import           Control.Monad (forM_, void)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Control.Monad.Trans.Resource as R
import           Control.Monad.IO.Unlift (MonadUnliftIO)

import qualified Data.Conduit.Algorithms as CAlg
import qualified Data.Conduit.Algorithms.Storable as CAlg
import qualified Data.Conduit.Algorithms.Utils as CAlg
import qualified Data.Conduit.Algorithms.Async as CAlg
import qualified Data.Conduit.Algorithms.Async.ByteString as CAlg

import TestLZMA (tgroup_lzma)

main :: IO ()
main = defaultMain $ testGroup "All tests" [tgroup_lzma, maintests]

maintests = $(testGroupGenerator)

testingFileNameGZ :: FilePath
testingFileNameGZ = "file_just_for_testing_delete_me_please.gz"
testingFileNameGZ2 :: FilePath
testingFileNameGZ2 = "file_just_for_testing_delete_me_please_2.gz"
testingFileNameBZ2 :: FilePath
testingFileNameBZ2 = "file_just_for_testing_delete_me_please.bz2"
testingFileNameBZ22 :: FilePath
testingFileNameBZ22 = "file_just_for_testing_delete_me_please_2.bz2"
testingFileNameXZ :: FilePath
testingFileNameXZ = "file_just_for_testing_delete_me_please.xz"
testingFileNameXZ2 :: FilePath
testingFileNameXZ2 = "file_just_for_testing_delete_me_please_2.xz"
testingFileNameZstd :: FilePath
testingFileNameZstd = "file_just_for_testing_delete_me_please.zstd"

extract :: C.ConduitM () b FID.Identity () -> [b]
extract c = C.runConduitPure (c .| CC.sinkList)

extractIO :: MonadUnliftIO m => C.ConduitM () b (ResourceT m) () -> m [b]
extractIO c = C.runConduitRes (c .| CC.sinkList)

shouldProduce :: (Show b, Eq b) => [b] -> C.ConduitM () b FID.Identity () -> Assertion
shouldProduce values cond = extract cond @?= values

shouldProduceIO :: (Show b, Eq b) => [b] -> C.ConduitM () b (ResourceT IO) () -> IO ()
shouldProduceIO values cond = do
    p <- extractIO cond
    p @?= values

assertError :: IO f -> IO ()
assertError f = do
    errored <- catch (f >> pure False) handler
    if errored then
        pure ()
    else
        assertFailure "Expected error but none was triggered"
    where
        handler :: ErrorCall -> IO Bool
        handler _ = pure True

case_uniqueC :: Assertion
case_uniqueC = extract (CC.yieldMany [1,2,3,1,1,2,3] .| CAlg.uniqueC) @=? [1,2,3 :: Int]
case_mergeC :: Assertion
case_mergeC = shouldProduce expected $
                            CAlg.mergeC
                                [ CC.yieldMany i1
                                , CC.yieldMany i2
                                , CC.yieldMany i3
                                , CC.yieldMany i3
                                ]
    where
        expected = sort (concat [i1, i2, i3, i3])
        i1 = [ 1, 2, 4 :: Int]
        i2 = [ 1, 4, 4, 5]
        i3 = [-1, 0, 7]

case_mergeCrepeated :: Assertion
case_mergeCrepeated = shouldProduce expected $
                            CAlg.mergeC
                                [ CC.yieldMany i1
                                , CC.yieldMany i2
                                , CC.yieldMany i3
                                , CC.yieldMany i3
                                ]
    where
        expected = sort (concat [i1, i2, i3, i3])
        i1 = [1, 2, 3, 4 :: Int]
        i2 = i1
        i3 = i1

case_mergeCmonad :: Assertion
case_mergeCmonad = shouldProduce expected $
                            CAlg.mergeC
                                [ mYield i1
                                , mYield i2
                                , mYield i3
                                ]
    where
        expected = sort (concat [i1, i2, i3])
        mYield lst = do
            let lst' = map return lst
            forM_ lst' $ \elemnt -> do
                elemnt' <- elemnt
                C.yield elemnt'
        i1 = [ 0, 2, 4 :: Int]
        i2 = [ 1, 3, 4, 5]
        i3 = [-1, 0, 7]


case_mergeCempty :: Assertion
case_mergeCempty = shouldProduce ([] :: [Int]) $ CAlg.mergeC []

case_mergeC2 :: Assertion
case_mergeC2 = shouldProduce [0, 1, 1, 2, 3, 5 :: Int] $
                            CAlg.mergeC2
                                (CC.yieldMany [0, 1, 2])
                                (CC.yieldMany [1, 3, 5])

case_mergeC2same :: Assertion
case_mergeC2same = shouldProduce [0, 0, 1, 1, 2, 2 :: Int] $
                            CAlg.mergeC2
                                (CC.yieldMany [0, 1, 2])
                                (CC.yieldMany [0, 1, 2])

case_mergeC2monad :: Assertion
case_mergeC2monad = shouldProduce [0, 1, 2, 2, 3, 4 :: Int] $ do
                            CAlg.mergeC2
                                (CC.yieldMany [0, 2])
                                (CC.yieldMany [1, 2])
                            CC.yieldMany [3]
                            CC.yieldMany [4]

case_groupC :: Assertion
case_groupC = shouldProduce [[0,1,2], [3,4,5], [6,7,8], [9, 10 :: Int]] $
                            CC.yieldMany [0..10] .| CAlg.groupC 3

case_enumerateC :: Assertion
case_enumerateC = shouldProduce [(0,'z'), (1,'o'), (2,'t')] $
                            CC.yieldMany ("zot" :: String) .| CAlg.enumerateC

case_removeRepeatsC :: Assertion
case_removeRepeatsC = shouldProduce [0,1,2,3,4,5,6,7,8,9, 10 :: Int] $
                            CC.yieldMany [0,0,0,1,1,1,2,2,3,4,5,6,6,6,6,7,7,8,9,10,10] .| CAlg.removeRepeatsC

case_asyncMap :: IO ()
case_asyncMap = do
    vals <- extractIO (CC.yieldMany [0..10] .| CAlg.asyncMapC 3 (+ (1:: Int)))
    vals @?= [1..11]

case_unorderedAsyncMapC :: IO ()
case_unorderedAsyncMapC = do
    vals <- extractIO (CC.yieldMany [0..10] .| CAlg.unorderedAsyncMapC 3 (+ (1:: Int)))
    sort vals @?= [1..11]

case_asyncGzip :: IO ()
case_asyncGzip = do
    C.runConduitRes (CC.yieldMany ["Hello", " ", "World"] .| CAlg.asyncGzipToFile testingFileNameGZ)
    r <- B.concat <$> extractIO (CAlg.asyncGzipFromFile testingFileNameGZ)
    r @?= "Hello World"
    removeFile testingFileNameGZ

case_asyncGzip_with_non_default_params :: IO ()
case_asyncGzip_with_non_default_params = do
    withFile testingFileNameGZ WriteMode $ \h -> do
        C.runConduitRes (CC.yieldMany ["Hello", " ", "World"] .| CAlg.asyncGzipTo' 1 h)
    r <- B.concat <$> extractIO (CAlg.asyncGzipFromFile testingFileNameGZ)
    r @?= "Hello World"
    removeFile testingFileNameGZ

case_asyncGzipLarge :: IO ()
case_asyncGzipLarge = do
    let repeats = 16384 -- large enough to cause the input to be split into blocks inside asyncGzipToFile
    C.runConduitRes (CC.yieldMany (concat $ replicate repeats ["Hello" :: B.ByteString, " ", "World\n"]) .| CAlg.asyncGzipToFile testingFileNameGZ)
    let checkHello !n = C.await >>= \case
                Nothing -> return n
                Just "Hello World\n" -> checkHello (n + 1)
                _ -> return (-1)
    r <- C.runConduitRes $
                CAlg.asyncGzipFromFile testingFileNameGZ
                    .| CC.chunksOfE 12
                    .| checkHello (0 :: Int)

    removeFile testingFileNameGZ
    r @?= repeats

case_asyncBzip2 :: IO ()
case_asyncBzip2 = do
    C.runConduitRes (CC.yieldMany ["Hello", " ", "World"] .| CAlg.asyncBzip2ToFile testingFileNameBZ2)
    r <- B.concat <$> extractIO (CAlg.asyncBzip2FromFile testingFileNameBZ2)
    r @?= "Hello World"
    removeFile testingFileNameBZ2

case_asyncXz :: IO ()
case_asyncXz = do
    C.runConduitRes (CC.yieldMany ["Hello", " ", "World"] .| CAlg.asyncXzToFile testingFileNameXZ)
    r <- B.concat <$> extractIO (CAlg.asyncXzFromFile testingFileNameXZ)
    r @?= "Hello World"
    removeFile testingFileNameXZ

case_async_gzip_to_from :: IO ()
case_async_gzip_to_from = do
    let testdata = [0 :: Int .. 12]
    C.runConduitRes $
        CC.yieldMany testdata
            .| CL.map (B8.pack . (\n -> show n ++ "\n"))
            .| CAlg.asyncGzipToFile testingFileNameGZ
    C.runConduitRes $
        CAlg.asyncGzipFromFile testingFileNameGZ
        .| CAlg.asyncGzipToFile testingFileNameGZ2
    shouldProduceIO testdata $
        CAlg.asyncGzipFromFile testingFileNameGZ2
            .| CB.lines
            .| CL.map (read . B8.unpack)
    removeFile testingFileNameGZ
    removeFile testingFileNameGZ2

case_withPossiblyCompressedFile :: IO ()
case_withPossiblyCompressedFile = do
    let testdata = [0 :: Int .. 12]
    C.runConduitRes $
        CC.yieldMany testdata
            .| CL.map (B8.pack . (\n -> show n ++ "\n"))
            .| CAlg.asyncGzipToFile testingFileNameGZ
    back <- R.runResourceT $
                CAlg.withPossiblyCompressedFile testingFileNameGZ $ \src ->
                    C.runConduit (src .| CB.lines .| CL.map (read . B8.unpack) .| CC.sinkList)
    removeFile testingFileNameGZ
    back @?= testdata

case_withPossiblyCompressedFileOutput :: IO ()
case_withPossiblyCompressedFileOutput = do
    let testdata = [0 :: Int .. 14]
    R.runResourceT $
        CAlg.withPossiblyCompressedFileOutput testingFileNameGZ $ \sink ->
            C.runConduit $
                CC.yieldMany testdata
                    .| CL.map (B8.pack . (\n -> show n ++ "\n"))
                    .| sink
    back <- R.runResourceT $
                CAlg.withPossiblyCompressedFile testingFileNameGZ $ \src ->
                    C.runConduit (src .| CB.lines .| CL.map (read . B8.unpack) .| CC.sinkList)
    removeFile testingFileNameGZ
    back @?= testdata

case_async_bzip2_to_from :: IO ()
case_async_bzip2_to_from = do
    let testdata = [0 :: Int .. 12]
    C.runConduitRes $
        CC.yieldMany testdata
            .| CL.map (B8.pack . (\n -> show n ++ "\n"))
            .| CAlg.asyncBzip2ToFile testingFileNameBZ2
    C.runConduitRes $
        CAlg.asyncBzip2FromFile testingFileNameBZ2
        .| CAlg.asyncBzip2ToFile testingFileNameBZ22
    shouldProduceIO testdata $
        CAlg.asyncBzip2FromFile testingFileNameBZ22
            .| CB.lines
            .| CL.map (read . B8.unpack)
    removeFile testingFileNameBZ2
    removeFile testingFileNameBZ22

case_async_xz_to_from :: IO ()
case_async_xz_to_from = do
    let testdata = [0 :: Int .. 12]
    C.runConduitRes $
        CC.yieldMany testdata
            .| CL.map (B8.pack . (\n -> show n ++ "\n"))
            .| CAlg.asyncXzToFile testingFileNameXZ
    C.runConduitRes $
        CAlg.asyncXzFromFile testingFileNameXZ
        .| CAlg.asyncXzToFile testingFileNameXZ2
    shouldProduceIO testdata $
        CAlg.asyncXzFromFile testingFileNameXZ2
            .| CB.lines
            .| CL.map (read . B8.unpack)
    removeFile testingFileNameXZ
    removeFile testingFileNameXZ2

case_async_zstd_to_from :: IO ()
case_async_zstd_to_from = do
    C.runConduitRes (CC.yieldMany ["Hello", " ", "World"] .| CAlg.asyncZstdToFile testingFileNameZstd)
    r <- B.concat <$> extractIO (CAlg.asyncZstdFromFile testingFileNameZstd)
    r @?= "Hello World"
    removeFile testingFileNameZstd

case_asyncFilterLines :: IO ()
case_asyncFilterLines = do
    vals <- extractIO (CC.yieldMany ["This is\nMy data\nBut"," sometimes","\nit is split,\n","in weird ways."] .| CAlg.asyncFilterLinesC 2 (B8.notElem ','))
    vals @?= ["This is", "My data", "But sometimes", "in weird ways."]

case_asyncFilterLinesAllTrue :: IO ()
case_asyncFilterLinesAllTrue = do
    vals <- extractIO (CC.yieldMany ["This is\nMy data\nBut"," sometimes","\nit is split,\n","in weird ways."] .| CAlg.asyncFilterLinesC 2 (const True))
    vals @?= ["This is", "My data", "But sometimes", "it is split,", "in weird ways."]

case_storableVector :: IO ()
case_storableVector = do
    let v = VS.fromList [0:: Int, 1, 2, 4, 6, 12]
    vals <- extractIO (CC.yieldMany [v,v,v] .| CAlg.writeStorableV .| CAlg.readStorableV 3)
    VS.concat vals @=? VS.concat [v,v,v]

case_dispatchC :: IO ()
case_dispatchC = do
    let v = [(0 :: Int, 0 :: Int),
                 (0, 1),
                 (1, 1),
                 (0, 2),
                 (-1, 3),
                 (2, -1),
                 (2, -1),
                 (3, -1),
                 (2, -1)
                ]
        acc !n = C.await >>= \case
                 Nothing -> return n
                 Just x -> acc (n + x)
    C.runConduitPure (CC.yieldMany v .| CAlg.dispatchC [acc i | i <- [0 :: Int ..2]]) @=? [6,2,-2 :: Int]
    C.runConduitPure (CC.yieldMany v .| CAlg.dispatchC_ [void (acc i) | i <- [0 :: Int ..2]]) @=? ()
