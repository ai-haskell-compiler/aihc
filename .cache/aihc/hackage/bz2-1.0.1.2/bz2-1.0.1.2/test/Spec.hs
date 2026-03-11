module Main (main) where

import           Codec.Compression.BZip
import           Control.Concurrent     (forkIO, newEmptyMVar, putMVar, readMVar)
import           Control.DeepSeq        (deepseq)
import qualified Data.ByteString.Lazy   as BSL
import           System.Directory       (doesFileExist)
import           System.FilePath        ((-<.>))
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

testDecompress :: FilePath -> TestTree
testDecompress fp =
    goldenVsString ("Decompress " ++ fp) (fp -<.> ".ref") (decompress <$> BSL.readFile fp)

forceHead :: BSL.ByteString -> IO ()
forceHead bsl = head (BSL.toChunks bsl) `seq` pure ()

forceBSL :: BSL.ByteString -> IO ()
forceBSL bsl = last (BSL.toChunks bsl) `seq` pure ()

testCompress :: FilePath -> TestTree
testCompress fp = testCase ("Roundtrip " ++ fp) $ do
    contents <- BSL.readFile fp
    let actual = decompress (compress contents)
    actual @?= contents

testValgrind :: TestTree
testValgrind = testCase "Unpack valgrind" $ do
    contents <- BSL.readFile "valgrind-3.15.0.tar.bz2"
    let actual = decompress contents
    assertBool "Unpacks w/o error" (actual `deepseq` True)

testValgrindPack :: TestTree
testValgrindPack = testCase "Pack valgrind" $ do
    contents <- BSL.readFile "valgrind-3.15.0.tar"
    -- let actual = compress contents
    let preRes = compress contents
    forceHead preRes
    res <- newEmptyMVar
    _ <- forkIO (forceBSL preRes)
    _ <- forkIO (forceBSL preRes *> putMVar res preRes)
    actual <- readMVar res
    assertBool "Packs w/o error" (actual `deepseq` True)

-- if decompressing invalid data, bail out
testNoLoop :: TestTree
testNoLoop = testCase "Decompress partial" $ do
    contents <- BSL.readFile "test/data/sample2.bz2"
    let partial = BSL.take (64 * 1024) contents
        res = decompressErr partial
    res @?= Left BzUnexpectedEof

main :: IO ()
main = do
    valgrind <- (&&) <$> doesFileExist "valgrind-3.15.0.tar.bz2" <*> doesFileExist "valgrind-3.15.0.tar"
    let go =
            if valgrind
                then (testValgrind:) . (testValgrindPack:)
                else id

    defaultMain $
        testGroup "bz2" (go [testNoLoop, testDecompression, testCompression])

    where
        compressedFiles = [ "test/data/sample1.bz2"
                          , "test/data/sample2.bz2"
                          , "test/data/sample3.bz2"
                          ]
        testDecompression = testGroup "Decompress"
            (testDecompress <$> compressedFiles)
        testCompression = testGroup "Compress"
            [ testCompress "test/data/sample1.ref"
            , testCompress "test/data/sample2.ref"
            , testCompress "test/data/sample3.ref"
            ]
