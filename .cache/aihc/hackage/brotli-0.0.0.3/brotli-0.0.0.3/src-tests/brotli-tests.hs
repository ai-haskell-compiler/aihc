{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.ByteString.Lazy.Char8  ()
import           Data.List                   (intersperse)
import           Prelude

import           Test.Tasty
import           Test.Tasty.QuickCheck       as QC
import           Test.Tasty.HUnit

import           Codec.Compression.Brotli    as Brotli

main :: IO ()
main = defaultMain tests

-- this is supposed to be equivalent to 'id'
codecompress :: BL.ByteString -> BL.ByteString
codecompress = decompress . compress

newtype ZeroBS = ZeroBS BL.ByteString

instance Show ZeroBS where
    show (ZeroBS s) | BL.length s > 0 = "ZeroBS (replicate " ++ show (BL.length s) ++ " " ++ show (BL.head s) ++ ")"
                    | otherwise = "ZeroBS (empty)"

instance Arbitrary ZeroBS where
  arbitrary = do
    len <- choose (0, 1*1024*1024) -- up to 1MiB
    return $ (ZeroBS $ BL.replicate len 0)

--  shrink (ABS bs) = map ABS $ shrinks bs

randBS :: Int -> Gen BS.ByteString
randBS n = BS.pack `fmap` vectorOf n (choose (0, 255))

randBL :: Gen BL.ByteString
randBL = do
    ns <- arbitrary
    chunks <- mapM (randBS . (`mod` 10240)) ns
    return $ BL.fromChunks chunks

newtype RandBLSm = RandBLSm BL.ByteString
                 deriving Show

newtype RandBL = RandBL BL.ByteString
               deriving Show

instance Arbitrary RandBL where
    arbitrary = RandBL <$> randBL

instance Arbitrary RandBLSm where
    arbitrary = do
        n <- choose (0,1024)
        RandBLSm . BL.fromChunks . (:[]) <$> randBS n

tests :: TestTree
tests = testGroup "ByteString API" [unitTests, properties]
  where
    unitTests  = testGroup "testcases"
        [ testCase "decode-empty" $ decompress nullxz @?= BL.empty
        , testCase "encode-empty" $ codecompress BL.empty @?= BL.empty
        , testCase "encode-hello" $ codecompress "hello" @?= "hello"
        , testCase "encode-hello2" $ codecompress (singletonChunked "hello") @?= "hello"
        , testCase "decode-sample" $ decompress samplexz @?= sampleref
        , testCase "decode-sample2" $ decompress (singletonChunked samplexz) @?= sampleref
        , testCase "encode-sample" $ codecompress sampleref @?= sampleref
        , testCase "encode-empty^50" $ (iterate decompress (iterate (compressWith lowProf) "" !! 50) !! 50) @?= ""
        , testCase "encode-10MiB-zeros" $ let z = BL.replicate (10*1024*1024) 0 in codecompress z @?= z
        ]

    properties = testGroup "properties"
        [ QC.testProperty "decompress . compress === id (zeros)" $
          \(ZeroBS bs) -> codecompress bs == bs

        , QC.testProperty "decompress . compress === id (chunked)" $
          \(RandBL bs) -> codecompress bs == bs

-- We don't support concatenation yet
--      , QC.testProperty "decompress . (compress a <> compress b) === a <> b" $
--        \(RandBLSm a) (RandBLSm b) -> decompress (compress a `mappend` compress b) == a `mappend` b
        ]

    lowProf = defaultCompressParams { compressLevel = CompressionLevel0 }

nullxz :: BL.ByteString
nullxz = BL.singleton 0x3f

samplexz :: BL.ByteString
samplexz = BL.pack [27,149,1,232,37,169,208,210,122,16,67,60,33,76,195,0,85,117,73,135,40,94,219,146,49,252,21,2]

singletonChunked :: BL.ByteString -> BL.ByteString
singletonChunked = BL.fromChunks . map BS.singleton . BL.unpack

sampleref :: BL.ByteString
sampleref = BL.concat (intersperse " " $ replicate 11 "This sentence occurs multiple times.")
