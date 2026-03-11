{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Bits              (xor)
import           Data.Word              (Word64)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Base16 as B16

-- reference implementation
import qualified Data.Digest.Pure.MD5 as REF

-- implementation under test
import qualified Crypto.Hash.MD5      as IUT

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

vectors :: [ByteString]
vectors =
    [ ""
    , "The quick brown fox jumps over the lazy dog"
    , "The quick brown fox jumps over the lazy cog"
    , "abc"
    , "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    , "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
    , B.replicate 1000000 0x61
    ]

answers :: [ByteString]
answers = map (B.filter (/= 0x20))
    [ "d41d8cd9 8f00b204 e9800998 ecf8427e"
    , "9e107d9d 372bb682 6bd81d35 42a419d6"
    , "1055d3e6 98d289f2 af866372 5127bd4b"
    , "90015098 3cd24fb0 d6963f7d 28e17f72"
    , "8215ef07 96a20bca aae116d3 876c664a"
    , "03dd8807 a93175fb 062dfb55 dc7d359c"
    , "7707d6ae 4e027c70 eea2a935 c2296f21"
    ]

ansXLTest :: ByteString
ansXLTest = B.filter (/= 0x20)
    "d3381391 69d50f55 526194c7 90ec0448"

katTests :: [TestTree]
katTests
  | length vectors == length answers = map makeTest (zip3 [1::Int ..] vectors answers) ++ [xltest, xltest']
  | otherwise = error "vectors/answers length mismatch"
  where
    makeTest (i, v, r) = testGroup ("vec"++show i) $
        [ testCase "one-pass" (r @=? runTest v)
        , testCase "one-pass'" (r @=? runTest' v)
        , testCase "inc-1"    (r @=? runTestInc 1 v)
        , testCase "inc-2"    (r @=? runTestInc 2 v)
        , testCase "inc-3"    (r @=? runTestInc 3 v)
        , testCase "inc-4"    (r @=? runTestInc 4 v)
        , testCase "inc-5"    (r @=? runTestInc 5 v)
        , testCase "inc-7"    (r @=? runTestInc 7 v)
        , testCase "inc-8"    (r @=? runTestInc 8 v)
        , testCase "inc-9"    (r @=? runTestInc 9 v)
        , testCase "inc-16"   (r @=? runTestInc 16 v)
        , testCase "lazy-1"   (r @=? runTestLazy 1 v)
        , testCase "lazy-2"   (r @=? runTestLazy 2 v)
        , testCase "lazy-7"   (r @=? runTestLazy 7 v)
        , testCase "lazy-8"   (r @=? runTestLazy 8 v)
        , testCase "lazy-16"  (r @=? runTestLazy 16 v)
        , testCase "lazy-1'"   (r @=? runTestLazy' 1 v)
        , testCase "lazy-2'"   (r @=? runTestLazy' 2 v)
        , testCase "lazy-7'"   (r @=? runTestLazy' 7 v)
        , testCase "lazy-8'"   (r @=? runTestLazy' 8 v)
        , testCase "lazy-16'"  (r @=? runTestLazy' 16 v)
        ] ++
        [ testCase "lazy-63u"  (r @=? runTestLazyU 63 v) | B.length v > 63 ] ++
        [ testCase "lazy-65u"  (r @=? runTestLazyU 65 v) | B.length v > 65 ] ++
        [ testCase "lazy-97u"  (r @=? runTestLazyU 97 v) | B.length v > 97 ] ++
        [ testCase "lazy-131u" (r @=? runTestLazyU 131 v) | B.length v > 131 ] ++
        [ testCase "lazy-63u'"  (r @=? runTestLazyU' 63 v) | B.length v > 63 ] ++
        [ testCase "lazy-65u'"  (r @=? runTestLazyU' 65 v) | B.length v > 65 ] ++
        [ testCase "lazy-97u'"  (r @=? runTestLazyU' 97 v) | B.length v > 97 ] ++
        [ testCase "lazy-131u'" (r @=? runTestLazyU' 131 v) | B.length v > 131 ]

    runTest :: ByteString -> ByteString
    runTest = B16.encode . IUT.hash

    runTest' :: ByteString -> ByteString
    runTest' = B16.encode . IUT.finalize . IUT.start

    runTestInc :: Int -> ByteString -> ByteString
    runTestInc i = B16.encode . IUT.finalize . myfoldl' IUT.update IUT.init . splitB i

    runTestLazy :: Int -> ByteString -> ByteString
    runTestLazy i = B16.encode . IUT.hashlazy . BL.fromChunks . splitB i

    runTestLazy' :: Int -> ByteString -> ByteString
    runTestLazy' i = B16.encode . IUT.finalize . IUT.startlazy . BL.fromChunks . splitB i

    -- force unaligned md5-blocks
    runTestLazyU :: Int -> ByteString -> ByteString
    runTestLazyU i = B16.encode . IUT.hashlazy . BL.fromChunks . map B.copy . splitB i

    runTestLazyU' :: Int -> ByteString -> ByteString
    runTestLazyU' i = B16.encode . IUT.finalize . IUT.startlazy . BL.fromChunks . map B.copy . splitB i

    ----

    xltest = testGroup "XL-vec"
        [ testCase "inc" (ansXLTest @=? (B16.encode . IUT.hashlazy) vecXL) ]
      where
        vecXL = BL.fromChunks (replicate 16777216 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno")

    xltest' = testGroup "XL-vec'"
        [ testCase "inc'" (ansXLTest @=? (B16.encode . IUT.finalize . IUT.startlazy) vecXL) ]
      where
        vecXL = BL.fromChunks (replicate 16777216 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno")

splitB :: Int -> ByteString -> [ByteString]
splitB l b
  | B.length b > l = b1 : splitB l b2
  | otherwise = [b]
  where
    (b1, b2) = B.splitAt l b


rfc2202Vectors :: [(ByteString,ByteString,ByteString)]
rfc2202Vectors = -- (secrect,msg,mac)
    [ (rep 16 0x0b, "Hi There", x"9294727a3638bb1c13f48ef8158bfc9d")
    , ("Jefe", "what do ya want for nothing?", x"750c783e6ab0b503eaa86e310a5db738")
    , (rep 16 0xaa, rep 50 0xdd, x"56be34521d144c88dbb8c733f0e8b3f6")
    , (B.pack [1..25], rep 50 0xcd, x"697eaf0aca3a3aea3a75164746ffaa79")
    , (rep 16 0x0c, "Test With Truncation", x"56461ef2342edc00f9bab995690efd4c")
    , (rep 80 0xaa, "Test Using Larger Than Block-Size Key - Hash Key First", x"6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd")
    , (rep 80 0xaa, "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data", x"6f630fad67cda0ee1fb1f562db3aa53e")
    ]
  where
    x = B16.decodeLenient
    rep n c = B.replicate n c

rfc2202Tests :: [TestTree]
rfc2202Tests = zipWith makeTest [1::Int ..] rfc2202Vectors
  where
    makeTest i (key, msg, mac) = testGroup ("vec"++show i) $
        [ testCase "hmac" (hex mac  @=? hex (IUT.hmac key msg))
        , testCase "hmaclazy" (hex mac  @=? hex (IUT.hmaclazy key lazymsg))
        ]
      where
        lazymsg = BL.fromChunks . splitB 1 $ msg

    hex = B16.encode

-- define own 'foldl' here to avoid RULE rewriting to 'startlazy
myfoldl' :: (b -> a -> b) -> b -> [a] -> b
myfoldl' f z0 xs0 = lgo z0 xs0
  where
    lgo z []     = z
    lgo z (x:xs) = let z' = f z x
                   in z' `seq` lgo z' xs

newtype RandBS = RandBS { unRandBS :: ByteString }
newtype RandLBS = RandLBS BL.ByteString

instance Arbitrary RandBS where
    arbitrary = fmap (RandBS . B.pack) arbitrary
    shrink (RandBS x) = fmap RandBS (go x)
      where
        go bs = zipWith B.append (B.inits bs) (tail $ B.tails bs)

instance Show RandBS where
    show (RandBS x) = "RandBS {len=" ++ show (B.length x)++"}"

instance Arbitrary RandLBS where
    arbitrary = fmap (RandLBS . BL.fromChunks . map unRandBS) arbitrary

instance Show RandLBS where
    show (RandLBS x) = "RandLBS {len=" ++ show (BL.length x) ++ ", chunks=" ++ show (length $ BL.toChunks x)++"}"

refImplTests :: [TestTree]
refImplTests =
    [ testProperty "hash" prop_hash
    , testProperty "start" prop_start
    , testProperty "hashlazy" prop_hashlazy
    , testProperty "startlazy" prop_startlazy
    , testProperty "hashlazyAndLength" prop_hashlazyAndLength
    , testProperty "hmac" prop_hmac
    , testProperty "hmaclazy" prop_hmaclazy
    , testProperty "hmaclazyAndLength" prop_hmaclazyAndLength
    ]
  where
    prop_hash (RandBS bs)
        = ref_hash bs == IUT.hash bs

    prop_start (RandBS bs)
        = ref_hash bs == (IUT.finalize $ IUT.start bs)

    prop_hashlazy (RandLBS bs)
        = ref_hashlazy bs == IUT.hashlazy bs

    prop_hashlazyAndLength (RandLBS bs)
        = ref_hashlazyAndLength bs == IUT.hashlazyAndLength bs

    prop_startlazy (RandLBS bs)
        = ref_hashlazy bs == (IUT.finalize $ IUT.startlazy bs)

    prop_hmac (RandBS k) (RandBS bs)
        = ref_hmac k bs == IUT.hmac k bs

    prop_hmaclazy (RandBS k) (RandLBS bs)
        = ref_hmaclazy k bs == IUT.hmaclazy k bs

    prop_hmaclazyAndLength (RandBS k) (RandLBS bs)
        = ref_hmaclazyAndLength k bs == IUT.hmaclazyAndLength k bs

    ref_hash :: ByteString -> ByteString
    ref_hash = ref_hashlazy . fromStrict

    ref_hashlazy :: BL.ByteString -> ByteString
    ref_hashlazy = REF.md5DigestBytes . REF.md5

    ref_hashlazyAndLength :: BL.ByteString -> (ByteString,Word64)
    ref_hashlazyAndLength x = (ref_hashlazy x, fromIntegral (BL.length x))

    -- stolen & adapted from SHA package
    ref_hmac :: ByteString -> ByteString -> ByteString
    ref_hmac k m = ref_hash (B.append opad (ref_hash (B.append ipad m)))
     where
      opad = B.map (xor 0x5c) k'
      ipad = B.map (xor 0x36) k'

      k' = B.append kt pad
       where
        kt  = if kn > bn then ref_hash k else k
        pad = B.replicate (bn - ktn) 0
        kn  = B.length k
        ktn = B.length kt
        bn  = 64

    ref_hmaclazy :: ByteString -> BL.ByteString -> ByteString
    ref_hmaclazy secret = ref_hmac secret . toStrict

    ref_hmaclazyAndLength :: ByteString -> BL.ByteString -> (ByteString,Word64)
    ref_hmaclazyAndLength secret msg = (ref_hmaclazy secret msg, fromIntegral (BL.length msg))

    -- toStrict/fromStrict only available with bytestring-0.10 and later
    toStrict = B.concat . BL.toChunks
    fromStrict = BL.fromChunks . (:[])

main :: IO ()
main = defaultMain $ testGroup "cryptohash-md5"
    [ testGroup "KATs" katTests
    , testGroup "RFC2202" rfc2202Tests
    , testGroup "REF" refImplTests
    ]
