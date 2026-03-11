{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL

-- implementation under test
import qualified Crypto.Hash.SHA512t    as IUT

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck  as QC

vectors :: Int -> [ByteString]
vectors 224 =
    [ ""
    , "The quick brown fox jumps over the lazy dog"
    , "The quick brown fox jumps over the lazy cog"
    , "abc"
    , "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    , "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
    , B.replicate 1000000 0x61
    ]
vectors 256 = vectors 224

vectors 100 = [ "", "a" ]
vectors 128 = [ "", "a" ]
vectors 160 = [ "", "a" ]
vectors 192 = [ "", "a" ]
vectors _ = undefined

answers :: Int -> [ByteString]
answers 224 = map (B.filter (/= 0x20))
    [ "6ed0dd02806fa89e25de060c19d3ac86cabb87d6a0ddd05c333b84f4"
    , "944cd2847fb54558d4775db0485a50003111c8e5daa63fe722c6aa37"
    , "2b9d6565a7e40f780ba8ab7c8dcf41e3ed3b77997f4c55aa987eede5"
    , "4634270f707b6a54daae7530460842e20e37ed265ceee9a43e8924aa"
    , "e5302d6d54bb242275d1e7622d68df6eb02dedd13f564c13dbda2174"
    , "23fec5bb94d60b23308192640b0c453335d664734fe40e7268674af9"
    , "37ab331d76f0d36de422bd0edeb22a28accd487b7a8453ae965dd287"
    ]
answers 256 = map (B.filter (/= 0x20))
    [ "c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a"
    , "dd9d67b371519c339ed8dbd25af90e976a1eeefd4ad3d889005e532fc5bef04d"
    , "cc8d255a7f2f38fd50388fd1f65ea7910835c5c1e73da46fba01ea50d5dd76fb"
    , "53048e2681941ef99b2e29b76b4c7dabe4c2d0c634fc6d46e0e2f13107e7af23"
    , "bde8e1f9f19bb9fd3406c90ec6bc47bd36d8ada9f11880dbc8a22a7078b6a461"
    , "3928e184fb8690f840da3988121d31be65cb9d3ef83ee6146feac861e19b563a"
    , "9a59a052930187a97038cae692f30708aa6491923ef5194394dc68d56c74fb21"
    ]
answers 100 = map (B.filter (/= 0x20))
    [ "c00f7e9998dd8ee623557a8490"
    , "12ed51ef9eec8c369a200b6c50"
    ]
answers 128 = map (B.filter (/= 0x20))
    [ "deca5d803a5cfcbf4191e9fc4bc065e3"
    , "81dc223475633e08da618cca7053d145"
    ]
answers 160 = map (B.filter (/= 0x20))
    [ "4cc04bc7087617e98d7da7443d79fb481cf169bf"
    , "ffa50d50f314aa7f53e3a215e5789508c2342135"
    ]
answers 192 = map (B.filter (/= 0x20))
    [ "9896f27c73cdc4ecc8eca3e16f6eeb63afe04b6c0d39276c"
    , "08ce8ab4f3bbc3acc1959cd62527b63b4359a54012e167a5"
    ]
answers _ = undefined

ansXLTest :: Int -> ByteString
ansXLTest 224 = B.filter (/= 0x20) "9a7f86727c3be1403d6702617646b15589b8c5a92c70f1703cd25b52"
ansXLTest 256 = B.filter (/= 0x20) "b5855a6179802ce567cbf43888284c6ac7c3f6c48b08c5bc1e8ad75d12782c9e"
ansXLTest _   = ""

katTests :: Int -> [TestTree]
katTests t
  | length (vectors t) == length (answers t) = map makeTest (zip3 [1::Int ..] (vectors t) (answers t)) ++
                                               (if B.null (ansXLTest t) then [] else [xltest,xltest'])
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
    runTest = B16.encode . IUT.hash t

    runTest' :: ByteString -> ByteString
    runTest' = B16.encode . IUT.finalize . IUT.start t

    runTestInc :: Int -> ByteString -> ByteString
    runTestInc i = B16.encode . IUT.finalize . myfoldl' IUT.update (IUT.init t) . splitB i

    runTestLazy :: Int -> ByteString -> ByteString
    runTestLazy i = B16.encode . IUT.hashlazy t . BL.fromChunks . splitB i

    runTestLazy' :: Int -> ByteString -> ByteString
    runTestLazy' i = B16.encode . IUT.finalize . IUT.startlazy t . BL.fromChunks . splitB i

    -- force unaligned md5-blocks
    runTestLazyU :: Int -> ByteString -> ByteString
    runTestLazyU i = B16.encode . IUT.hashlazy t . BL.fromChunks . map B.copy . splitB i

    runTestLazyU' :: Int -> ByteString -> ByteString
    runTestLazyU' i = B16.encode . IUT.finalize . IUT.startlazy t . BL.fromChunks . map B.copy . splitB i


    ----

    xltest = testGroup "XL-vec"
        [ testCase "inc" (ansXLTest t @=? (B16.encode . IUT.hashlazy t) vecXL) ]
      where
        vecXL = BL.fromChunks (replicate 16777216 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno")

    xltest' = testGroup "XL-vec'"
        [ testCase "inc" (ansXLTest t @=? (B16.encode . IUT.finalize . IUT.startlazy t) vecXL) ]
      where
        vecXL = BL.fromChunks (replicate 16777216 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno")


splitB :: Int -> ByteString -> [ByteString]
splitB l b
  | B.length b > l = b1 : splitB l b2
  | otherwise = [b]
  where
    (b1, b2) = B.splitAt l b


rfc4231Vectors :: Int -> [(ByteString,ByteString,ByteString)]
rfc4231Vectors t = case t of   -- (secrect,msg,mac)
    224 -> [ (rep 20 0x0b, "Hi There", x"b244ba01307c0e7a8ccaad13b1067a4cf6b961fe0c6a20bda3d92039")
           , ("Jefe", "what do ya want for nothing?", x"4a530b31a79ebcce36916546317c45f247d83241dfb818fd37254bde")
           , (rep 20 0xaa, rep 50 0xdd, x"db34ea525c2c216ee5a6ccb6608bea870bbef12fd9b96a5109e2b6fc")
           , (B.pack [1..25], rep 50 0xcd, x"c2391863cda465c6828af06ac5d4b72d0b792109952da530e11a0d26")
           , (rep 20 0x0c, "Test With Truncation", x"1df8eae8baeedd4eddfb555ec0ba768f4b5ba29e9e3d55f08303120f")
           , (rep 131 0xaa, "Test Using Larger Than Block-Size Key - Hash Key First", x"29bef8ce88b54d4226c3c7718ea9e32ace2429026f089e38cea9aeda")
           , (rep 131 0xaa, "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.", x"82a9619b47af0cea73a8b9741355ce902d807ad87ee9078522a246e1")
           ]

    256 -> [ (rep 20 0x0b, "Hi There", x"9f9126c3d9c3c330d760425ca8a217e31feae31bfe70196ff81642b868402eab")
           , ("Jefe", "what do ya want for nothing?", x"6df7b24630d5ccb2ee335407081a87188c221489768fa2020513b2d593359456")
           , (rep 20 0xaa, rep 50 0xdd, x"229006391d66c8ecddf43ba5cf8f83530ef221a4e9401840d1bead5137c8a2ea")
           , (B.pack [1..25], rep 50 0xcd, x"36d60c8aa1d0be856e10804cf836e821e8733cbafeae87630589fd0b9b0a2f4c")
           , (rep 20 0x0c, "Test With Truncation", x"337f526924766971bf72b82ad19c2c825301791e3ae2d8bb4ec03817dd821f46")
           , (rep 131 0xaa, "Test Using Larger Than Block-Size Key - Hash Key First", x"87123c45f7c537a404f8f47cdbedda1fc9bec60eeb971982ce7ef10e774e6539")
           , (rep 131 0xaa, "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.", x"6ea83f8e7315072c0bdaa33b93a26fc1659974637a9db8a887d06c05a7f35a66")
           ]

    _ -> []
  where
    x = B16.decodeLenient
    rep n c = B.replicate n c

rfc4231Tests :: Int -> [TestTree]
rfc4231Tests t = zipWith makeTest [1::Int ..] (rfc4231Vectors t)
  where
    makeTest i (key, msg, mac) = testGroup ("vec"++show i) $
        [ testCase "hmac" (hex mac  @=? hex (IUT.hmac t key msg))
        , testCase "hmaclazy" (hex mac  @=? hex (IUT.hmaclazy t key lazymsg))
        ]
      where
        lazymsg = BL.fromChunks . splitB 1 $ msg

    hex = B16.encode

-- define own 'foldl' here to avoid RULE rewriting to 'hashlazy'
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


main :: IO ()
main = defaultMain $ testGroup "cryptohash-sha512t"
    [ testGroup "SHA512/224"
      [ testGroup "KATs" (katTests 224)
      , testGroup "RFC4231" (rfc4231Tests 224)
      ]
    , testGroup "SHA512/256"
      [ testGroup "KATs" (katTests 256)
      , testGroup "RFC4231" (rfc4231Tests 256)
      ]
    , testGroup "SHA512/100" [ testGroup "KATs" (katTests 100) ]
    , testGroup "SHA512/128" [ testGroup "KATs" (katTests 128) ]
    , testGroup "SHA512/160" [ testGroup "KATs" (katTests 160) ]
    , testGroup "SHA512/192" [ testGroup "KATs" (katTests 192) ]
    ]
