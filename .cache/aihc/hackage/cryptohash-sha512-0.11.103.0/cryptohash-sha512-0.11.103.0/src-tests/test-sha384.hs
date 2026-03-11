{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Word              (Word64)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Base16 as B16

-- reference implementation
import qualified Data.Digest.Pure.SHA   as REF

-- implementation under test
import qualified Crypto.Hash.SHA384     as IUT

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck  as QC

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
    [ "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"
    , "ca737f1014a48f4c0b6dd43cb177b0afd9e5169367544c494011e3317dbf9a509cb1e5dc1e85a941bbee3d7f2afbc9b1"
    , "098cea620b0978caa5f0befba6ddcf22764bea977e1c70b3483edfdf1de25f4b40d6cea3cadf00f809d422feb1f0161b"
    , "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"
    , "3391fdddfc8dc7393707a65b1b4709397cf8b1d162af05abfe8f450de5f36bc6b0455a8520bc4e6f5fe95b1fe3c8452b"
    , "09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039"
    , "9d0e1809716474cb086e834e310a4a1ced149e9c00f248527972cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985"
    ]

ansXLTest :: ByteString
ansXLTest = B.filter (/= 0x20)
    "5441235cc0235341ed806a64fb354742b5e5c02a3c5cb71b5f63fb793458d8fdae599c8cd8884943c04f11b31b89f023"

katTests :: [TestTree]
katTests
  | length vectors == length answers = map makeTest (zip3 [1::Int ..] vectors answers) ++ [xltest,xltest']
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
        [ testCase "inc" (ansXLTest @=? (B16.encode . IUT.finalize . IUT.startlazy) vecXL) ]
      where
        vecXL = BL.fromChunks (replicate 16777216 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno")


splitB :: Int -> ByteString -> [ByteString]
splitB l b
  | B.length b > l = b1 : splitB l b2
  | otherwise = [b]
  where
    (b1, b2) = B.splitAt l b


rfc4231Vectors :: [(ByteString,ByteString,ByteString)]
rfc4231Vectors = -- (secrect,msg,mac)
    [ (rep 20 0x0b, "Hi There", x"afd03944d84895626b0825f4ab46907f15f9dadbe4101ec682aa034c7cebc59cfaea9ea9076ede7f4af152e8b2fa9cb6")
    , ("Jefe", "what do ya want for nothing?", x"af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649")
    , (rep 20 0xaa, rep 50 0xdd, x"88062608d3e6ad8a0aa2ace014c8a86f0aa635d947ac9febe83ef4e55966144b2a5ab39dc13814b94e3ab6e101a34f27")
    , (B.pack [1..25], rep 50 0xcd, x"3e8a69b7783c25851933ab6290af6ca77a9981480850009cc5577c6e1f573b4e6801dd23c4a7d679ccf8a386c674cffb")
    , (rep 20 0x0c, "Test With Truncation", x"3abf34c3503b2a23a46efc619baef897f4c8e42c934ce55ccbae9740fcbc1af4ca62269e2a37cd88ba926341efe4aeea")
    , (rep 131 0xaa, "Test Using Larger Than Block-Size Key - Hash Key First", x"4ece084485813e9088d2c63a041bc5b44f9ef1012a2b588f3cd11f05033ac4c60c2ef6ab4030fe8296248df163f44952")
    , (rep 131 0xaa, "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.", x"6617178e941f020d351e2f254e8fd32c602420feb0b8fb9adccebb82461e99c5a678cc31e799176d3860e6110c46523e")
    ]
  where
    x = B16.decodeLenient
    rep n c = B.replicate n c

rfc4231Tests :: [TestTree]
rfc4231Tests = zipWith makeTest [1::Int ..] rfc4231Vectors
  where
    makeTest i (key, msg, mac) = testGroup ("vec"++show i) $
        [ testCase "hmac" (hex mac  @=? hex (IUT.hmac key msg))
        , testCase "hmaclazy" (hex mac  @=? hex (IUT.hmaclazy key lazymsg))
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
    ref_hashlazy = toStrict . REF.bytestringDigest . REF.sha384

    ref_hashlazyAndLength :: BL.ByteString -> (ByteString,Word64)
    ref_hashlazyAndLength x = (ref_hashlazy x, fromIntegral (BL.length x))

    ref_hmac :: ByteString -> ByteString -> ByteString
    ref_hmac secret = ref_hmaclazy secret . fromStrict

    ref_hmaclazy :: ByteString -> BL.ByteString -> ByteString
    ref_hmaclazy secret = toStrict . REF.bytestringDigest . REF.hmacSha384 (fromStrict secret)

    ref_hmaclazyAndLength :: ByteString -> BL.ByteString -> (ByteString,Word64)
    ref_hmaclazyAndLength secret msg = (ref_hmaclazy secret msg, fromIntegral (BL.length msg))

    -- toStrict/fromStrict only available with bytestring-0.10 and later
    toStrict = B.concat . BL.toChunks
    fromStrict = BL.fromChunks . (:[])

-- generated via `openssl kdf -kdfopt digest:SHA2-512`
hkdfVectors :: [(Int,ByteString,ByteString,ByteString,ByteString)]
hkdfVectors = -- (l,ikm,salt,info,okm)
    [ (42, rep 22 0x0b, x"000102030405060708090a0b0c", x"f0f1f2f3f4f5f6f7f8f9", x"9b5097a86038b805309076a44b3a9f38063e25b516dcbf369f394cfab43685f748b6457763e4f0204fc5")
    , ( 82
      , x"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f"
      , x"606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf"
      , x"b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
      , x"484ca052b8cc724fd1c4ec64d57b4e818c7e25a8e0f4569ed72a6a05fe0649eebf69f8d5c832856bf4e4fbc17967d54975324a94987f7f41835817d8994fdbd6f4c09c5500dca24a56222fea53d8967a8b2e"
      )
    , ( 42, rep 22 0x0b, "", "", x"c8c96e710f89b0d7990bca68bcdec8cf854062e54c73a7abc743fade9b242daacc1cea5670415b52849c")
    , ( 42, rep 11 0x0b, x"000102030405060708090a0b0c", x"f0f1f2f3f4f5f6f7f8f9", x"fb7e6743eb42cde96f1b70778952ab7548cafe53249f7ffe1497a1635b201ff185b93e951992d858f11a" )
    , ( 42, rep 22 0x0c, "", "", x"6ad7c726c84009546a76e0545df266787e2b2cd6ca4373a1f31450a7bdf9482bfab811f554200ead8f53" )
    , ( 1317, rep 17 0x33, "HeLoLoL", "WoRlDoD", x"01baa7d9cf33777f237967ddce0203a2d7269de62910f00963283b127ccd8546de690cf37b767bc1a435a54a4bcddac63a5eade30bffc1d0a91ceac2c0afc3ff5dea18436db433c071da8c7fbc51376a88b2c7a131bcb49056f37159455ac813d5fdbe8bb3e054a9f0b03f852c78a7b64ea1404298569cfebcf699a096405beeaa22147f522a3e4f4b30d27d3395282af9d5e56459005b2d09f41bb74530956760cd0271eef7ea002e670054fb0397470c5f34625428b3e1355cf12e96cab712af5e3bcc2cc706fd486b2ea921d80af0d094b0f6af0255d76ec1a0774fa0f6e65cbff319a5911ec899079e07dae2569b51d65ad09d610b8833d6b23081b109ad85282c008526bbd3d5c6225bf3866d5d0cedf3edaaa44ded4e318d07e050e22568b59402500d8a542ce90a2113d563e90e10998ccb9517da0f89dec82e2164f079f1b6f3b6176e738ae3620cf35013ad8edd9caf1e365e0af705b152db70898f1be05917c9a912cfccee3de5e667e55bd4bc452fb8ecb87884b6cde738e12a77f2e3c7e1ede76880b1aa34a9ad4ac5e6ecff1ee6bd18784ace50860aab1a6b17672d8e3a146444c95b542b4f1109bf6e92a8b70de3842a172b439ee2658a8d35c9ed07162b21af3438c2efa670f08f0d5af33ed6e4b64d7970ca2c16d820cf76baa8951d1bfe0a6443688c0a31b435711928c48095f078437a39fccea10cf1c871dcce78323a6563ca5ccea8e441f540fc4e5659a0e6f77501efa6c1cd64cc381fc512d7ccebc4a4543168ee8b154792b250f618a0b3f04a62c72962a1ee4cad38aff8a33a3fef1d0f2bcd58e7056a0977446e836aa4a857b7c3eeb6a4d7ef30132db99cc3707134100c2b1383d1bfef218166abbba496a961e91f12c4eb98e25f259f5f9753a1caab56a49930ef3b68f4d13a99045724a91cf653b8986ca331861242a7cb90ed8a112243297997f78542e59cd8c5441e02bcf27ff802de52a166d4165c6468ad65e60c93c64ee1a8c0e5e1f1189c8ebea7f94c22dd002513c22508b9e66eb2816762e420f92732c50e1232053d4fc25fd15c3f47bf1a0777a8b392122a510484721803f9cbf6d707a1ca1771d632d79174022842a2cd6cbaf71928648e54d793f984d3f55619b9ca507ea3c326c344eed1900014406e98de675098ad2b488e10f0c4b3cd7972983b2e8860d7ed6fd63f3a40160dacd43111ae325e4e92c1284ab9bd23001038a3066c936404b1fa3a6804dc1b4a058d4ee985d0c0b6d10572c93849206cdaffdb9f979faf07194306c9fa254f45084d4559a5859ca555161d812c44909af881d7e420b6ff918e6c17ac3e1bf1f3761b72a54efe906435432fb599cab9e588ba6058ced12993a7a19b2c0dd94e01865f1c2414e216c82d8c7d68470d4e78779ce2f5dd0ac4cb7524718468eda89c1fdbe1fd144d4c5c34249075f68dc5a5c709ea81b84b5f845844efdda1ef191723535ce9644a71aec9e3c0ddb4c3cf6ec2b3370bb5e34a2704c7c9bf74cc2d84705e4280388cfade633a5885beab5fea84bf41ef6ca161522b403b6395d249a8c85a8c059a2542d2347bb5c2dd359830163455ed81506f6382147c26a77dadaf477dc7907a479310f9c387017d52c8970eddd4cd19e8213ed892e3a8918a4159ed993089a8c0c4f54742d18d3b208b449ae0145f8287ca0aa78a5bebce79b2c69a6bb2bdb6536e74beebf48c150fb227ae93470bab594d35e964f13c6449977610aff90a97b94f035358ea058c23981caf5ab60e95c18255be5c58b8d453cd55a3e12a34d478308ea298d7dd952f16986bb62d8fcdcb30fd4df44d56483a4cd8721ba833e319bc08f934")
    ]
  where
    x = B16.decodeLenient
    rep n c = B.replicate n c

hkdfTests :: [TestTree]
hkdfTests = zipWith makeTest [1::Int ..] hkdfVectors
  where
    makeTest i (l,ikm,salt,info,okm) = testGroup ("vec"++show i) $
        [ testCase "hkdf" (hex okm @=? hex (IUT.hkdf ikm salt info l)) ]

    hex = B16.encode

main :: IO ()
main = defaultMain $ testGroup "cryptohash-sha384"
    [ testGroup "KATs" katTests
    , testGroup "RFC4231" rfc4231Tests
    , testGroup "REF" refImplTests
    , testGroup "HKDF" hkdfTests
    ]
