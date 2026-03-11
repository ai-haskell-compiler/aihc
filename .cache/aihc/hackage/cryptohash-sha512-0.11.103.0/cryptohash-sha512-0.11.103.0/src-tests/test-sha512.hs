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
import qualified Crypto.Hash.SHA512     as IUT

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
    [ "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
    , "07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6"
    , "3eeee1d0e11733ef152a6c29503b3ae20c4f1f3cda4cb26f1bc1a41f91c7fe4ab3bd86494049e201c4bd5155f31ecb7a3c8606843c4cc8dfcab7da11c8ae5045"
    , "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
    , "204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c33596fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445"
    , "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"
    , "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b"
    ]

ansXLTest :: ByteString
ansXLTest = B.filter (/= 0x20)
    "b47c933421ea2db149ad6e10fce6c7f93d0752380180ffd7f4629a712134831d77be6091b819ed352c2967a2e2d4fa5050723c9630691f1a05a7281dbe6c1086"

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
    [ (rep 20 0x0b, "Hi There", x"87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854")
    , ("Jefe", "what do ya want for nothing?", x"164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737")
    , (rep 20 0xaa, rep 50 0xdd, x"fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb")
    , (B.pack [1..25], rep 50 0xcd, x"b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd")
    , (rep 20 0x0c, "Test With Truncation", x"415fad6271580a531d4179bc891d87a650188707922a4fbb36663a1eb16da008711c5b50ddd0fc235084eb9d3364a1454fb2ef67cd1d29fe6773068ea266e96b")
    , (rep 131 0xaa, "Test Using Larger Than Block-Size Key - Hash Key First", x"80b24263c7c1a3ebb71493c1dd7be8b49b46d1f41b4aeec1121b013783f8f3526b56d037e05f2598bd0fd2215d6a1e5295e64f73f63f0aec8b915a985d786598")
    , (rep 131 0xaa, "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.", x"e37b6a775dc87dbaa4dfa9f96e5e3ffddebd71f8867289865df5a32d20cdc944b6022cac3c4982b10d5eeb55c3e4de15134676fb6de0446065c97440fa8c6a58")
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
    ref_hashlazy = toStrict . REF.bytestringDigest . REF.sha512

    ref_hashlazyAndLength :: BL.ByteString -> (ByteString,Word64)
    ref_hashlazyAndLength x = (ref_hashlazy x, fromIntegral (BL.length x))

    ref_hmac :: ByteString -> ByteString -> ByteString
    ref_hmac secret = ref_hmaclazy secret . fromStrict

    ref_hmaclazy :: ByteString -> BL.ByteString -> ByteString
    ref_hmaclazy secret = toStrict . REF.bytestringDigest . REF.hmacSha512 (fromStrict secret)

    ref_hmaclazyAndLength :: ByteString -> BL.ByteString -> (ByteString,Word64)
    ref_hmaclazyAndLength secret msg = (ref_hmaclazy secret msg, fromIntegral (BL.length msg))

    -- toStrict/fromStrict only available with bytestring-0.10 and later
    toStrict = B.concat . BL.toChunks
    fromStrict = BL.fromChunks . (:[])

-- generated via `openssl kdf -kdfopt digest:SHA2-512`
hkdfVectors :: [(Int,ByteString,ByteString,ByteString,ByteString)]
hkdfVectors = -- (l,ikm,salt,info,okm)
    [ (42, rep 22 0x0b, x"000102030405060708090a0b0c", x"f0f1f2f3f4f5f6f7f8f9", x"832390086cda71fb47625bb5ceb168e4c8e26a1a16ed34d9fc7fe92c1481579338da362cb8d9f925d7cb")
    , ( 82
      , x"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f"
      , x"606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf"
      , x"b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
      , x"ce6c97192805b346e6161e821ed165673b84f400a2b514b2fe23d84cd189ddf1b695b48cbd1c8388441137b3ce28f16aa64ba33ba466b24df6cfcb021ecff235f6a2056ce3af1de44d572097a8505d9e7a93"
      )
    , ( 42, rep 22 0x0b, "", "", x"f5fa02b18298a72a8c23898a8703472c6eb179dc204c03425c970e3b164bf90fff22d04836d0e2343bac")
    , ( 42, rep 11 0x0b, x"000102030405060708090a0b0c", x"f0f1f2f3f4f5f6f7f8f9", x"7413e8997e020610fbf6823f2ce14bff01875db1ca55f68cfcf3954dc8aff53559bd5e3028b080f7c068" )
    , ( 42, rep 22 0x0c, "", "", x"1407d46013d98bc6decefcfee55f0f90b0c7f63d68eb1a80eaf07e953cfc0a3a5240a155d6e4daa965bb" )
    , ( 1317, rep 17 0x33, "HeLoLoL", "WoRlDoD", x"e363393c57e4ab1a475bd8750ee08d96b024cd8252c06ae9d3a338e0f68f09d6e138777ea7039f6924f76652b93966c35486afe804aa0f210139aa88189ca930f21f27f12bdcc73f95a352ae6c158ac729cc09b32c68fb8322fc5cafa378b0e123dfe8be44fdf242384e61a42e59c13646a2ef5e4931eef2ed8cd38da6f439cc3b6e1ea541aae2b9c59de7a194e9e7662ee156c67ad93f2c04313a6a431f784b12e8feafcc4cd4d98797e9d753b666a4c4f9c4981ae70255e5e3adcdfd8aaf08faae09a601a144af880cf98948be6dfe90114f6615cdc1edc4446451daaa1829b881e7f8968db279a03542a9729b8c08020320fb46c86c3dba9fdbb80767d487b911b52012deaa10dce7079afaad6f76a188945f99d7ba53cbaace919b9ce63cff2fd50e8ade7f5eb452ac2ea70be546784c4af03019519e971d3a2f6317f73c86d0b0c6cc6fe32163bdb34178a2c1f10d2d892bc06854f08e4bbca66ae7b57e915c0b57a90e2b624399f973d83675ccddee069e5ee150c6bd2a415d8a327d6fd7d3dfa49ec407d705fd5ac018d5285d2da8fdcba7494496a5a5ef753169bf4eae9dd5491ca11d1c7eb4255d404fc0dfa34e4605ab1b6495676aad0701e96c677ef4e4a87d661775ccaf7aaed60835219a391280ec786b08380f59780a744faa7313ac06f2ed0ef292c5b43ae7824e8ff8601957d570562feaadb011bb6a809bbc3ae3379f713579ffbe8c0fc46205d831ee3db910eb011aea8d4fbe040c15596c63f273dd5f0e386632eb27506d1ec9d1c5ec516c63406b1e9326145888244629b1dcdf877777532c05369dd42eff3871995d278b73123086625d3bf029cc3297aab6646d340a99da3008016d58c753504fd5c53c58ae2aa4d8ba6eae53f0b0ec5d3a3ece14f4e9286abbae628475f402f9a3b8f73d58aeb01ac43ee531784a8fd7c6b260f4ad69fdfe0bf641df8ab0d98be0df922f58b90840883621b26450c8e7c4b3fd0fbb09c3708270d26d976c96f41fc7ddf2c5470d0b39efa3e6c2762d2b388438f7520a4d5ca6ed80adc04fda438dbd542ff06335422d3055d885f04795e7369686b22f1181153920420b4a110ccd912af0ef6767b4c31ace6f39b6d203ac1694d65c6c6255ac5a6b2f82b9f700fffc3e37d416e919e6c509fa1b49186b6387dbd472b93cc38a8a8793aab9a1c4fa8960c989687f0266563db2f2ec083bea1ce1d479a5dde3a74587cb4f7e240f224129ff674f8b7d2ce6e079700300a655cb6aaa2e36d34c8d20f330a4fcb5b5a968b1fb3a1e189ca00c289fb7bc75ee9880c30dd807a4ff497f6156c03eb9771d9af1f767ca2395158f9eec133c585d554f6aa1f28fb1d78c0bbd764532c59c564a87abb5b6980c63351b32561a1eca0998f9378b9f39cc219c9b5c9cc3a09e1398ce019469a88c9b5ee77842e1ce458a11aef5d7002dd5ea6ca3f0f5fd533be56ee43bef39ec0a2ab1561570a5d4e2936b91e3c16b214b80271610f67d35edd877878f242b34fe34329355b09fd6f817c5cedef92c0198e62a15f1e526de0f66ed2afa0b388320008cafb62bfd8d0c447e8db920c526513ecbabcd806a1bda7e6b97d2f225a53b20e705c51e1c1bb646343a851fe0eadb89df13ac8a82c9c865d47a299d7800cbc50fb0296e0b44014e548c7a8d2ca2efde39e4794112194fd71a75431f1521719676d1b72f57ec00989052c3f62b7298d3d60fcd3345726b321f3b96e83a4d0a97851bc515b3c7f382dd5a2db0724546402a5c89b0f34202129358e53576124a5f4a7591a2eb5eb319a979d0701e16fde1357bcec651bb2f858fe1dcd6fa5eb5195f71152d1a460b5f36e9")
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
main = defaultMain $ testGroup "cryptohash-sha512"
    [ testGroup "KATs" katTests
    , testGroup "RFC4231" rfc4231Tests
    , testGroup "REF" refImplTests
    , testGroup "HKDF" hkdfTests
    ]
