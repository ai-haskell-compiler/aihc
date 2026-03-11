{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ViewPatterns, NoMonomorphismRestriction #-}
{- |
  Maintainer: Thomas.DuBuisson@gmail.com
  Stability: beta
  Portability: portable 


  Basic tests for some common cryptographic algorithms
  Most user only need to run the {make,run}Tests functions:

@        runTests (makeMD5Tests (undefined :: MD5Digest))
@
 
   or
  
@       runTests =<< makeAESTests (undefined :: AESKey)
@
 
   TODO: More KATs are needed - particularly ones for non-AES, SHA, or MD5
   algorithms.
-}
module Test.Crypto
        (
        -- * Block Cipher KATs
          makeBlockCipherPropTests
        -- * Hash property tests
        , makeHashPropTests
        -- * Utils
        , hexStringToBS
        -- * Re-exported
        , defaultMain
        ) where

import Test.QuickCheck
import Test.ParseNistKATs
import Crypto.Classes
import Crypto.Types
import Crypto.Padding
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Monad (forM)
import qualified Data.Serialize as Ser
import Numeric (readHex)
import Control.Arrow (first,second)

import Test.HUnit.Base (assertEqual)
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

instance Arbitrary B.ByteString where
    arbitrary = do
        len <- choose (0,4096) :: Gen Int
        words <- forM [0..len] (\_ -> arbitrary)
        return $ B.pack words

instance Arbitrary L.ByteString where
    arbitrary = do
        len <- choose (0,10) :: Gen Int
        chunks <- vector len
        return $ L.fromChunks chunks

-- |Verify hashing a lazy bytestring is the same as
-- hashing the strict bytestring equivalent.
prop_LazyStrictEqual :: Hash c d => d -> L.ByteString -> Bool
prop_LazyStrictEqual d lps =
    let strict   = B.concat $ L.toChunks lps
        f  = hashFunc d
        f' = hashFunc' d
    in f lps == f' strict

-- |Verify the Serialize and Binary instances result
-- in bytestrings of the correct length for a given digest
prop_DigestLen :: Hash c d => d -> L.ByteString -> Bool
prop_DigestLen d lps =
        fromIntegral o == L.length h && o == B.length h'
  where f = hashFunc d
        f' = hashFunc' d
        h = L.fromChunks [Ser.encode $ f lps]
        h' = Ser.encode . f' . B.concat . L.toChunks $ lps
        o = (outputLength `for` d) `div` 8

-- |Verify the Serilize and Binary (decode . encode = id)
prop_GetPutHash :: Hash c d => d -> L.ByteString -> Bool
prop_GetPutHash d lps = Ser.decode (Ser.encode h') == Right h'
  where
  f = hashFunc d
  f' = hashFunc' d
  h = f lps
  h' = f' . B.concat . L.toChunks $ lps

-- |verify:
--
-- > blockLength .::. d `rem` 8 == 0
prop_BlockLengthIsByteAligned :: Hash c d => d -> Bool
prop_BlockLengthIsByteAligned d = blockLength .::. d `rem` 8 == 0

-- |verify
--
-- > outputLength .::. d `rem` 8 == 0
prop_OutputLengthIsByteAligned :: Hash c d => d -> Bool
prop_OutputLengthIsByteAligned d = blockLength .::. d `rem` 8 == 0

-- |Construct a test group to check common hash properties.
-- Properties include:
--
--    * Operating on lazy bytestrings obtains the same result as on strict bytestrings.
--
--    * The length of the digest (instance definition) matches the Serialize definition.
--
--    * encode . decode == id
--
--    * Hash block length is byte aligned (the 'crypto-api' operations require this!)
--
--    * The digest (output) length is byte aligned (also needed by 'crypto-api')
makeHashPropTests :: Hash c d => d -> Test
makeHashPropTests d =
        testGroup "Cryptographic Digest Property Tests"
        [ testProperty "LazyStrictEqual" (prop_LazyStrictEqual d)
        , testProperty "DigestLen" (prop_DigestLen d)
        , testProperty "GetPutHash" (prop_GetPutHash d)
        , testProperty "BlockLengthIsByteAligned"(prop_BlockLengthIsByteAligned d)
        , testProperty "OutputLengthIsByteAligned"  (prop_OutputLengthIsByteAligned d)
        ]

-- |some generic blockcipher tests

goodKey :: BlockCipher k => k -> B.ByteString -> Bool
goodKey k bs =
        case (getKey k bs `asTypeOf` Just k) of
                Nothing -> False
                Just _  -> True

bKey k bs = let Just k' = (getKey k bs `asTypeOf` Just k) in k'

-- Pad out (or trim) material to correct length (for testing only!)
getKey :: BlockCipher k => k -> B.ByteString -> Maybe k
getKey k bs =
        let l  = (keyLength `for` k) `div` 8
            b' = B.take l (B.concat $ replicate l (B.append bs (B.singleton 0)))
        in buildKey b'

bIV :: BlockCipher k => k -> B.ByteString -> Either String (IV k)
bIV k bs = Ser.decode bs

isRight (Right _) = True
isRight (Left _)  = False

comparePadded :: BlockCipher k => k -> (k -> B.ByteString -> B.ByteString) -> (k -> B.ByteString -> B.ByteString) -> B.ByteString -> Bool
comparePadded k enc dec msg = unpadESP (dec k (enc k (padESPBlockSize k msg))) == Just msg

prop_ECBEncDecID :: BlockCipher k => k -> B.ByteString -> B.ByteString -> Property
prop_ECBEncDecID k kBS msg = goodKey k kBS ==>
        let key = bKey k kBS
        in comparePadded key ecb unEcb msg

prop_BlockMode_EncDec_ID :: BlockCipher k =>
                            (k -> IV k -> B.ByteString -> (B.ByteString, IV k)) -> -- enc mode
                            (k -> IV k -> B.ByteString -> (B.ByteString, IV k)) -> -- dec mode
                            k ->             -- The key type witness
                            B.ByteString ->  -- The key material
                            B.ByteString ->  -- The IV material
                            B.ByteString ->  -- The message
                            Property
prop_BlockMode_EncDec_ID enc dec k kBS ivBS msg = goodKey k kBS && isRight (bIV k ivBS) ==>
        let key = bKey k kBS
            Right iv = bIV k ivBS
            msg' = padESPBlockSize key msg
            (ct, iv2) = enc key iv msg'
        in dec key iv ct == (msg', iv2)

prop_CBCEncDecID = prop_BlockMode_EncDec_ID cbc unCbc
prop_CFBEncDecID = prop_BlockMode_EncDec_ID cfb unCfb
prop_OFBEncDecID = prop_BlockMode_EncDec_ID ofb unOfb
prop_CTREncDecID = prop_BlockMode_EncDec_ID ctr unCtr

-- FIXME siv tests

takeBlockSize :: BlockCipher k => k -> L.ByteString -> L.ByteString
takeBlockSize k bs = L.take (len - (len `rem` bLen)) bs
  where
  len = L.length bs
  bLen = fromIntegral $ blockSizeBytes `for` k

l2b = B.concat . L.toChunks

prop_StrictLazyEq :: BlockCipher k =>
                     (k -> IV k -> B.ByteString -> (B.ByteString,IV k)) ->
                     (k -> IV k -> L.ByteString -> (L.ByteString,IV k)) ->
                     (k -> IV k -> B.ByteString -> (B.ByteString,IV k)) ->
                     (k -> IV k -> L.ByteString -> (L.ByteString,IV k)) ->
                     k ->
                     B.ByteString ->
                     B.ByteString ->
                     L.ByteString ->
                     Property
prop_StrictLazyEq enc' enc dec' dec k kBS ivBS msg = goodKey k kBS &&
                                                     isRight (bIV k ivBS) ==>
        let key = bKey k kBS
            Right iv = bIV k ivBS
            msg' = takeBlockSize k msg
            ctStrict = enc' key iv (l2b msg')
            ctLazy   = enc  key iv msg'
            ptStrict = dec' key iv (l2b msg')
            ptLazy   = dec key iv msg'
        in ctStrict == first l2b ctLazy && ptStrict == first l2b ptLazy
                                      

prop_OFBStrictLazyEq = prop_StrictLazyEq ofb ofbLazy unOfb unOfbLazy
prop_CBCStrictLazyEq = prop_StrictLazyEq cbc cbcLazy unCbc unCbcLazy
prop_CFBStrictLazyEq = prop_StrictLazyEq cfb cfbLazy unCfb unCfbLazy
prop_CTRStrictLazyEq = prop_StrictLazyEq ctr ctrLazy unCtr unCtrLazy

prop_ECBStrictLazyEq :: BlockCipher k => k -> B.ByteString -> L.ByteString -> Property
prop_ECBStrictLazyEq k kBS msg = goodKey k kBS ==>
        let key = bKey k kBS
            msg' = takeBlockSize k msg
            ctStrict = ecb key (l2b msg')
            ctLazy   = ecbLazy  key msg'
            ptStrict = unEcb key (l2b msg')
            ptLazy   = unEcbLazy key msg'
        in ctStrict == l2b ctLazy && ptStrict == l2b ptLazy

-- | Build test groups for encrypt/decrypt identity and 
-- equality of operations on strict and lazy ByteStrings.
--
-- Admittedly, these tests conflate testing the algorithm
-- in question and testing the mode implementation in 'crypto-api',
-- but more testing isn't exactly a bad thing.
makeBlockCipherPropTests :: BlockCipher k => k -> [Test]
makeBlockCipherPropTests k =
        testGroup "Block Cipher tests (ident)"
        [ testProperty "ECBEncDecID" (prop_ECBEncDecID k)
        , testProperty "CBCEncDecID" (prop_CBCEncDecID k)
        , testProperty "CFBEncDecID" (prop_CFBEncDecID k)
        , testProperty "OFBEncDecID" (prop_OFBEncDecID k)
        , testProperty "CTREncDecID" (prop_CTREncDecID k)] :
        testGroup "Block Cipher tests (lazy/string bytestring equality)"
        [ testProperty "ECBStringLazyEq" (prop_ECBStrictLazyEq k)
        , testProperty "CBCStrictLazyEq" (prop_CBCStrictLazyEq k)
        , testProperty "CFBStrictLazyEq" (prop_CFBStrictLazyEq k)
        , testProperty "OFBStrictLazyEq" (prop_OFBStrictLazyEq k)
        , testProperty "CTRStrictLazyEq" (prop_CTRStrictLazyEq k)] :[]

-- *Known Answer Tests
toD :: Hash c d => d -> String -> d
toD d str = (fromRight . Ser.decode . hexStringToBS $ str) `asTypeOf` d
  where
  fromRight (Right x) = x

-- |Convert hex strings to bytestrings, for example:
-- 
-- > "3adf91c0" ==> B.pack [0x3a, 0xdf, 0x91, 0xc0]
--
-- Strings of odd length will cause an exception as will non-hex characters such as '0x'.
hexStringToBS :: String -> B.ByteString
hexStringToBS [] = B.empty
hexStringToBS (_:[]) = error "Not an even number of hex characters in input to hexStringToBS!"
hexStringToBS (a:b:xs) = B.cons (rHex (a:b:[])) (hexStringToBS xs)
  where
  rHex = fst . head . readHex
