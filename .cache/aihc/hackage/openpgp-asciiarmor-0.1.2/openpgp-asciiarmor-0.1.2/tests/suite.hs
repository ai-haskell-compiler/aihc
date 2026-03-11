import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

import Codec.Encryption.OpenPGP.ASCIIArmor (decode, decodeLazy, encode, encodeLazy, multipartMerge)
import Codec.Encryption.OpenPGP.ASCIIArmor.Types
import Codec.Encryption.OpenPGP.ASCIIArmor.Utils

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.Digest.CRC24 (crc24)
import Data.Word (Word32)

testCRC24 :: ByteString -> Word32 -> Assertion
testCRC24 bs crc = assertEqual "crc24" crc (crc24 bs)

testArmorDecode :: FilePath -> [FilePath] -> Assertion
testArmorDecode fp targets = do
    bs <- BL.readFile $ "tests/data/" ++ fp
    tbss <- mapM (\target -> BL.readFile $ "tests/data/" ++ target) targets
    case decodeLazy bs of
        Left e -> assertFailure $ "Decode failed (" ++ e ++ ") on " ++ fp
        Right as -> assertEqual ("for " ++ fp) tbss (map getPayload as)
    where
        getPayload (Armor _ _ pl) = pl
        getPayload _ = error "This should not happen."

testArmorMultipartDecode :: FilePath -> FilePath -> Assertion
testArmorMultipartDecode fp target = do
    bs <- BL.readFile $ "tests/data/" ++ fp
    tbs <- BL.readFile $ "tests/data/" ++ target
    case decodeLazy bs of
        Left e -> assertFailure $ "Decode failed (" ++ e ++ ") on " ++ fp
        Right as -> assertEqual ("for " ++ fp) tbs (getPayload (multipartMerge as))
    where
        getPayload (Armor _ _ pl) = pl
        getPayload _ = error "This should not happen."

testClearsignedDecodeBody :: FilePath -> FilePath -> Assertion
testClearsignedDecodeBody fp target = do
    bs <- BL.readFile $ "tests/data/" ++ fp
    tbs <- BL.readFile $ "tests/data/" ++ target
    case decodeLazy bs of
        Left e -> assertFailure $ "Decode failed (" ++ e ++ ") on " ++ fp
        Right [a] -> assertEqual ("for " ++ fp) (convertEndings tbs) (getBody a)
        _ -> assertFailure "This shouldn't happen."
    where
        getBody (ClearSigned _ txt _) = txt
        getBody _ = error "This should not happen."
        convertEndings = crlfUnlinesLazy . BLC8.lines

testClearsignedDecodeSig :: FilePath -> FilePath -> Assertion
testClearsignedDecodeSig fp target = do
    bs <- BL.readFile $ "tests/data/" ++ fp
    tbs <- BL.readFile $ "tests/data/" ++ target
    case decodeLazy bs of
        Left e -> assertFailure $ "Decode failed (" ++ e ++ ") on " ++ fp
        Right [a] -> assertEqual ("for " ++ fp) tbs (getSig a)
        _ -> assertFailure "This shouldn't happen."
    where
        getSig (ClearSigned _ _ (Armor _ _ sig)) = sig
        getSig _ = error "This should not happen."

testArmorEncode :: [FilePath] -> FilePath -> Assertion
testArmorEncode fps target = do
    bss <- mapM (\fp -> BL.readFile $ "tests/data/" ++ fp) fps
    tbs <- BL.readFile $ "tests/data/" ++ target
    assertEqual "literaldata" tbs (encodeLazy (map (Armor ArmorMessage [("Version","OpenPrivacy 0.99")]) bss))

testClearsignedEncode :: FilePath -> FilePath -> FilePath -> Assertion
testClearsignedEncode ftxt fsig ftarget = do
    txt <- BL.readFile $ "tests/data/" ++ ftxt
    sig <- BL.readFile $ "tests/data/" ++ fsig
    target <- BL.readFile $ "tests/data/" ++ ftarget
    assertEqual "clearsigned encode" target (encodeLazy [ClearSigned [("Hash","SHA1")] txt (Armor ArmorSignature [("Version","OpenPrivacy 0.99")] sig)])

testStrictDecode :: FilePath -> Assertion
testStrictDecode fp = do
    bs <- BL.readFile $ "tests/data/" ++ fp
    assertEqual "strict decode" (decodeLazy bs :: Either String [Armor]) (decode (B.concat . BL.toChunks $ bs) :: Either String [Armor])

testStrictEncode :: FilePath -> Assertion
testStrictEncode fp = do
    bs <- BL.readFile $ "tests/data/" ++ fp
    let fakearmors = [Armor ArmorMessage [("Version","OpenPrivacy 0.99")] bs, ClearSigned [("Hash","SHA1")] bs (Armor ArmorSignature [("Version","OpenPrivacy 0.99")] bs)]
    assertEqual "strict encode" (encodeLazy fakearmors) (BL.fromChunks [encode fakearmors])

tests :: TestTree
tests = testGroup "openpgp-asciiarmor" [
   testGroup "CRC24" [
      testCase "CRC24: A" (testCRC24 (BC8.pack "A") 16680698)
    , testCase "CRC24: Haskell" (testCRC24 (BC8.pack "Haskell") 15612750)
    , testCase "CRC24: hOpenPGP and friends" (testCRC24 (BC8.pack "hOpenPGP and friends") 11940960)
    ]
 , testGroup "ASCII armor" [
      testCase "Decode sample armor" (testArmorDecode "msg1.asc" ["msg1.gpg"])
    , testCase "Decode sample armor with cruft" (testArmorDecode "msg1a.asc" ["msg1.gpg"])
    , testCase "Decode multiple sample armors" (testArmorDecode "msg1b.asc" ["msg1.gpg","msg1.gpg","msg1.gpg"])
    , testCase "Decode detached signature" (testArmorDecode "msg4.asc" ["msg4.sig"])
    , testCase "Decode multi-part armor" (testArmorMultipartDecode "msg2.asc" "msg2.pgp")
    , testCase "Decode body of clear-signed" (testClearsignedDecodeBody "msg3.asc" "msg3")
    , testCase "Decode sig of clear-signed" (testClearsignedDecodeSig "msg3.asc" "msg3.sig")
    , testCase "Encode sample armor" (testArmorEncode ["msg1.gpg"] "msg1.asc")
    , testCase "Encode multiple sample armors" (testArmorEncode ["msg1.gpg","msg1.gpg","msg1.gpg"] "msg1c.asc")
    , testCase "Encode clear-signed sig" (testClearsignedEncode "msg3" "msg3.sig" "msg3.asc")
    , testCase "Decode from strict ByteString" (testStrictDecode "msg1.asc")
    , testCase "Encode to strict ByteString" (testStrictEncode "msg1.gpg")
    ]
 ]

main :: IO ()
main = defaultMain tests
