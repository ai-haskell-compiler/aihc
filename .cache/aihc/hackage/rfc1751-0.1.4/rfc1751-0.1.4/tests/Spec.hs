import           Control.Monad
import           Crypto.RFC1751
import           Data.Bits
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Serialize
import           Data.Word
import           Numeric
import           Test.Hspec
import           Test.Hspec.QuickCheck

type TestVector = (ByteString, String)

main :: IO ()
main =
    hspec $ do
        describe "codec unit tests" $ do
            it "encodes keys" (forM_ vectors encodeTest)
            it "decodes keys" (forM_ vectors decodeTest)
            it "encodes lowercase keys" (forM_ vectorsLC encodeTest)
            it "encodes lowercase keys" (forM_ vectorsLC decodeTest)
            it "decodes bad checksums" (forM_ badChecksumVectors badChecksum)
            it
                "decodes bad lowercase checksums"
                (forM_ badChecksumVectorsLC badChecksum)
            it "decodes not in dictionary" (forM_ notDictVectors notDict)
            it
                "decodes lowercase not in dictionary"
                (forM_ notDictVectorsLC notDict)
        describe "codec property tests" $ do
            prop "encode/decode keys" decodeEncode
            prop "encode/decode lowercase keys" decodeEncodeLC
            prop "encode/decode double 64-bit keys" doubleWord64

badChecksumVectors :: [String]
badChecksumVectors =
    [ "RASH BUSH MILK LOOK BAD A AVID GAFF BAIT ROT POD LOVE"
    , "TAB BORE DUNK SURE COVE NORM PRY IF JOE MYRA GWEN TENT"
    , "BORN ROLL LOVE BEAR AGEE IFFY CUTS MASK MOOD FOWL ROME MIT"
    ]

badChecksumVectorsLC :: [String]
badChecksumVectorsLC =
    [ "rash bush milk look bad a avid gaff bait rot pod love"
    , "tab bore dunk sure cove norm pry if joe myra gwen tent"
    , "born roll love bear agee iffy cuts mask mood fowl rome mit"
    ]

badChecksum :: String -> Expectation
badChecksum s = mnemonicToKey s `shouldSatisfy` isNothing

notDictVectors :: [String]
notDictVectors =
    [ "PHON MEMO HOP NELL RET DEAF HURT YAWN FLAG MILE LEO LESK"
    , "JAG SUCH PER HASH FULL PHON DAN THEY CAIN BOND LEFT COCA"
    , "TENT TIER LIEU ROD URGE BOWL PATK HOOK FLEW ELY MAN OAK"
    , "TIE OLDY FEEL DOCK EWE PA EMIT HAVE HIS TOTE SWAN KTUH"
    ]

notDictVectorsLC :: [String]
notDictVectorsLC =
    [ "phon memo hop nell ret deaf hurt yawn flag mile leo lesk"
    , "jag such per hash full phon dan they cain bond left coca"
    , "tent tier lieu rod urge bowl patk hook flew ely man oak"
    , "tie oldy feel dock ewe pa emit have his tote swan ktuh"
    ]

notDict :: String -> Expectation
notDict s = mnemonicToKey s `shouldSatisfy` isNothing

integerToBS :: Integer -> ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0     = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just (fromInteger x :: Word8, x `shiftR` 8)

hexToBS :: String -> ByteString
hexToBS str
    | null str  = BS.empty
    | otherwise = BS.append z2 r2
  where
    (z,r) = span (== '0') $ filter (/= ' ') str
    z2    = BS.replicate (fromIntegral $ length z `div` 2) 0
    r1    = readHex r
    r2 | null r    = BS.empty
       | null r1   = error "cannot read hex"
       | otherwise = integerToBS $ fst $ head r1

vectors :: [TestVector]
vectors =
  [ ( hexToBS "CCAC 2AED 5910 56BE 4F90 FD44 1C53 4766"
    , "RASH BUSH MILK LOOK BAD BRIM AVID GAFF BAIT ROT POD LOVE"
    )
  , ( hexToBS "EFF8 1F9B FBC6 5350 920C DD74 16DE 8009"
    , "TROD MUTE TAIL WARM CHAR KONG HAAG CITY BORE O TEAL AWL"
    )
  ]

vectorsLC :: [TestVector]
vectorsLC =
  [ ( hexToBS "ccac 2aed 5910 56be 4f90 fd44 1c53 4766"
    , "rash bush milk look bad brim avid gaff bait rot pod love"
    )
  , ( hexToBS "eff8 1f9b fbc6 5350 920c dd74 16de 8009"
    , "trod mute tail warm char kong haag city bore o teal awl"
    )
  ]

encodeTest :: TestVector -> Expectation
encodeTest (bs, m) = mnemonicToKey m `shouldBe` Just bs

decodeTest :: TestVector -> Expectation
decodeTest (bs, hk) =
    map toLower <$> keyToMnemonic bs `shouldBe` Just (map toLower hk)

decodeEncode :: (Word64, Word64) -> Bool
decodeEncode (w1, w2) = (mnemonicToKey =<< keyToMnemonic bs) == Just bs
  where
    bs = encode w1 `BS.append` encode w2

decodeEncodeLC :: (Word64, Word64) -> Bool
decodeEncodeLC (w1, w2) =
    (mnemonicToKey =<< map toLower <$> keyToMnemonic bs) == Just bs
  where
    bs = encode w1 `BS.append` encode w2


doubleWord64 :: Word64 -> Bool
doubleWord64 w =
    maybe False (uncurry (==) . splitAt 6 . words) (keyToMnemonic bs)
  where
    bs = encode w `BS.append` encode w
