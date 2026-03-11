{-# Language OverloadedStrings #-}
import Conduit
import qualified Crypto.Cipher.ChaChaPoly1305.Conduit as ChaCha
import qualified Crypto.ECC as ECC
import Crypto.Hash
import Crypto.MAC.HMAC
import Crypto.MAC.HMAC.Conduit
import qualified Crypto.PubKey.ECIES.Conduit as PubKey
import Crypto.Random
import Data.ByteArray.Encoding
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Crypton conduit tests"
    [ testGroup "HMAC"
        [ testCase "File HMAC is correct" testFileHMAC
        ]
    , testGroup "ChaChaPoly1305"
        [ testProperty "encrypt/decrypt works" (ioProperty . propChaChaPoly1305)
        ]
    , testGroup "publicECC"
        [ testProperty "encrypt/decrypt works" (ioProperty . propPublicECC)
        ]
    ]

testFileHMAC :: Assertion
testFileHMAC = do
    let source = BL.take (1024 * 1024 * 3 + 150) $ BL.iterate (+ 1) 0
    testhmac <- runConduit $ sourceLazy source .| sinkHMAC ("foobar" :: BS.ByteString)
    let hexdump = convertToBase Base16 (testhmac :: HMAC SHA512t_256)
    assertEqual "HMAC mismatch" "ab78ef7a3a7b02b2ef50ee1a17e43ae0c134e0bece468b047780626264301831" (hexdump :: BS.ByteString)

propChaChaPoly1305 :: [[Word8]] -> IO Bool
propChaChaPoly1305 octets = do
    let chunksIn = map BS.pack octets
    nonce <- getRandomBytes 12
    key <- getRandomBytes 32
    chunksOut <- runConduit
       $ mapM_ yield chunksIn
      .| ChaCha.encrypt nonce key
      .| ChaCha.decrypt key
      .| sinkLazy
    return $ BL.fromChunks chunksIn == chunksOut

propPublicECC :: [[Word8]] -> IO Bool
propPublicECC octets = do
    let chunksIn = map BS.pack octets
    ECC.KeyPair point scalar <- ECC.curveGenerateKeyPair (Proxy :: Proxy ECC.Curve_P256R1)
    chunksOut <- runConduit
       $ mapM_ yield chunksIn
      .| PubKey.encrypt point
      .| PubKey.decrypt scalar
      .| sinkLazy
    return $ BL.fromChunks chunksIn == chunksOut
