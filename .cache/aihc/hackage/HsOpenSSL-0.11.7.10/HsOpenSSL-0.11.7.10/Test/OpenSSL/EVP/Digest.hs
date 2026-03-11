module Main (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char
import OpenSSL
import Text.Printf
import OpenSSL.EVP.Digest
import TestUtils

main :: IO ()
main = withOpenSSL $ do
    Just md5 <- getDigestByName "MD5"
    Just sha1 <- getDigestByName "SHA1"
    Just sha256 <- getDigestByName "SHA256"
    let hex  = concatMap (printf "%02x" . ord) . B.unpack
        checkHMAC digestName key testData result = do
            assertEqual what result $
                hex $ hmacBS d (B.pack key) (B.pack testData)
            assertEqual ("lazy " ++ what) result $
                hex $ hmacLBS d (B.pack key) (BL.pack testData)
            where what =
                      "HMAC_" ++ digestName ++
                      "(" ++ show key ++ ", " ++ show testData ++ ")"
                  d = case digestName of
                      "MD5" -> md5
                      "SHA1" -> sha1
                      "SHA256" -> sha256
                      _ -> error digestName
    -- test data from
    -- https://en.wikipedia.org/wiki/Hash-based_message_authentication_code

    checkHMAC "MD5" "" ""    "74e6f7298a9c2d168935f58c001bad88"
    checkHMAC "SHA1" "" ""   "fbdb1d1b18aa6c08324b7d64b71fb76370690e1d"
    checkHMAC "SHA256" "" "" "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad"
    checkHMAC "MD5" "key" "The quick brown fox jumps over the lazy dog"    "80070713463e7749b90c2dc24911e275"
    checkHMAC "SHA1" "key" "The quick brown fox jumps over the lazy dog"   "de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9"
    checkHMAC "SHA256" "key" "The quick brown fox jumps over the lazy dog" "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"
