{-# LANGUAGE OverloadedStrings #-}
module Test.MD5
	( makeMD5Tests
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Data.Serialize (encode, decode)
import Crypto.Classes
import Test.Crypto

import Test.HUnit.Base (assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)


makeMD5Tests :: (Show d, Hash c d) => d -> [Test]
makeMD5Tests d = [makeHashPropTests d,md5KATs d]

md5KATs :: (Show d, Hash c d) => d -> Test
md5KATs d =
  let hash = hashFunc d in
  testGroup "MD5 KATs" $ map (testCase "md5KAT")
        [ assertEqual "md5KAT1" (hash "") (toD d "d41d8cd98f00b204e9800998ecf8427e")
        , assertEqual "md5KAT2" (hash "a") (toD d "0cc175b9c0f1b6a831c399e269772661")
        , assertEqual "md5KAT3" (hash "abc") (toD d "900150983cd24fb0d6963f7d28e17f72")
        , assertEqual "md5KAT4" (hash "message digest") (toD d "f96b697d7cb7938d525a2f31aaf161d0")
        , assertEqual "md5KAT5" (hash "abcdefghijklmnopqrstuvwxyz") (toD d "c3fcd3d76192e4007dfb496cca67e13b")
        , assertEqual "md5KAT6" (hash "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") (toD d "d174ab98d277d9f5a5611c2c9f419d9f")
        , assertEqual "md5KAT7" (hash "12345678901234567890123456789012345678901234567890123456789012345678901234567890") (toD d "57edf4a22be3c955ac49da2e2107b67a")
        ]

-- *Known Answer Tests
toD :: Hash c d => d -> String -> d
toD d str = (fromRight . decode . hexStringToBS $ str) `asTypeOf` d
  where
  fromRight (Right x) = x
