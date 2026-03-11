
import TestUtils
import VectorsHMAC

import qualified Data.ByteString as B

import Crypto.Nettle.HMAC
import Crypto.Nettle.Hash


assertHMAC :: HashAlgorithm a => (B.ByteString, B.ByteString, String) -> Tagged a Assertion
assertHMAC (key, msg, h) = do
	h' <- hmac key msg
	return $ assertEqualHex "" (hs h) $ B.take (B.length $ hs h) h'

testHMAC :: HashAlgorithm a => Tagged a Test
testHMAC = do
	name <- hashName
	vectors <- findHmacTestVectors ("HMAC-" ++ name)
	results <- mapM assertHMAC vectors
	return $ testCases ("testing HMAC-" ++ name) results
--	return $ debugTestCases ("testing HMAC-" ++ name) results

main = defaultMain
	[ testHMAC `witness` (undefined :: MD5)
	, testHMAC `witness` (undefined :: RIPEMD160)
	, testHMAC `witness` (undefined :: SHA1)
	, testHMAC `witness` (undefined :: SHA224)
	, testHMAC `witness` (undefined :: SHA256)
	, testHMAC `witness` (undefined :: SHA384)
	, testHMAC `witness` (undefined :: SHA512)
	]
