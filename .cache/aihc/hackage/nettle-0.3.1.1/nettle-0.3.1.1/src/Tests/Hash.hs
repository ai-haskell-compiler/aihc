{-# LANGUAGE OverloadedStrings #-}


import TestUtils
import qualified Data.ByteString as B
import VectorsHash

import Crypto.Nettle.Hash

assertHash :: HashAlgorithm a => (B.ByteString, String) -> Tagged a Assertion
assertHash (src, h) = do
	h' <- hash src
	return $ assertEqualHex "" (hs h) h'

testHash :: HashAlgorithm a => Tagged a Test
testHash = do
	name <- hashName
	vectors <- findHashTestVectors name
	results <- mapM assertHash vectors
	return $ testCases ("testing HashAlgorithm " ++ name) results
--	return $ debugTestCases ("testing HashAlgorithm " ++ name) results

main = defaultMain
	[ testHash `witness` (undefined :: GOSTHASH94)
	, testHash `witness` (undefined :: MD2)
	, testHash `witness` (undefined :: MD4)
	, testHash `witness` (undefined :: MD5)
	, testHash `witness` (undefined :: RIPEMD160)
	, testHash `witness` (undefined :: SHA1)
	, testHash `witness` (undefined :: SHA224)
	, testHash `witness` (undefined :: SHA256)
	, testHash `witness` (undefined :: SHA384)
	, testHash `witness` (undefined :: SHA512)
	, testHash `witness` (undefined :: SHA3_224)
	, testHash `witness` (undefined :: SHA3_256)
	, testHash `witness` (undefined :: SHA3_384)
	, testHash `witness` (undefined :: SHA3_512)
	]
