
module KAT.Utils
	( module Crypto.Cipher.Tests
	, concatKATs
	) where

import Crypto.Cipher.Tests

concatKATs :: [KATs] -> KATs
concatKATs l = KATs (m kat_ECB) (m kat_CBC) (m kat_CFB) (m kat_CTR) (m kat_XTS) (m kat_AEAD)
	where
	m :: (KATs -> [x]) -> [x]
	m sel = concat $ map sel l
