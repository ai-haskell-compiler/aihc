{-# LANGUAGE OverloadedStrings, Safe #-}

module VectorsUMAC
	( umacTestVectors
	) where

import HexUtils
import qualified Data.ByteString as B

-- repeat chunks of s until filled length bytes
repString :: Int -> B.ByteString -> Int -> [B.ByteString]
repString len s n = if len > B.length s then s:let l' = len - B.length s in l' `seq` repString l' s n else [B.take len s]

-- [(key, nonce, message-chunks, [(umac32, umac64, umac128)])]
-- umac96 is truncated umac128
umacTestVectors :: [(B.ByteString, Maybe B.ByteString, Int -> [B.ByteString], [(String, String, String)])]
umacTestVectors =
--  /* From RFC 4418 (except that it lacks the last 32 bits of 128-bit tags) */
	[ ("abcdefghijklmnop", Just "bcdefghi", repString 0 "",
		[ ("113145FB", "6E155FAD26900BE1", "32fedb100c79ad58f07ff7643cc60465")
		])
	, ("abcdefghijklmnop", Just "bcdefghi", repString 3 "a",
		[ ("3B91D102", "44B5CB542F220104", "185e4fe905cba7bd85e4c2dc3d117d8d")
		])
	, ("abcdefghijklmnop", Just "bcdefghi", repString (2^(10::Int)) "a",
		[ ("599B350B", "26BF2F5D60118BD9", "7a54abe04af82d60fb298c3cbd195bcb")
		])
	, ("abcdefghijklmnop", Just "bcdefghi", repString (2^(15::Int)) "aaaaaaaa",
		[ ("58DCF532", "27F8EF643B0D118D", "7b136bd911e4b734286ef2be501f2c3c")
		])
	, ("abcdefghijklmnop", Just "bcdefghi", repString (2^(20::Int)) "aaaaaaaa",
		[ ("DB6364D1", "A4477E87E9F55853", "f8acfa3ac31cfeea047f7b115b03bef5")
		])
--  /* Needs POLY128 */
--  /* For the 'a' * 2^25 testcase, see errata http://fastcrypto.org/umac/rfc4418.errata.txt */
	, ("abcdefghijklmnop", Just "bcdefghi", repString (2^(25::Int)) "aaaaaaaa",
		[ ("85EE5CAE", "FACA46F856E9B45F", "a621c2457c0012e64f3fdae9e7e1870c")
		])
	, ("abcdefghijklmnop", Just "bcdefghi", repString 3 "abc",
		[ ("ABF3A3A0", "D4D7B9F6BD4FBFCF", "883c3d4b97a61976ffcf232308cba5a5")
		])
	, ("abcdefghijklmnop", Just "bcdefghi", repString 1500 "abc",
		[ ("ABEB3C8B", "D4CF26DDEFD5C01A", "8824a260c53c66a36c9260a62cb83aa1")
		])
	, ("abcdefghijklmnop", Nothing, const ["zero"],
		[ ("a0e94011", "a0e940111c9c2cd5", "a0e940111c9c2cd5fa59090e3ac2061f")
		, ("8c6fea51", "6d8971434be8ee41", "cbbf18b799fd0f4afb9216e52a89f247")
		, ("6d897143", "c9c9aef87e2be502", "c9c9aef87e2be50237716af8e24f8959")
		, ("db1b28c5", "a0a112b593656107", "d6e96ef461f54d1c85aa66cbd76ca336")
		, ("a75e23b7", "a75e23b7d419e03a", "a75e23b7d419e03a02d55ebf1ba62824")
		, ("44ea26be", "950526f26a8cc07a", "2e63031d182a59b84f148d9a91de70a3")
		])
	, ("abcdefghijklmnop", Just "a", const ["nonce-a"],
		[ ("81b4ac24", "b7e8aad0da6e7f99", "d7604bffb5e368da5fe564da0068d2cc")
		, ("b7e8aad0", "138814c6a03bdadf", "138814c6a03bdadff7f1666e1bd881aa")
		, ("f70246fe", "fb77dd1cd4c7074f", "86a016d9e67957c8ab5ebb78a673e4e9")
		, ("0595f0bf", "0595f0bf8585c7e2", "0595f0bf8585c7e28dfab00598d4e612")
		, ("a8e9fe85", "817c0b7757cb60f7", "3266ec16a9d85b4f0dc74ec8272238a9")
		])
	, ("abcdefghijklmnop", Just $ hs "beafcafe", const ["nonce-beaf-cafe"],
		[ ("f19d9dc1", "9e878413aa079032", "9e878413aa0790329604f3b6ae980e58")
		, ("4604a56a", "9cfd7af0bb107748", "f2b2dd5dab08bb3bc5e9a83e1b4ab2e7")
		, ("4ba9420e", "4ba9420e55b6ba13", "4ba9420e55b6ba137d03443f6ee01734")
		, ("da86ff71", "77facd797b686e24", "2721ca2e1bcda53a54ae65e0da139c0d")
		, ("77facd79", "9000c0de4f5f7236", "9000c0de4f5f7236b81ae1a52e78a821")
		])
--  /* Tests exercising various sizes of nonce and data: All nonce
--     lengths from 1 to 16 bytes. Data sizes chosen for testing for
--     various off-by-one errors,
--       0, 1, 2, 3, 4,
--       1020, 1021, 1022, 1023, 1024, 1025, 1026, 1027,
--       2046, 2047, 2048, 2049, 2050
--       16777212, 16777213, 16777214, 16777215, 16777216, 16777217,
--       16778239, 16778240, 16778241, 16778242, 16778243, 16778244
--  */
	, ("abcdefghijklmnop", Just "b", repString 0 "defdefdefdefdef",
		[ ("3a58486b", "9e38f67da91a08d9", "9e38f67da91a08d9c980f4db4089c877")
		])
	, ("abcdefghijklmnop", Just "bc", repString 1 "defdefdefdefdef",
		[ ("d86b1512", "fb0e207971b8e66a", "ef406c2ec70d0222f59e860eabb79ed0")
		])
	, ("abcdefghijklmnop", Just "bcd", repString 2 "defdefdefdefdef",
		[ ("1ae6e02d", "1ae6e02d73aa9ab2", "1ae6e02d73aa9ab2a27fb89e014dc07b")
		])
	, ("abcdefghijklmnop", Just "bcde", repString 3 "defdefdefdefdef",
		[ ("e8c1eb59", "c81cf22342e84302", "82626d0d575e01038e5e2cc6408216f5")
		])
	, ("abcdefghijklmnop", Just "bcdef", repString 4 "defdefdefdefdef",
		[ ("8950f0d3", "aba003e7bd673cc3", "aba003e7bd673cc368ba8513cecf2e7c")
		])
	, ("abcdefghijklmnop", Just "bcdefg", repString 1020 "defdefdefdefdef",
		[ ("7412167c", "f98828a161bb4ae3", "d8b4811f747d588d7a913360960de7cf")
		])
	, ("abcdefghijklmnop", Just "bcdefgh", repString 1021 "defdefdefdefdef",
		[ ("2d54936b", "2d54936be5bff72d", "2d54936be5bff72d2e1052361163b474")
		])
	, ("abcdefghijklmnop", Just "bcdefghi", repString 1022 "defdefdefdefdef",
		[ ("53ca8dd2", "2cee9784556387b3", "700513397f8a210a98938d3e7ac3bd88")
		])
	, ("abcdefghijklmnop", Just "bcdefghij", repString 1023 "defdefdefdefdef",
		[ ("26cc58df", "24ac4284ca371f42", "24ac4284ca371f4280f60bd274633d67")
		])
	, ("abcdefghijklmnop", Just "bcdefghijk", repString 1024 "defdefdefdefdef",
		[ ("3cada45a", "64c6a0fd14615a76", "abc223116cedd2db5af365e641a97539")
		])
	, ("abcdefghijklmnop", Just "bcdefghijkl", repString 1025 "defdefdefdefdef",
		[ ("93251e18", "93251e18e56bbdc4", "93251e18e56bbdc457de556f95c59931")
		])
	, ("abcdefghijklmnop", Just "bcdefghijklm", repString 1026 "defdefdefdefdef",
		[ ("24a4c3ab", "5d98bd8dfaf16352", "c1298672e52386753383a15ed58c0e42")
		])
	, ("abcdefghijklmnop", Just "bcdefghijklmn", repString 1027 "defdefdefdefdef",
		[ ("e7e98945", "5b0557c9fdcf661b", "5b0557c9fdcf661b1758efc603516ebe")
		])
	, ("abcdefghijklmnop", Just "bcdefghijklmno", repString 2046 "defdefdefdefdef",
		[ ("e12ddc9f", "65e85d47447c2277", "16bb5183017826ed47c9995c1e5834f3")
		])
	, ("abcdefghijklmnop", Just "bcdefghijklmnop", repString 2047 "defdefdefdefdef",
		[ ("34d723a6", "34d723a6cb1676d3", "34d723a6cb1676d3547a5064dc5b0a37")
		])
	, ("abcdefghijklmnop", Just "bcdefghijklmnopq", repString 2048 "defdefdefdefdef",
		[ ("21fd8802", "3968d5d0af147884", "84565620def1e3a614d274e87626f215")
		])
	, ("abcdefghijklmnop", Just "b", repString 2049 "defdefdefdefdef",
		[ ("097e5abd", "ad1ee4ab606061c5", "ad1ee4ab606061c55e0d2ecfee59940a")
		])
	, ("abcdefghijklmnop", Just "bc", repString 2050 "defdefdefdefdef",
		[ ("a03a7fe9", "835f4a8242100055", "971106d5f4a5e41dce40a91704cfe1f3")
		])
	, ("abcdefghijklmnop", Just "bcd", repString 16777212 "defdefdefdefdef",
		[ ("7ef41cf3", "7ef41cf351960aaf", "7ef41cf351960aaf729bb19fcee7d8c4")
		])
	, ("abcdefghijklmnop", Just "bcde", repString 16777213 "defdefdefdefdef",
		[ ("8bf81932", "ab250048807ff640", "e15b9f6695c9b441de035e9b10b8ac32")
		])
	, ("abcdefghijklmnop", Just "bcdef", repString 16777214 "defdefdefdefdef",
		[ ("ddb2f0ab", "ff42039fcfe1248e", "ff42039fcfe1248e36c19efed14d7140")
		])
	, ("abcdefghijklmnop", Just "bcdefg", repString 16777215 "defdefdefdefdef",
		[ ("e67ad507", "6be0ebda623d76df", "4adc426477fb64b1ce5afd76d505f048")
		])
	, ("abcdefghijklmnop", Just "bcdefgh", repString 16777216 "defdefdefdefdef",
		[ ("42d8562a", "42d8562a224a9e9a", "42d8562a224a9e9a75c2f85d39462d07")
		])
	, ("abcdefghijklmnop", Just "bcdefghi", repString 16777217 "defdefdefdefdef",
		[ ("486b138d", "374f09dbb0b84b88", "6ba48d669a51ed3195ebc2aa562ee71b")
		])
	, ("abcdefghijklmnop", Just "bcdefghij", repString 16778239 "defdefdefdefdef",
		[ ("850cb2c5", "876ca89ed045777b", "876ca89ed045777bf7efa7934e1758c2")
		])
	, ("abcdefghijklmnop", Just "bcdefghijk", repString 16778240 "defdefdefdefdef",
		[ ("b9fc4f81", "e1974b26fb35f2c6", "2e93c8ca83b97a6b1a21082e2a4c540d")
		])
	, ("abcdefghijklmnop", Just "bcdefghijkl", repString 16778241 "defdefdefdefdef",
		[ ("ffced8f2", "ffced8f2494d85bf", "ffced8f2494d85bf0cb39408ddfe0295")
		])
	, ("abcdefghijklmnop", Just "bcdefghijklm", repString 16778242 "defdefdefdefdef",
		[ ("1c99c5fb", "65a5bbdda3b85368", "f9148022bc6ab64f019e9db83704c17b")
		])
	, ("abcdefghijklmnop", Just "bcdefghijklmn", repString 16778243 "defdefdefdefdef",
		[ ("ec304be9", "50dc9565fbfc4884",  " 50dc9565fbfc48844a4be34403804605")
		])
	, ("abcdefghijklmnop", Just "bcdefghijklmno", repString 16778244 "defdefdefdefdef",
		[ ("8034e26f", "04f163b7c2d5d849", "77a26f7387d1dcd39378a3220652cff7")
		])
--  /* Test varying the alignment of the buffer eventually passed to _umac_nh and _umac_nh_n. */
--	, ("abcdefghijklmnop", Just "bcdefghijk", repString 1024 "defdefdefdefdef",
--		[ ("3cada45a", "64c6a0fd14615a76", "abc223116cedd2db5af365e641a97539")
--		])
	]
