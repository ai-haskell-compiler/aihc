
import TestUtils
import VectorsUMAC

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Maybe (maybe)
import Data.List (foldl')

import Crypto.Nettle.UMAC

-- test both strict and lazy updates
executeRound :: UMAC u => u -> [B.ByteString] -> (B.ByteString, u)
executeRound u [s] = umacFinalize $ umacUpdate u s
executeRound u s   = umacFinalize $ umacUpdateLazy u $ L.fromChunks s

assertUMAC :: (B.ByteString, Maybe B.ByteString, Int -> [B.ByteString], [(String, String, String)]) -> Assertion
assertUMAC (key, nonce, msg, hashes) = let
		umac32  = uinit :: UMAC32
		umac64  = uinit :: UMAC64
		umac96  = uinit :: UMAC96
		umac128 = uinit :: UMAC128
		in rounds 1 (umac32, umac64, umac96, umac128) hashes
	where
		uinit :: UMAC u => u
		uinit = maybe id (flip umacSetNonce) nonce $ umacInit key

		rounds _ _ [] = return ()
		rounds n (umac32, umac64, umac96, umac128) ((h32,h64,h128):xs) = let
			txt = "round " ++ show n
			(h32', umac32') = executeRound umac32 (msg $ 4*n + 0)
			(h64', umac64') = executeRound umac64 (msg $ 4*n + 1)
			(h96', umac96') = executeRound umac96 (msg $ 4*n + 2)
			(h128', umac128') = executeRound umac128 (msg $ 4*n + 3)
			in do
				assertEqualHex (txt ++ " UMAC32") (hs h32) h32'
				assertEqualHex (txt ++ " UMAC64") (hs h64) h64'
				assertEqualHex (txt ++ " UMAC96") (B.take 12 $ hs h128) h96'
				assertEqualHex (txt ++ " UMAC128") (hs h128) h128'
				rounds (n+1) (umac32', umac64', umac96', umac128') xs

main = defaultMain [ debugTestCases "UMAC" $ map assertUMAC umacTestVectors ]
