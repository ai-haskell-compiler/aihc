{-# LANGUAGE Safe #-}

module HexUtils
	( readHex'
	, readHex
	, toString
	, hs

	, hexchars
	, hexa
	, hexs

	, Word8
	) where

import qualified Numeric as N

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.Array.IArray as A

readHex' :: Monad m => String -> m [Word8]
readHex' [] = return []
readHex' (' ':xs) = readHex' xs
readHex' (a:' ':xs) = readHex' $ a:xs
readHex' (a:b:xs) = do
	n <- case N.readHex [a,b] of
		(n, ""):_ -> return n
		_ -> error "invalid hex encoding"
	xn <- readHex' xs
	return $ n:xn
readHex' _ = error "invalid hex encoding"

readHex :: String -> [Word8]
readHex s = let Just r = readHex' s in r

toString :: [Word8] -> String
toString = map (toEnum . fromIntegral)

-- read hex string
hs :: String -> B.ByteString
hs = B.pack . readHex

hexchars :: A.Array Word8 Char
hexchars = A.listArray (0,15) (['0'..'9'] ++ ['a'..'f'])

hexa :: [Word8] -> String
hexa [] = []
hexa (x:xs) = let (high,low) = divMod x 16 in hexchars A.! high:hexchars A.! low:hexa xs

hexs :: B.ByteString -> String
hexs = hexa . B.unpack
