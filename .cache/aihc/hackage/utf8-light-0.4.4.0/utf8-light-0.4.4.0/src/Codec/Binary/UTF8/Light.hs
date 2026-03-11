{-# LANGUAGE CPP, MagicHash, TypeSynonymInstances, FlexibleInstances #-}

{- |
  Module      :  Codec.Binary.UTF8.Light
  Copyright   :  (c) Matt Morrow 2008, Francesco Ariis 2022
  License     :  BSD3
  Maintainer  :  Francesco Ariis <fa-ml@ariis.it>
  Stability   :  provisional
  Portability :  portable

  Lightweight UTF8 handling.
-}

module Codec.Binary.UTF8.Light (
    UTF8(..)
  , lenUTF8
  , lenUTF16
  , countUTF8
  , decodeUTF8
  , encodeUTF8
  , encodeUTF8'
  , withUTF8
  , putUTF8
  , putUTF8Ln
  , hPutUTF8
  , hPutUTF8Ln
  , readUTF8File
  , writeUTF8File
  , appendUTF8File
  , hGetUTF8Line
  , hGetUTF8Contents
  , hGetUTF8
  , hGetUTF8NonBlocking
  , flipUTF8
  , unflipUTF8
  , flipTab
  , unflipTab
  , showHex
  , toBits
  , fromBits
  , Int8,Int16,Int32
  , Word,Word8,Word16,Word32
  , c2w, w2c
) where

import Data.Bits
import Data.List(foldl')
import Data.Char(chr,ord)
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as B
import Data.ByteString.Unsafe
import System.IO(Handle)
import Codec.Binary.UTF8.Light.Helper (c2w, w2c, cwrd, wh, toW8)

#if defined(__GLASGOW_HASKELL__)
import GHC.Exts
  (Int(I#))
import GHC.Int (Int8, Int16, Int32)
import GHC.Word
  (Word8, Word16, Word32(W32#))
import GHC.Prim
  (int2Word# ,and#, or#, ltWord#, uncheckedShiftRL#)
#else
import Data.Word
  (Word,Word8,Word16,Word32)
import Data.Int(Int32)
#endif

-- | For convenience
fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

-- | Instances:
--    @ByteString@, @String@
--    , @[Word32]@, @[Word]@
--    , @[Int32]@, @[Int]@
class UTF8 a where
  encode :: a -> ByteString
  decode :: ByteString -> a

instance UTF8 ByteString where
  encode = id
  decode = id

instance UTF8 [Word32] where
  encode = encodeUTF8
  decode = decodeUTF8

instance UTF8 [Word] where
  encode = encodeUTF8 . fmap fi
  decode = fmap fi . decodeUTF8

instance UTF8 [Int32] where
  encode = encodeUTF8 . fmap fi
  decode = fmap fi . decodeUTF8

instance UTF8 [Int] where
  encode = encodeUTF8 . fmap fi
  decode = fmap fi . decodeUTF8

instance UTF8 String where
  encode = encode . fmap ord
  decode = fmap chr . decode

withUTF8 :: (UTF8 a) => a -> (ByteString -> b) -> b
withUTF8 a k = k (encode a)

putUTF8 :: (UTF8 a) => a -> IO ()
putUTF8 = flip withUTF8 B.putStr

putUTF8Ln :: (UTF8 a) => a -> IO ()
putUTF8Ln = flip withUTF8 B8.putStrLn

hPutUTF8 :: (UTF8 a) => Handle -> a -> IO ()
hPutUTF8 h = flip withUTF8 (B.hPut h)

hPutUTF8Ln :: (UTF8 a) => Handle -> a -> IO ()
hPutUTF8Ln h = flip withUTF8 (B8.hPutStrLn h)

readUTF8File :: (UTF8 a) => FilePath -> IO a
readUTF8File = (return . decode =<<) . B.readFile

writeUTF8File :: (UTF8 a) => FilePath -> a -> IO ()
writeUTF8File p = B.writeFile p . encode

appendUTF8File :: (UTF8 a) => FilePath -> a -> IO ()
appendUTF8File p = B.appendFile p . encode

hGetUTF8Line :: (UTF8 a) => Handle -> IO a
hGetUTF8Line = (return . decode =<<) . B.hGetLine

hGetUTF8Contents :: (UTF8 a) => Handle -> IO a
hGetUTF8Contents = (return . decode =<<) . B.hGetContents

-- | Be careful that you're sure you're not
--  chopping a UTF8 char in two!
hGetUTF8 :: (UTF8 a) => Handle -> Int -> IO a
hGetUTF8 h = (return . decode =<<) . B.hGet h

-- | Same warning as for @hGetUTF8@
hGetUTF8NonBlocking :: (UTF8 a) => Handle -> Int -> IO a
hGetUTF8NonBlocking h = (return . decode =<<) . B.hGetNonBlocking h

-- | Length in Word8s
lenUTF8 :: Word8 -> Int
{-# INLINE lenUTF8 #-}
lenUTF8 w8
  | w8 < 0x80 = 1
  | w8 < 0xe0 = 2
  | w8 < 0xf0 = 3
  | w8 < 0xf8 = 4
  | otherwise = 0

-- | Length in Word16s
lenUTF16 :: Word16 -> Int
lenUTF16 w16
-- I'm sure this could be
-- made more efficient
  | w16`shiftR`10==0x36 = 2
  | w16`shiftR`10==0x37 = 0
  | otherwise           = 1

-- | Lengths in Word8s
countUTF8 :: ByteString -> [Int]
countUTF8 bs = go 0 (B.length bs) bs
  where go :: Int -> Int -> ByteString -> [Int]
        go i len s | len <= i = []
          | otherwise = case lenUTF8 (unsafeIndex s i)
                          of  0 -> []
                              n -> n : go (i+n) len s

encodeUTF8 :: [Word32] -> ByteString
encodeUTF8 = B.pack . concat . encodeUTF8'

#if !defined(__GLASGOW_HASKELL__)

-- | Word32s not representing
--  valid UTF8 chars are /dropped/.
encodeUTF8' :: [Word32] -> [[Word8]]
encodeUTF8' [] = []
encodeUTF8' (x:xs)
  | x < 0x80 =
      [fi x] : encodeUTF8' xs
  | x < 0x800 =
      [ fi(x`shiftR`6.|.0xc0)
      , fi(x.&.0x3f.|.0x80)
      ] : encodeUTF8' xs
  | x < 0xf0000 =
      [ fi(x`shiftR`12.|.0xe0)
      , fi(x`shiftR`6.&.0x3f.|.0x80)
      , fi(x.&.0x3f.|.0x80)
      ] : encodeUTF8' xs
  | x < 0xe00000 =
      [ fi(x`shiftR`18.|.0xf0)
      , fi(x`shiftR`12.&.0x3f.|.0x80)
      , fi(x`shiftR`6.&.0x3f.|.0x80)
      , fi(x.&.0x3f.|.0x80)
      ] : encodeUTF8' xs
  | otherwise = [] : encodeUTF8' xs

decodeUTF8 :: ByteString -> [Word32]
decodeUTF8 s = go 0 (B.length s) s
  where go :: Int -> Int -> ByteString -> [Word32]
        go i len s | len <= i  = []
          | otherwise = let c1 = unsafeIndex s i
                        in case lenUTF8 c1 of
                            0 -> []
                            1 -> fi c1 : go (i+1) len s
                            2 -> if len <= i+1 then [] else
                                  let c2 = unsafeIndex s (i+1)
                                  in fi(c1.&.0x1f)`shiftL`6
                                        `xor`fi(c2.&.0x3f)
                                          : go (i+2) len s
                            3 -> if len <= i+2 then [] else
                                  let c2 = unsafeIndex s (i+1)
                                      c3 = unsafeIndex s (i+2)
                                  in fi(c1.&.0x1f)`shiftL`12
                                      `xor`fi(c2.&.0x3f)`shiftL`6
                                        `xor`fi(c3.&.0x3f)
                                          : go (i+3) len s
                            4 -> if len <= i+3 then [] else
                                  let c2 = unsafeIndex s (i+1)
                                      c3 = unsafeIndex s (i+2)
                                      c4 = unsafeIndex s (i+3)
                                  in fi(c1.&.0x1f)`shiftL`18
                                      `xor`fi(c2.&.0x3f)`shiftL`12
                                        `xor`fi(c3.&.0x3f)`shiftL`6
                                          `xor`fi(c4.&.0x3f)
                                            : go (i+4) len s

#else


-- | Word32s not representing
--  valid UTF8 chars are /dropped/.
encodeUTF8' :: [Word32] -> [[Word8]]
encodeUTF8' [] = []
-- with ghc-6.10, we
-- can use Word# literalls
-- ==> 0xff00ff00##
encodeUTF8' ((W32# w):xs)
#if MIN_VERSION_base(4,7,0)
  | I# (ww`ltWord#`(int2Word# 0x80#)) /= 0 =
#else
  | w`ltWord#`(int2Word# 0x80#) =
#endif
      [wh w] : encodeUTF8' xs

#if MIN_VERSION_base(4,7,0)
  | I# (ww`ltWord#`(int2Word# 0x800#)) /= 0 =
#else
  | w`ltWord#`(int2Word# 0x800#) =
#endif
      [ toW8(ww`uncheckedShiftRL#`6#
              `or#`(int2Word# 0xc0#))
      , toW8(ww`and#`(int2Word# 0x3f#)
              `or#`(int2Word# 0x80#))
      ] : encodeUTF8' xs
#if MIN_VERSION_base(4,7,0)
  | I# (ww`ltWord#`(int2Word# 0xf0000#)) /= 0 =
#else
  | w`ltWord#`(int2Word# 0xf0000#) =
#endif
      [ toW8(ww`uncheckedShiftRL#`12#
              `or#`(int2Word# 0xe0#))
      , toW8(ww`uncheckedShiftRL#`6#
              `and#`(int2Word# 0x3f#)
                `or#`(int2Word# 0x80#))
      , toW8(ww`and#`(int2Word# 0x3f#)
              `or#`(int2Word# 0x80#))
      ] : encodeUTF8' xs
#if MIN_VERSION_base(4,7,0)
  | I# (ww`ltWord#`(int2Word# 0xe00000#)) /= 0 =
#else
  | w`ltWord#`(int2Word# 0xe00000#) =
#endif
      [ toW8(ww`uncheckedShiftRL#`18#
              `or#`(int2Word# 0xf0#))
      , toW8(ww`uncheckedShiftRL#`12#
              `and#`(int2Word# 0x3f#)
                `or#`(int2Word# 0x80#))
      , toW8(ww`uncheckedShiftRL#`6#
              `and#`(int2Word# 0x3f#)
                `or#`(int2Word# 0x80#))
      , toW8(ww`and#`(int2Word# 0x3f#)
              `or#`(int2Word# 0x80#))
      ] : encodeUTF8' xs
  | otherwise = [] : encodeUTF8' xs

    where
          ww = cwrd w

-- TODO: ghc-ify decodeUTF8
decodeUTF8 :: ByteString -> [Word32]
decodeUTF8 bs = go 0 (B.length bs) bs
  where go :: Int -> Int -> ByteString -> [Word32]
        go i len s | len <= i  = []
          | otherwise = let c1 = unsafeIndex s i
                        in case lenUTF8 c1 of
                            0 -> []
                            1 -> fi c1 : go (i+1) len s
                            2 -> if len <= i+1 then [] else
                                  let c2 = unsafeIndex s (i+1)
                                  in fi(c1.&.0x1f)`shiftL`6
                                        `xor`fi(c2.&.0x3f)
                                          : go (i+2) len s
                            3 -> if len <= i+2 then [] else
                                  let c2 = unsafeIndex s (i+1)
                                      c3 = unsafeIndex s (i+2)
                                  in fi(c1.&.0x1f)`shiftL`12
                                      `xor`fi(c2.&.0x3f)`shiftL`6
                                        `xor`fi(c3.&.0x3f)
                                          : go (i+3) len s
                            4 -> if len <= i+3 then [] else
                                  let c2 = unsafeIndex s (i+1)
                                      c3 = unsafeIndex s (i+2)
                                      c4 = unsafeIndex s (i+3)
                                  in fi(c1.&.0x1f)`shiftL`18
                                      `xor`fi(c2.&.0x3f)`shiftL`12
                                        `xor`fi(c3.&.0x3f)`shiftL`6
                                          `xor`fi(c4.&.0x3f)
                                            : go (i+4) len s
                            _ -> error "decodeUTF8: len > 4"

#endif


-----------------------------------------------------------------------------

-- misc debug stuff

toBits :: Word8 -> [Word8]
toBits w8 = fmap ((.&.0x01) . (w8`shiftR`)) [7,6,5,4,3,2,1,0]

fromBits :: [Word8] -> Word8
fromBits = foldl' (\a (n,b) -> a.|.b`shiftL`n) 0
            . reverse . zip [0..7] . reverse

hexTab :: ByteString
hexTab = B.pack . fmap B.c2w $
  "0123456789abcdef"

showHex :: Int -> String
showHex i = ("0x"++)
  . flip fmap [28,24,20,16,12,8,4,0] $ \n ->
    B.w2c (unsafeIndex hexTab (i`shiftR`n.&.0xf))

-----------------------------------------------------------------------------

-- now, for fun...

{- |
> ghci> putUTF8Ln $ flipUTF8 "[?np_bs!]"
> [¡sq‾bu¿]
-}
flipUTF8 :: (UTF8 a) => a -> a
flipUTF8 = decode . flipString flipTab . encode

{- |
> ghci> putUTF8Ln $ (unflipUTF8 . flipUTF8) "[?np_bs!]"
> [?np_bs!]
-}
unflipUTF8 :: (UTF8 a) => a -> a
unflipUTF8 = decode . flipString unflipTab . encode

-- | Omits chars it doesn't know how to flip. Possibly
--  it's more desirable to just be id on such chars?
flipString :: [(Int,Int)] -> ByteString -> ByteString
flipString tab = encode
                  . reverse
                    . fmap (maybe ' ' chr
                              . flip lookup tab)
                      . decode

unflipTab :: [(Int,Int)]
unflipTab = fmap (uncurry(flip(,))) flipTab

flipTab :: [(Int,Int)]
flipTab = fmap (\(a,b)->(ord a,b))
  [('a', 0x250)
  ,('b', ord 'q')
  ,('c', 0x254)
  ,('d', ord 'p')
  ,('e', 0x1dd)
  ,('f', 0x25f)
  ,('g', 0x183)
  ,('h', 0x265)
  ,('i', 0x131)
  ,('j', 0x27e)
  ,('k', 0x29e)
  ,('l', ord 'l')
  ,('m', 0x26f)
  ,('n', ord 'u')
  ,('o', ord 'o')
  ,('p', ord 'b')
  ,('q', ord 'd')
  ,('r', 0x279)
  ,('s', ord 's')
  ,('t', 0x287)
  ,('u', ord 'n')
  ,('v', 0x28c)
  ,('w', 0x28d)
  ,('x', ord 'x')
  ,('y', 0x28e)
  ,('z', ord 'z')
  ,('.', 0x2d9)
  ,('[', ord ']')
  ,(']', ord '[')
  ,('{', ord '}')
  ,('}', ord '{')
  ,('<', ord '>')
  ,('>', ord '<')
  ,('?', 0xbf)
  ,('!', 0xa1)
  ,('\'', ord ',')
  ,('_', 0x203e)
  ,(';', 0x061b)
  ]

{-
ghci> mapM_ print . zip (fmap show [0..9] ++ fmap (:[]) ['a'..'f']) . fmap (drop 4 . toBits) $ [0..15]
("0",[0,0,0,0])
("1",[0,0,0,1])
("2",[0,0,1,0])
("3",[0,0,1,1])
("4",[0,1,0,0])
("5",[0,1,0,1])
("6",[0,1,1,0])
("7",[0,1,1,1])
("8",[1,0,0,0])
("9",[1,0,0,1])
("a",[1,0,1,0])
("b",[1,0,1,1])
("c",[1,1,0,0])
("d",[1,1,0,1])
("e",[1,1,1,0])
("f",[1,1,1,1])

class (Num a) => Bits a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  xor :: a -> a -> a
  complement :: a -> a
  shift :: a -> Int -> a
  rotate :: a -> Int -> a
  bit :: Int -> a
  setBit :: a -> Int -> a
  clearBit :: a -> Int -> a
  complementBit :: a -> Int -> a
  testBit :: a -> Int -> Bool
  bitSize :: a -> Int
  isSigned :: a -> Bool
  shiftL :: a -> Int -> a
  shiftR :: a -> Int -> a
  rotateL :: a -> Int -> a
  rotateR :: a -> Int -> a

uncheckedIShiftL#   :: Int# -> Int# -> Int#
uncheckedIShiftRA#  :: Int# -> Int# -> Int#
uncheckedIShiftRL#  :: Int# -> Int# -> Int#
uncheckedShiftL#    :: Word# -> Int# -> Word#
uncheckedShiftRL#   :: Word# -> Int# -> Word#
-}



{-
data Char#
gtChar# :: Char# -> Char# -> Bool
geChar# :: Char# -> Char# -> Bool
eqChar# :: Char# -> Char# -> Bool
neChar# :: Char# -> Char# -> Bool
ltChar# :: Char# -> Char# -> Bool
leChar# :: Char# -> Char# -> Bool
ord# :: Char# -> Int#

data Int#
(+#) :: Int# -> Int# -> Int#
(-#) :: Int# -> Int# -> Int#
(*#) :: Int# -> Int# -> Int#
(>#) :: Int# -> Int# -> Bool
(>=#) :: Int# -> Int# -> Bool
(==#) :: Int# -> Int# -> Bool
(/=#) :: Int# -> Int# -> Bool
(<#) :: Int# -> Int# -> Bool
(<=#) :: Int# -> Int# -> Bool
chr# :: Int# -> Char#
int2Word# :: Int# -> Word#
uncheckedIShiftL# :: Int# -> Int# -> Int#
uncheckedIShiftRA# :: Int# -> Int# -> Int#
uncheckedIShiftRL# :: Int# -> Int# -> Int#

data Word#
plusWord# :: Word# -> Word# -> Word#
minusWord# :: Word# -> Word# -> Word#
timesWord# :: Word# -> Word# -> Word#
and# :: Word# -> Word# -> Word#
or# :: Word# -> Word# -> Word#
xor# :: Word# -> Word# -> Word#
not# :: Word# -> Word#
uncheckedShiftL# :: Word# -> Int# -> Word#
uncheckedShiftRL# :: Word# -> Int# -> Word#
word2Int# :: Word# -> Int#
gtWord# :: Word# -> Word# -> Bool
geWord# :: Word# -> Word# -> Bool
eqWord# :: Word# -> Word# -> Bool
neWord# :: Word# -> Word# -> Bool
ltWord# :: Word# -> Word# -> Bool
leWord# :: Word# -> Word# -> Bool
narrow8Int# :: Int# -> Int#
narrow16Int# :: Int# -> Int#
narrow32Int# :: Int# -> Int#
narrow8Word# :: Word# -> Word#
narrow16Word# :: Word# -> Word#
narrow32Word# :: Word# -> Word#

data MutByteArr# s
newByteArray# :: Int# -> State# s -> (#State# s, MutByteArr# s#)
newPinnedByteArray# :: Int# -> State# s -> (#State# s, MutByteArr# s#)
byteArrayContents# :: ByteArr# -> Addr#
sameMutableByteArray# :: MutByteArr# s -> MutByteArr# s -> Bool
unsafeFreezeByteArray# :: MutByteArr# s -> State# s -> (#State# s, ByteArr##)
sizeofByteArray# :: ByteArr# -> Int#
sizeofMutableByteArray# :: MutByteArr# s -> Int#
indexCharArray# :: ByteArr# -> Int# -> Char#
indexWideCharArray# :: ByteArr# -> Int# -> Char#
indexIntArray# :: ByteArr# -> Int# -> Int#
indexWordArray# :: ByteArr# -> Int# -> Word#
indexAddrArray# :: ByteArr# -> Int# -> Addr#
indexInt8Array# :: ByteArr# -> Int# -> Int#
indexInt16Array# :: ByteArr# -> Int# -> Int#
indexInt32Array# :: ByteArr# -> Int# -> Int#
indexInt64Array# :: ByteArr# -> Int# -> Int#
indexWord8Array# :: ByteArr# -> Int# -> Word#
indexWord16Array# :: ByteArr# -> Int# -> Word#
indexWord32Array# :: ByteArr# -> Int# -> Word#
indexWord64Array# :: ByteArr# -> Int# -> Word#
readCharArray# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Char##)
readWideCharArray# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Char##)
readIntArray# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Int##)
readWordArray# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Word##)
readAddrArray# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Addr##)
readInt8Array# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Int##)
readInt16Array# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Int##)
readInt32Array# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Int##)
readInt64Array# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Int##)
readWord8Array# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Word##)
readWord16Array# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Word##)
readWord32Array# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Word##)
readWord64Array# :: MutByteArr# s -> Int# -> State# s -> (#State# s, Word##)
writeCharArray# :: MutByteArr# s -> Int# -> Char# -> State# s -> State# s
writeWideCharArray# :: MutByteArr# s -> Int# -> Char# -> State# s -> State# s
writeIntArray# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeWordArray# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s
writeAddrArray# :: MutByteArr# s -> Int# -> Addr# -> State# s -> State# s
writeInt8Array# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeInt16Array# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeInt32Array# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeInt64Array# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeWord8Array# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s
writeWord16Array# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s
writeWord32Array# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s
writeWord64Array# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s

data Addr#
nullAddr# :: Addr#
plusAddr# :: Addr# -> Int# -> Addr#
minusAddr# :: Addr# -> Addr# -> Int#
remAddr# :: Addr# -> Int# -> Int#
addr2Int# :: Addr# -> Int#
int2Addr# :: Int# -> Addr#
gtAddr# :: Addr# -> Addr# -> Bool
geAddr# :: Addr# -> Addr# -> Bool
eqAddr# :: Addr# -> Addr# -> Bool
neAddr# :: Addr# -> Addr# -> Bool
ltAddr# :: Addr# -> Addr# -> Bool
leAddr# :: Addr# -> Addr# -> Bool
indexCharOffAddr# :: Addr# -> Int# -> Char#
indexWideCharOffAddr# :: Addr# -> Int# -> Char#
indexIntOffAddr# :: Addr# -> Int# -> Int#
indexWordOffAddr# :: Addr# -> Int# -> Word#
indexAddrOffAddr# :: Addr# -> Int# -> Addr#
indexInt8OffAddr# :: Addr# -> Int# -> Int#
indexInt16OffAddr# :: Addr# -> Int# -> Int#
indexInt32OffAddr# :: Addr# -> Int# -> Int#
indexInt64OffAddr# :: Addr# -> Int# -> Int#
indexWord8OffAddr# :: Addr# -> Int# -> Word#
indexWord16OffAddr# :: Addr# -> Int# -> Word#
indexWord32OffAddr# :: Addr# -> Int# -> Word#
indexWord64OffAddr# :: Addr# -> Int# -> Word#
readCharOffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Char##)
readWideCharOffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Char##)
readIntOffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Int##)
readWordOffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Word##)
readAddrOffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Addr##)
readInt8OffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Int##)
readInt16OffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Int##)
readInt32OffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Int##)
readInt64OffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Int##)
readWord8OffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Word##)
readWord16OffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Word##)
readWord32OffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Word##)
readWord64OffAddr# :: Addr# -> Int# -> State# s -> (#State# s, Word##)
writeCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
writeWideCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
writeIntOffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeWordOffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeAddrOffAddr# :: Addr# -> Int# -> Addr# -> State# s -> State# s
writeInt8OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt16OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt32OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt64OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeWord8OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord16OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord32OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord64OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s

data State# s
data RealWorld

dataToTag# :: a -> Int#
tagToEnum# :: Int# -> a

reallyUnsafePtrEquality# :: a -> a -> Int#

data BCO#
addrToHValue# :: Addr# -> (#a#)
mkApUpd0# :: BCO# -> (#a#)
newBCO# :: ByteArr# -> ByteArr# -> Array# a -> Int# -> ByteArr# -> State# s -> (#State# s, BCO##)
unpackClosure# :: a -> (#Addr#, Array# b, ByteArr##)
getApStackVal# :: a -> Int# -> (#Int#, b#)
seq :: a -> b -> b
inline :: a -> a
lazy :: a -> a

data Any a
unsafeCoerce# :: a -> b



--------GHC.Exts

data Int = I# Int#
data Word = W# Word#

data Char = C# Char#
data Ptr a = Ptr Addr#
data FunPtr a = FunPtr Addr#

shiftL# :: Word# -> Int# -> Word#
shiftRL# :: Word# -> Int# -> Word#
iShiftL# :: Int# -> Int# -> Int#
iShiftRA# :: Int# -> Int# -> Int#
iShiftRL# :: Int# -> Int# -> Int#
-}

