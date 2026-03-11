{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitParse
  ( BitParse(..)
  ) where

import Control.Applicative
import Data.Word
import GHC.Exts
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.String.Parse

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.ByteString      as BS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

-- | Parsers for bit strings
class BitParse a where
  -- | Version of bit string parser that can consume no inputs
  bitParse0       :: Parser a
  -- | Version of bit string parser that must consume at least one input
  bitParse1       :: Parser a

p0 :: Parser Bool
p0 = char '1' >> return True

p1 :: Parser Bool
p1 = char '0' >> return False

instance BitParse Bool where
  bitParse0 = bitParse1 <|> return False
  bitParse1 = p0 <|> p1

instance BitParse Word8 where
  bitParse0 = bitParse1 <|> return 0
  bitParse1 = do
    a :: Bool <- bitParse1
    b :: Bool <- bitParse0
    c :: Bool <- bitParse0
    d :: Bool <- bitParse0
    e :: Bool <- bitParse0
    f :: Bool <- bitParse0
    g :: Bool <- bitParse0
    h :: Bool <- bitParse0
    return $
      (if a then 0x01 else 0) .|.
      (if b then 0x02 else 0) .|.
      (if c then 0x04 else 0) .|.
      (if d then 0x08 else 0) .|.
      (if e then 0x10 else 0) .|.
      (if f then 0x20 else 0) .|.
      (if g then 0x40 else 0) .|.
      (if h then 0x80 else 0)

instance BitParse Word16 where
  bitParse0 = bitParse1 <|> return 0
  bitParse1 = do
    (a :: Word8) <- bitParse1
    (b :: Word8) <- bitParse0
    return $ (fromIntegral b .<. bitLength a) .|. fromIntegral a

instance BitParse Word32 where
  bitParse0 = bitParse1 <|> return 0
  bitParse1 = do
    (a :: Word16) <- bitParse1
    (b :: Word16) <- bitParse0
    return $ (fromIntegral b .<. bitLength a) .|. fromIntegral a

instance BitParse Word64 where
  bitParse0 = bitParse1 <|> return 0
  bitParse1 = do
    (a :: Word32) <- bitParse1
    (b :: Word32) <- bitParse0
    return $ (fromIntegral b .<. bitLength a) .|. fromIntegral a

instance BitParse BS.ByteString where
  bitParse0 = fmap BS.pack bitParse0
  bitParse1 = fmap BS.pack bitParse1

instance BitParse [Word8] where
  bitParse0 = bitParse1 <|> return []
  bitParse1 = many bitParse1

instance BitParse [Word16] where
  bitParse0 = bitParse1 <|> return []
  bitParse1 = many bitParse1

instance BitParse [Word32] where
  bitParse0 = bitParse1 <|> return []
  bitParse1 = many bitParse1

instance BitParse [Word64] where
  bitParse0 = bitParse1 <|> return []
  bitParse1 = many bitParse1

instance BitParse (DV.Vector Word8) where
  bitParse0 = bitParse1 <|> return DV.empty
  bitParse1 = fromList `fmap` bitParse0

instance BitParse (DV.Vector Word16) where
  bitParse0 = bitParse1 <|> return DV.empty
  bitParse1 = fromList `fmap` bitParse0

instance BitParse (DV.Vector Word32) where
  bitParse0 = bitParse1 <|> return DV.empty
  bitParse1 = fromList `fmap` bitParse0

instance BitParse (DV.Vector Word64) where
  bitParse0 = bitParse1 <|> return DV.empty
  bitParse1 = fromList `fmap` bitParse0

instance BitParse (DVS.Vector Word8) where
  bitParse0 = bitParse1 <|> return DVS.empty
  bitParse1 = fromList `fmap` bitParse0

instance BitParse (DVS.Vector Word16) where
  bitParse0 = bitParse1 <|> return DVS.empty
  bitParse1 = fromList `fmap` bitParse0

instance BitParse (DVS.Vector Word32) where
  bitParse0 = bitParse1 <|> return DVS.empty
  bitParse1 = fromList `fmap` bitParse0

instance BitParse (DVS.Vector Word64) where
  bitParse0 = bitParse1 <|> return DVS.empty
  bitParse1 = fromList `fmap` bitParse0

instance BitParse (DVU.Vector Bit.Bit) where
  bitParse0 = bitParse1 <|> return DVU.empty
  bitParse1 = fromList . map Bit.Bit <$> many bitParse1

instance BitParse (DVU.Vector BitTS.Bit) where
  bitParse0 = bitParse1 <|> return DVU.empty
  bitParse1 = fromList . map BitTS.Bit <$> many bitParse1
