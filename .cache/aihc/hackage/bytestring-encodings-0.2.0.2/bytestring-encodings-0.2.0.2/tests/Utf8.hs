{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Utf8
  ( testUtf8
  ) where

{-# OPTIONS_GHC -Wall #-}

import Control.Applicative (liftA2)
import Control.Monad (join, sequence)
import Data.Bits ((.&.), xor)
import Data.ByteString.Encodings (isUtf8)
import Data.Char (chr)
import Data.Foldable (foldl')
import Data.Word (Word8)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI

randWBS_Fail :: Gen B.ByteString
randWBS_Fail = do
  m <- Gen.enum 1 7
  case m of
    1 -> g ( [ Gen.enum 0b10000000 0b11101111   -- Byte 1 <- [128..239]
             , Gen.enum 0b00000000 0b01000000   -- Byte 2 <- [  0..127]
             , Gen.enum 0b00000000 0b01000000   -- Byte 3 <- [  0..127]
             , Gen.enum 0b00000000 0b01000000   -- Byte 4 <- [  0..127]
             ] :: [Gen Word8])
    2 -> g ( [ Gen.enum 0b11111000 0b11111111   -- Byte 1 <- [248..255]
             , Gen.enum 0b11000000 0b11111111   -- Byte 2 <- [192..255]
             , Gen.enum 0b11000000 0b11111111   -- Byte 3 <- [192..255] 
             , Gen.enum 0b11000000 0b11111111   -- Byte 3 <- [192..255] 
             ] :: [Gen Word8])
    3 -> g ( [ Gen.enum 0b10000000 0b10111111   -- Byte 1 <- [128..191]
             , Gen.enum 0b00000000 0b01000000   -- Byte 2 <- [  0..127] 
             ] :: [Gen Word8])
    4 -> g ( [ Gen.enum 0b11100000 0b11111111   -- Byte 1 <- [224..255]
             , Gen.enum 0b11000000 0b11111111   -- Byte 2 <- [192..255] 
             ] :: [Gen Word8])
    5 -> g ( [ Gen.enum 0b10000000 0b11011111   -- Byte 1 <- [128..223]
             , Gen.enum 0b00000000 0b01000000   -- Byte 2 <- [  0..127] 
             , Gen.enum 0b00000000 0b01000000   -- Byte 3 <- [  0..127] 
             ] :: [Gen Word8])
    6 -> g ( [ Gen.enum 0b11110000 0b11111111   -- Byte 1 <- [240..255]
             , Gen.enum 0b00000000 0b01000000   -- Byte 2 <- [  0..127]
             , Gen.enum 0b00000000 0b01000000   -- Byte 2 <- [  0..127]
             ] :: [Gen Word8])
    7 -> g ( [ Gen.enum 0b11100000 0b11111111 
             ] :: [Gen Word8])
    _ -> pure $ B.pack [0b01000000] 
   where
      g :: [Gen Word8] -> Gen B.ByteString
      g ls = foldl (liftA2 mappend) (pure B.empty) $ fmap (fmap B.singleton) ls 

randWBS_Succ :: Gen B.ByteString
randWBS_Succ = do
  m <- Gen.enum 1 4
  case m of
    1 -> g ( [ Gen.enum 0b00000000 0b01000000
             ] :: [Gen Word8])
    2 -> g ( [ Gen.enum 0b11000001 0b11011111
             , Gen.enum 0b10000000 0b10111111
             ] :: [Gen Word8])
    3 -> g ( [ Gen.enum 0b11100000 0b11101111
             , Gen.enum 0b10000000 0b10111111
             , Gen.enum 0b10000000 0b10111111
             ] :: [Gen Word8])
    4 -> g ( [ Gen.enum 0b11110000 0b11110111
             , Gen.enum 0b10000000 0b10111111
             , Gen.enum 0b10000000 0b10111111
             , Gen.enum 0b10000000 0b10111111] :: [Gen Word8])
    where
      g :: [Gen Word8] -> Gen B.ByteString
      g ls = foldl (liftA2 mappend) (pure B.empty) $ fmap (fmap B.singleton) ls 
      
sizedByteString_Fail :: Range.Size -> Gen B.ByteString
sizedByteString_Fail (Range.Size n) = do
  m <- Gen.enum 0 n
  fmap (foldl' B.append B.empty) $ Gen.list (Range.constant 0 m) randWBS_Fail

sizedByteString_Succ :: Range.Size -> Gen B.ByteString
sizedByteString_Succ (Range.Size n) = do
  m <- Gen.enum 0 n
  fmap (foldl' B.append B.empty) $ Gen.list (Range.constant 0 m) randWBS_Succ

showRawByteString :: B.ByteString -> String
showRawByteString bs@(BI.PS fptr off len) =
  "Payload: " ++ show (B.unpack bs) ++ ", ptr: " ++ show fptr ++ ", offset: " ++ show off ++ " len: " ++ show len

prop_isUtf8_Fail :: Property
prop_isUtf8_Fail =
  property $ do
    xs <- forAllWith showRawByteString (Gen.sized sizedByteString_Fail)
    if B.length xs > 0
      then isUtf8 xs === False
      else isUtf8 xs === True

prop_isUtf8_Succ :: Property
prop_isUtf8_Succ =
  property $ do
    xs <- forAllWith showRawByteString (Gen.sized sizedByteString_Succ)
    isUtf8 xs === True

testUtf8 :: IO Bool
testUtf8 = checkParallel $$(discover)
