{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: © 2019-2020  Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Main (main)  where

import           Control.Monad
import           Data.Bits
import           Data.Int
import           Data.Monoid             (mempty)
import           Data.Word
import qualified Foreign.Marshal         as F
import qualified Foreign.Ptr             as F
import           Text.Printf             (printf)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Short   as SBS

import qualified Test.QuickCheck.Monadic as QCM
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck   as QC

import qualified Data.XOR                as IUT

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "hello"
    [ testCase "REF"             $ xor32StrictByteString'ref  0x37fa213d "\x7f\x9f\x4d\x51\x58" @?= "Hello"
    , testCase "S.ByteString"    $ IUT.xor32StrictByteString' 0x37fa213d "\x7f\x9f\x4d\x51\x58" @?= (0xfa213d37,"Hello")
    , testCase "L.ByteString"    $ IUT.xor32LazyByteString    0x37fa213d "\x7f\x9f\x4d\x51\x58" @?= "Hello"
    , testCase "ShortByteString" $ IUT.xor32ShortByteString   0x37fa213d "\x7f\x9f\x4d\x51\x58" @?= "Hello"
    ]

  , testGroup "empty"
    [ testProperty "S.ByteString"    $ \msk -> IUT.xor32StrictByteString msk mempty === Data.Monoid.mempty
    , testProperty "L.ByteString"    $ \msk -> IUT.xor32LazyByteString   msk mempty === mempty
    , testProperty "ShortByteString" $ \msk -> IUT.xor32ShortByteString  msk mempty === mempty
    ]

  , testGroup "zero-xor"
    [ testProperty "S.ByteString" $ \xs -> let bs = BS.pack xs in IUT.xor32StrictByteString 0 bs === bs
    , testProperty "L.ByteString" $ \xs -> let bs = BL.fromChunks (map BS.pack xs) in IUT.xor32LazyByteString 0 bs === BL.fromChunks [BS.pack (concat xs)]
    , testProperty "ShortByteString" $ \xs -> let bs = SBS.pack xs in IUT.xor32ShortByteString 0 bs === bs
    ]

  , testProperty "xor32Ptr" $ \msk lb len8 -> QCM.monadicIO $ do
       let bufsize, ofs, len :: Int
           bufsize = 2048
           ofs     = 512 + fromIntegral (lb :: Int8)
           len     = fromIntegral (len8 :: Word8)

       QCM.run $ F.allocaBytes bufsize $ \bufptr -> do

         origbuf <- BS.packCStringLen (bufptr, bufsize)

         let (origbufPre, tmp) = BS.splitAt ofs origbuf
             (origbufMid, origbufPost) = BS.splitAt len tmp

         _ <- IUT.xor32CStringLen msk (F.castPtr bufptr `F.plusPtr` ofs,len)
         newbufIut <- BS.packCStringLen (bufptr, bufsize)

         let newbufRef = BS.concat [origbufPre, xor32StrictByteString'ref msk origbufMid, origbufPost]

         unless (BS.length newbufRef == bufsize) $ fail "internal error"

         unless (newbufRef == newbufIut) $ do
           putStrLn $ show (msk,bufsize, ofs, len)
           putStrLn $ "orig " ++ concatMap (printf "%02x") (BS.unpack origbuf)
           putStrLn $ "ref  " ++ concatMap (printf "%02x") (BS.unpack newbufRef)
           putStrLn $ "iut  " ++ concatMap (printf "%02x") (BS.unpack newbufIut)

           forM_ (zip3 [0..] (BS.unpack newbufRef) (BS.unpack newbufIut)) $ \(j,ref8,iut8) ->
             unless (ref8 == iut8) $
               printf "%d (%d): %02x %02x\n" (j::Int) (j-ofs) ref8 iut8

         return $! newbufRef == newbufIut

  , testGroup "ref-vs-iut 32-bit"
    [ testProperty "S.ByteString" $ \xs msk ->
        let bs = BS.pack xs
        in IUT.xor32StrictByteString msk bs === xor32StrictByteString'ref msk bs
    , testProperty "L.ByteString" $ \xs msk ->
        let bs  = BL.fromChunks (map BS.pack xs)
            bs' = BS.pack (concat xs)
        in IUT.xor32LazyByteString msk bs === BL.fromStrict (xor32StrictByteString'ref msk bs')
    , testProperty "ShortByteString" $ \xs msk ->
        let bs = SBS.pack xs
        in IUT.xor32ShortByteString msk bs === xor32ShortByteString'ref msk bs
    ]

  , testGroup "ref-vs-iut 8-bit"
    [ testProperty "S.ByteString" $ \xs msk ->
        let bs = BS.pack xs
        in IUT.xor8StrictByteString msk bs === xor8StrictByteString'ref msk bs
    , testProperty "L.ByteString" $ \xs msk ->
        let bs  = BL.fromChunks (map BS.pack xs)
        in IUT.xor8LazyByteString msk bs === xor8LazyByteString'ref msk bs
    , testProperty "ShortByteString" $ \xs msk ->
        let bs = SBS.pack xs
        in IUT.xor8ShortByteString msk bs === xor8ShortByteString'ref msk bs
    ]

  , testGroup "self-inverse"
    [ testProperty "S.ByteString" $ \xs msk ->
        let bs = BS.pack xs
        in IUT.xor32StrictByteString msk (IUT.xor32StrictByteString msk bs) === bs
    , testProperty "L.ByteString" $ \xs msk ->
        let bs  = BL.fromChunks (map BS.pack xs)
            bs' = BL.fromChunks [BS.pack (concat xs)]
        in IUT.xor32LazyByteString msk (IUT.xor32LazyByteString msk bs) === bs'
    , testProperty "ShortByteString" $ \xs msk ->
        let bs = SBS.pack xs
        in IUT.xor32ShortByteString msk (IUT.xor32ShortByteString msk bs) === bs
    ]
  ]

xor32StrictByteString'ref :: Word32 -> BS.ByteString -> BS.ByteString
xor32StrictByteString'ref 0    = id
xor32StrictByteString'ref msk0 = snd . BS.mapAccumL go msk0
  where
    go :: Word32 -> Word8 -> (Word32,Word8)
    go msk b = let b'   = fromIntegral msk' `xor` b
                   msk' = rotateL msk 8
               in b' `seq` (msk',b')

xor8StrictByteString'ref :: Word8 -> BS.ByteString -> BS.ByteString
xor8StrictByteString'ref 0    = id
xor8StrictByteString'ref msk0 = BS.map (xor msk0)

liftSBS :: (t -> BS.ByteString -> BS.ByteString) -> t -> SBS.ShortByteString -> SBS.ShortByteString
liftSBS op = \msk -> SBS.toShort . op msk . SBS.fromShort

liftBL :: (t -> BS.ByteString -> BS.ByteString) -> t -> BL.ByteString -> BL.ByteString
liftBL op = \msk -> BL.fromStrict . op msk . BL.toStrict

xor32ShortByteString'ref :: Word32 -> SBS.ShortByteString -> SBS.ShortByteString
xor32ShortByteString'ref = liftSBS xor32StrictByteString'ref

xor8ShortByteString'ref :: Word8 -> SBS.ShortByteString -> SBS.ShortByteString
xor8ShortByteString'ref = liftSBS xor8StrictByteString'ref

xor8LazyByteString'ref :: Word8 -> BL.ByteString -> BL.ByteString
xor8LazyByteString'ref = liftBL xor8StrictByteString'ref
