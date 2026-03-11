{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude hiding (replicate)

-- liftA2 is needed by GHC 9.4
import Control.Applicative (liftA2)
import Control.Monad.ST (runST)
import Data.Bytes.Builder
import Data.Bytes.Builder.Template (bldr)
import Data.Bytes.Types (MutableBytes (MutableBytes))
import Data.Char (chr, ord)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Primitive (ByteArray, PrimArray)
import Data.Text.Short (ShortText)
import Data.WideWord (Word128 (Word128), Word256 (Word256))
import Data.Word
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary, (===))
import Test.QuickCheck.Instances.Natural ()
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@=?))
import Text.Printf (printf)

import qualified Arithmetic.Nat as Nat
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Builder.Bounded as Bounded
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.Bytes.Text.Latin1 as Latin1
import qualified Data.List as L
import qualified Data.Primitive as PM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as TQC
import qualified Prelude

import qualified HexWord64
import qualified Word16Tree

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "live"
        [ TQC.testProperty "word64Dec" $ \w ->
            runConcat 1 (word64Dec w) === pack (show w)
        , TQC.testProperty "word64Dec-x3" $ \x y z ->
            runConcat 1 (word64Dec x <> word64Dec y <> word64Dec z)
              === pack (show x ++ show y ++ show z)
        , TQC.testProperty "int64Dec-x3" $ \x y z ->
            runConcat 1 (int64Dec x <> int64Dec y <> int64Dec z)
              === pack (show x ++ show y ++ show z)
        , TQC.testProperty "word64BE-x3" $ \x y z ->
            runConcat 1 (word64BE x <> word64BE y <> word64BE z)
              === pack (LB.unpack (BB.toLazyByteString (BB.word64BE x <> BB.word64BE y <> BB.word64BE z)))
        , TQC.testProperty "word256PaddedLowerHex" $ \w ->
            Bounded.run Nat.constant (Bounded.word256PaddedLowerHex w)
              === pack (showWord256PaddedLowerHex w)
        , TQC.testProperty "word128PaddedUpperHex" $ \w ->
            Bounded.run Nat.constant (Bounded.word128PaddedUpperHex w)
              === pack (showWord128PaddedUpperHex w)
        , TQC.testProperty "word64PaddedUpperHex" $ \w ->
            runConcat 1 (word64PaddedUpperHex w)
              === pack (showWord64PaddedUpperHex w)
        , TQC.testProperty "word16PaddedLowerHex" $ \w ->
            runConcat 1 (word16PaddedLowerHex w)
              === pack (showWord16PaddedLowerHex w)
        , TQC.testProperty "wordPaddedDec2" $ TQC.forAll (TQC.choose (0, 99)) $ \w ->
            Bounded.run Nat.two (Bounded.wordPaddedDec2 w)
              === pack (zeroPadL 2 (show w))
        , TQC.testProperty "wordPaddedDec4" $ TQC.forAll (TQC.choose (0, 9999)) $ \w ->
            Bounded.run Nat.constant (Bounded.wordPaddedDec4 w)
              === pack (zeroPadL 4 (show w))
        , TQC.testProperty "wordPaddedDec9" $ TQC.forAll (TQC.choose (0, 999999999)) $ \w ->
            Bounded.run Nat.constant (Bounded.wordPaddedDec9 w)
              === pack (zeroPadL 9 (show w))
        , TQC.testProperty "word8Dec" $ \w ->
            runConcat 1 (word8Dec w)
              === pack (show w)
        , TQC.testProperty "consLength32BE" $ \w ->
            runConcat 1 (consLength32BE (word8Dec w))
              === pack ('\x00' : '\x00' : '\x00' : chr (L.length (show w)) : show w)
        , TQC.testProperty "consLength64BE-uni" $ \w ->
            pack
              ( '\x00'
                  : '\x00'
                  : '\x00'
                  : '\x00'
                  : '\x00'
                  : '\x00'
                  : '\x00'
                  : chr (L.length (show w))
                  : show w
              )
              === runConcat 1 (consLength64BE (word16Dec w))
        , TQC.testProperty "consLength64BE-multi" $ \w ->
            pack
              ( '\x00'
                  : '\x00'
                  : '\x00'
                  : '\x00'
                  : '\x00'
                  : '\x00'
                  : '\x00'
                  : chr (1 + L.length (show w))
                  : '\x42'
                  : show w
              )
              === runConcat 1 (consLength64BE (word8 0x42 <> flush 2 <> word16Dec w))
        , THU.testCase "stringUtf8" $
            packUtf8 "¿Cómo estás? I am doing well."
              @=? runConcat 1 (stringUtf8 "¿Cómo estás? I am doing well.")
        , THU.testCase "doubleDec-A" $
            pack (show (2 :: Int)) @=? runConcat 1 (doubleDec 2.0)
        , THU.testCase "doubleDec-B" $
            pack (show (2.5 :: Double)) @=? runConcat 1 (doubleDec 2.5)
        , THU.testCase "doubleDec-C" $
            pack ("1e+15") @=? runConcat 1 (doubleDec 1e15)
        , THU.testCase "doubleDec-D" $
            pack ("-42") @=? runConcat 1 (doubleDec (-42))
        , THU.testCase "doubleDec-E" $
            AsciiByteArray (pack ("-8.8888888888889e+14")) @=? AsciiByteArray (runConcat 1 (doubleDec (-888888888888888.8888888)))
        , THU.testCase "doubleDec-F" $
            pack ("42") @=? runConcat 1 (doubleDec 42)
        , THU.testCase "doubleDec-G" $
            pack ("0") @=? runConcat 1 (doubleDec 0)
        , THU.testCase "doubleDec-H" $
            pack ("0.5") @=? runConcat 1 (doubleDec 0.5)
        , THU.testCase "doubleDec-I" $
            pack ("-0.5") @=? runConcat 1 (doubleDec (-0.5))
        , THU.testCase "doubleDec-J" $
            pack ("999999999") @=? runConcat 1 (doubleDec 999999999)
        , THU.testCase "doubleDec-K" $
            pack ("-99999999") @=? runConcat 1 (doubleDec (-99999999))
        , THU.testCase "doubleDec-L" $
            AsciiByteArray (pack ("6.6666666666667e-12")) @=? AsciiByteArray (runConcat 1 (doubleDec (2 / 300_000_000_000)))
        , THU.testCase "doubleDec-M" $
            AsciiByteArray (pack ("6.6666666666667e-10")) @=? AsciiByteArray (runConcat 1 (doubleDec 6.666666666666667e-10))
        , THU.testCase "doubleDec-N" $
            AsciiByteArray (pack ("5e-10")) @=? AsciiByteArray (runConcat 1 (doubleDec 5.0e-10))
        , THU.testCase "doubleDec-O" $
            AsciiByteArray (pack ("1.6666666666667e-10")) @=? AsciiByteArray (runConcat 1 (doubleDec 1.6666666666666669e-10))
        , THU.testCase "doubleDec-P" $
            AsciiByteArray (pack ("1e-09")) @=? AsciiByteArray (runConcat 1 (doubleDec 1.0e-9))
        , THU.testCase "doubleDec-Q" $
            AsciiByteArray (pack ("1e-08")) @=? AsciiByteArray (runConcat 1 (doubleDec 1.0e-8))
        , THU.testCase "shortTextJsonString-A" $
            pack ("\"hello\"") @=? runConcat 1 (shortTextJsonString "hello")
        , THU.testCase "shortTextJsonString-B" $
            pack ("\"\\\\_\\\"_/\"") @=? runConcat 1 (shortTextJsonString "\\_\"_/")
        , THU.testCase "shortTextJsonString-C" $
            pack ("\"Hi\\r\\nLo\"") @=? runConcat 1 (shortTextJsonString "Hi\r\nLo")
        , THU.testCase "shortTextJsonString-D" $
            pack ("\"Hi\\u001BLo\"") @=? runConcat 1 (shortTextJsonString "Hi\ESCLo")
        , THU.testCase "word-16-tree" $
            Word16Tree.expectedSmall
              @=? runConcat
                1
                (Word16Tree.encode Word16Tree.exampleSmall)
        , THU.testCase "byteArray-small" $
            let a = replicateByte 3 0x50
                b = replicateByte 5 0x51
             in mconcat [a, b]
                  @=? runConcat
                    1
                    (byteArray a <> byteArray b)
        , THU.testCase "byteArray-big" $
            let a = replicateByte 2105 0x50
                b = replicateByte 725 0x51
                c = replicateByte 900 0x52
                d = replicateByte 800 0x53
                e = replicateByte 700 0x54
                f = replicateByte 950 0x55
                g = replicateByte 975 0x56
                h = replicateByte 3000 0x57
                i = replicateByte 125 0x58
             in mconcat [a, b, c, d, e, f, g, h, i]
                  @=? runConcat
                    1
                    ( byteArray a
                        <> byteArray b
                        <> byteArray c
                        <> byteArray d
                        <> byteArray e
                        <> byteArray f
                        <> byteArray g
                        <> byteArray h
                        <> byteArray i
                    )
        , TQC.testProperty "word16ArrayLE" $ \(xs :: [Word16]) ->
            let ys = Exts.fromList xs :: PrimArray Word16
             in runConcat 1 (foldMap word16LE xs)
                  === runConcat 1 (word16ArrayLE ys 0 (Prelude.length xs))
        , TQC.testProperty "word16ArrayBE" $ \(xs :: [Word16]) ->
            let ys = Exts.fromList xs :: PrimArray Word16
             in runConcat 1 (foldMap word16BE xs)
                  === runConcat 1 (word16ArrayBE ys 0 (Prelude.length xs))
        , TQC.testProperty "word32ArrayLE" $ \(xs :: [Word32]) ->
            let ys = Exts.fromList xs :: PrimArray Word32
             in runConcat 1 (foldMap word32LE xs)
                  === runConcat 1 (word32ArrayLE ys 0 (Prelude.length xs))
        , TQC.testProperty "word32ArrayBE" $ \(xs :: [Word32]) ->
            let ys = Exts.fromList xs :: PrimArray Word32
             in runConcat 1 (foldMap word32BE xs)
                  === runConcat 1 (word32ArrayBE ys 0 (Prelude.length xs))
        , TQC.testProperty "word64ArrayLE" $ \(xs :: [Word64]) ->
            let ys = Exts.fromList xs :: PrimArray Word64
             in runConcat 1 (foldMap word64LE xs)
                  === runConcat 1 (word64ArrayLE ys 0 (Prelude.length xs))
        , TQC.testProperty "word64ArrayBE" $ \(xs :: [Word64]) ->
            let ys = Exts.fromList xs :: PrimArray Word64
             in runConcat 1 (foldMap word64BE xs)
                  === runConcat 1 (word64ArrayBE ys 0 (Prelude.length xs))
        , TQC.testProperty "word128ArrayLE" $ \(xs :: [Word128]) ->
            let ys = Exts.fromList xs :: PrimArray Word128
             in runConcat 1 (foldMap word128LE xs)
                  === runConcat 1 (word128ArrayLE ys 0 (Prelude.length xs))
        , TQC.testProperty "word128ArrayBE" $ \(xs :: [Word128]) ->
            let ys = Exts.fromList xs :: PrimArray Word128
             in runConcat 1 (foldMap word128BE xs)
                  === runConcat 1 (word128ArrayBE ys 0 (Prelude.length xs))
        , TQC.testProperty "word256ArrayLE" $ \(xs :: [Word256]) ->
            let ys = Exts.fromList xs :: PrimArray Word256
             in runConcat 1 (foldMap word256LE xs)
                  === runConcat 1 (word256ArrayLE ys 0 (Prelude.length xs))
        , TQC.testProperty "word256ArrayBE" $ \(xs :: [Word256]) ->
            let ys = Exts.fromList xs :: PrimArray Word256
             in runConcat 1 (foldMap word256BE xs)
                  === runConcat 1 (word256ArrayBE ys 0 (Prelude.length xs))
        , TQC.testProperty "word64Vlq" $ \(x :: Word64) ->
            runConcat 1 (word64Vlq x)
              === naiveVlq (fromIntegral x)
        , TQC.testProperty "word64LEB128" $ \(x :: Word64) ->
            runConcat 1 (word64LEB128 x)
              === naiveLeb128 (fromIntegral x)
        , TQC.testProperty "naturalDec-A" $ \(x :: Natural) ->
            runConcat 1 (naturalDec x)
              === pack (show x)
        , TQC.testProperty "naturalDec-B" $ \(x :: Natural) ->
            let y = 1234567892345678934678987654321 * x
             in runConcat 1 (naturalDec y)
                  === pack (show y)
        , testGroup
            "leb128-encoding"
            [ THU.testCase "16" $
                Chunks.concat (run 16 (word64LEB128 16))
                  @=? Latin1.fromString "\x10"
            , THU.testCase "1000000" $
                Chunks.concat (run 16 (word64LEB128 1000000))
                  @=? Exts.fromList [0xc0, 0x84, 0x3d]
            , THU.testCase "deadbeef-smile" $ do
                let inp = Latin1.fromString "\xDE\xAD\xBE\xEF"
                (Chunks.concat . run 16) (sevenEightSmile inp)
                  @=? Latin1.fromString "\x6F\x2B\x37\x6E\x0F"
            ]
        , testGroup
            "seven/eight encoding"
            [ THU.testCase "deadbeef" $ do
                let inp = Latin1.fromString "\xDE\xAD\xBE\xEF"
                (Chunks.concat . run 16) (sevenEightRight inp)
                  @=? Latin1.fromString "\x6F\x2B\x37\x6E\x78"
            , THU.testCase "deadbeef-smile" $ do
                let inp = Latin1.fromString "\xDE\xAD\xBE\xEF"
                (Chunks.concat . run 16) (sevenEightSmile inp)
                  @=? Latin1.fromString "\x6F\x2B\x37\x6E\x0F"
            ]
        ]
    , testGroup
        "alternate"
        [ TQC.testProperty "HexWord64" $ \x y ->
            runConcat
              1
              ( fromBounded Nat.constant (HexWord64.word64PaddedUpperHex x)
                  <> fromBounded Nat.constant (HexWord64.word64PaddedUpperHex y)
              )
              === pack (showWord64PaddedUpperHex x <> showWord64PaddedUpperHex y)
        ]
    , testGroup
        "putMany"
        [ THU.testCase "A" $ do
            ref <- newIORef []
            let txt = "hello_world_are_you_listening" :: [Char]
            putMany 7 ascii txt (bytesOntoRef ref)
            res <- readIORef ref
            id $
              [ map c2w "hello_"
              , map c2w "world_"
              , map c2w "are_yo"
              , map c2w "u_list"
              , map c2w "ening"
              ]
                @=? map Exts.toList (Exts.toList res)
        ]
    , testGroup
        "putManyConsLength"
        [ THU.testCase "A" $ do
            ref <- newIORef []
            let txt = "hello_world_are_you_listening" :: [Char]
            putManyConsLength
              Nat.constant
              (\n -> Bounded.word16BE (fromIntegral n))
              16
              ascii
              txt
              (bytesOntoRef ref)
            res <- readIORef ref
            id $
              [ 0x00 : 0x0A : map c2w "hello_worl"
              , 0x00 : 0x0A : map c2w "d_are_you_"
              , 0x00 : 0x09 : map c2w "listening"
              ]
                @=? map Exts.toList (Exts.toList res)
        ]
    , testGroup
        "bytes templates"
        [ THU.testCase "A" $ do
            let name = Just ("foo" :: ShortText)
                msgBuilder = [bldr|Hello `fromMaybe "World" name`!\n|]
                msg = Chunks.concat . Builder.run 200 $ msgBuilder
             in Ascii.fromString "Hello foo!\n" @=? msg
        , THU.testCase "B" $ do
            let one = "foo" :: ShortText
                two = "bar" :: String
                msgBuilder = [bldr|`one``two`|]
                msg = Chunks.concat . Builder.run 200 $ msgBuilder
             in Ascii.fromString "foobar" @=? msg
        , THU.testCase "C" $ do
            let msgBuilder = [bldr|a backtick for you: \`|]
                msg = Chunks.concat . Builder.run 200 $ msgBuilder
             in Ascii.fromString "a backtick for you: `" @=? msg
        , THU.testCase "D" $ do
            let i = 137 :: Int
                msgBuilder = [bldr|there are `i` lights!|]
                msg = Chunks.concat . Builder.run 200 $ msgBuilder
             in Ascii.fromString "there are 137 lights!" @=? msg
        ]
    ]

bytesOntoRef ::
  IORef [PM.ByteArray] ->
  MutableBytes Exts.RealWorld ->
  IO ()
bytesOntoRef !ref (MutableBytes buf off len) = do
  rs <- readIORef ref
  dst <- PM.newByteArray len
  PM.copyMutableByteArray dst 0 buf off len
  dst' <- PM.unsafeFreezeByteArray dst
  writeIORef ref (rs ++ [dst'])

replicateByte :: Int -> Word8 -> ByteArray
replicateByte n w = runST $ do
  m <- PM.newByteArray n
  PM.setByteArray m 0 n w
  PM.unsafeFreezeByteArray m

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

packUtf8 :: String -> ByteArray
packUtf8 = Exts.fromList . ByteString.unpack . TE.encodeUtf8 . T.pack

showWord256PaddedLowerHex :: Word256 -> String
showWord256PaddedLowerHex (Word256 hi mhi mlo lo) = printf "%016x%016x%016x%016x" hi mhi mlo lo

showWord128PaddedUpperHex :: Word128 -> String
showWord128PaddedUpperHex (Word128 hi lo) = printf "%016X%016X" hi lo

showWord64PaddedUpperHex :: Word64 -> String
showWord64PaddedUpperHex = printf "%016X"

showWord16PaddedLowerHex :: Word16 -> String
showWord16PaddedLowerHex = printf "%04x"

runConcat :: Int -> Builder -> ByteArray
runConcat n = Chunks.concatU . run n

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- Just a wrapper with a show instance that displays as ascii when possible.
newtype AsciiByteArray = AsciiByteArray ByteArray
  deriving (Eq)

instance Show AsciiByteArray where
  show (AsciiByteArray b) =
    if Bytes.all (\w -> w >= 32 && w < 127) (Bytes.fromByteArray b)
      then Latin1.toString (Bytes.fromByteArray b)
      else show (show b)

instance Arbitrary Word128 where
  arbitrary = liftA2 Word128 TQC.arbitrary TQC.arbitrary

instance Arbitrary Word256 where
  arbitrary = Word256 <$> TQC.arbitrary <*> TQC.arbitrary <*> TQC.arbitrary <*> TQC.arbitrary

zeroPadL :: Int -> String -> String
zeroPadL n s
  | length s < n = Prelude.replicate (n - length s) '0' ++ s
  | otherwise = s

naiveLeb128 :: Natural -> ByteArray
naiveLeb128 x =
  Bytes.toByteArray (Bytes.unsafeDrop 1 (Exts.fromList (0xFF : go [] x)))
 where
  go !xs !n =
    let (q, r) = quotRem n 128
        r' = fromIntegral @Natural @Word8 r
        w =
          if q == 0
            then r'
            else Bits.setBit r' 7
        xs' = w : xs
     in if q == 0
          then L.reverse xs'
          else go xs' q

naiveVlq :: Natural -> ByteArray
naiveVlq x =
  Bytes.toByteArray (Bytes.unsafeDrop 1 (Exts.fromList (0xFF : go [] x)))
 where
  go !xs !n =
    let (q, r) = quotRem n 128
        r' = fromIntegral @Natural @Word8 r
        w = case xs of
          [] -> r'
          _ -> Bits.setBit r' 7
        xs' = w : xs
     in if q == 0
          then xs'
          else go xs' q
