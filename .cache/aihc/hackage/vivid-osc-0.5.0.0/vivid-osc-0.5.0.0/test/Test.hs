{-# LANGUAGE
     BinaryLiterals
   , LambdaCase
   , OverloadedStrings
   , ViewPatterns
   #-}

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Ratio
import Data.Serialize hiding (runGet)
-- import Data.Serialize.Get hiding (runGet)
-- import qualified Data.Serialize.Get as Get
-- import Data.Serialize.Put
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word

import Test.Microspec

import Vivid.OSC

main :: IO ()
main = microspec $ do
   describe "OSC types" $ do
      describe "OSC Datum" $ do
         {-
          - Problem is we don't have type tag; not high priority because
          - we test it in 'getOSC/putOSC' etc:
         it "getOSCDatum . putOSCDatum" $ \oldD ->
            let Right newD = runGet getOSCDatum (runPut $ putOSCDatum o)
            in datumEq newD oldD
            -}

         describe "examples from the OSC 1.0 spec" $ do
            it "example 1, new" $ do
               encodeOSCDatum (OSC_S "OSC")
                  === BS.pack (map (toEnum . fromEnum) ['O','S','C', '\NUL'])
            it "example 2, new" $ do
               encodeOSCDatum (OSC_S "data")
                   === BS.pack (map (toEnum . fromEnum) ("data"++replicate 4 '\NUL'))


      describe "the OSC type" $ do
         it "decode . encode" $ \o ->
            let Right new = decodeOSC (encodeOSC o)
            in new `oscEq` o
         describe "examples from the OSC 1.0 spec" $ do
            it "example 1" $
               encodeOSC (OSC "/oscillator/4/frequency" [OSC_F 440.0])
                  === BS.pack [
                     0x2f, 0x6f, 0x73, 0x63
                   , 0x69, 0x6c, 0x6c, 0x61
                   , 0x74, 0x6f, 0x72, 0x2f
                   , 0x34, 0x2f, 0x66, 0x72
                   , 0x65, 0x71, 0x75, 0x65
                   , 0x6e, 0x63, 0x79, 0x00
                   , 0x2c, 0x66, 0x00, 0x00
                   , 0x43, 0xdc, 0x00, 0x00
                   ]
            it "example 2" $
               let cmd = OSC "/foo" [OSC_I 1000, OSC_I (-1), OSC_S "hello", OSC_F 1.234, OSC_F 5.678]
                   out = BS.pack [
                        0x2f, 0x66, 0x6f, 0x6f
                      , 0x00, 0x00, 0x00, 0x00
                      , 0x2c, 0x69, 0x69, 0x73
                      , 0x66, 0x66, 0x00, 0x00
                      , 0x00, 0x00, 0x03, 0xe8
                      , 0xff, 0xff, 0xff, 0xff
                      , 0x68, 0x65, 0x6c, 0x6c
                      , 0x6f, 0x00, 0x00, 0x00
                      , 0x3f, 0x9d, 0xf3, 0xb6
                      , 0x40, 0xb5, 0xb2, 0x2d
                      ]
               in encodeOSC cmd === out
      describe "OSCBundle" $ do
         it "decode . encode" $ \t xs ->
            let oldB = OSCBundle t (map Right xs)
                Right newB = decodeOSCBundle (encodeOSCBundle oldB)
            in newB `bundleEq` oldB
   describe "bijections" $ do
      it "getOSC . putOSC" $ \origOSC ->
         let Right newOSC = decodeOSC (encodeOSC origOSC)
         in newOSC `oscEq` origOSC
      describe "getString . putString" $ \(NonNullBS s) ->
         runGet getOSCString (runPut (putOSCString s)) === Right s
      it "decodetimestamp . encodetimestamp" $ \t ->
         fromRight (runGet getOSCTimestamp (runPut (putOSCTimestamp t)))
            `timestampEq` t
      it "encodeOSC isRight (with the valid input we generate)" pending
      describe "binary blobs" $ do
         it "decode . encode" $ \(BS.pack -> b) ->
            runGet getOSCBlob (runPut (putOSCBlob b)) === Right b
      describe "decodeOSCdatumswithpadding -== getOSCDatum" pending
   describe "unit tests - a few examples for each function" $ do
      describe "putOSCDatum" $ do
         describe "OSC_S" $ do
            it "pads an extra 4 when it's already a multiple of 4" $
               encodeOSCDatum (OSC_S "four") === "four\NUL\NUL\NUL\NUL"
      describe "do this for every function" pending
-- binary up to the mazimum size
-- very large and very small floats
-- NaNs and (+/-)Infinity (floats and doubles)
-- Huge numbers of variants
-- Some example times to parse
-- Some example numbers to parse


   describe "timestamp" $ do
      it "timestamp->utc" $ \t ->
         utcToTimestamp (timestampToUTC t) `timestampEq` t
      it "utc->timestamp" $ \u ->
         timestampToUTC (utcToTimestamp u) `utcEq` u
      it "timestamp->POSIX" $ \t ->
         timestampFromPOSIX (timestampToPOSIX t) `timestampEq` t
      it "POSIX->timestamp" $ \u ->
         timestampToPOSIX (timestampFromPOSIX u) `posixEq` u


      -- Examples from https://www.eecis.udel.edu/~mills/y2k.html:
      -- (Also in RFC 5905)
      describe "examples from the NTP time author" $ do
         let f :: Day -> Double -> Bool
             f day ts = (timestampFromUTC (UTCTime day 0)) -- can test: 0.00001
                `timestampEq` Timestamp ts
         it "first day NTP" $
            f (fromGregorian 1900 01 01) 0
         it "first day UNIX" $
            f (fromGregorian 1970 01 01) 2208988800
         it "first day UTC" $
            f (fromGregorian 1972 01 01) 2272060800
         it "last day 20th century" $
            f (fromGregorian 1999 12 31) 3155587200
         it "last day NTP era 0" $
            f (fromGregorian 2036 02 07) 4294944000

      -- From ntp.org:
      describe "hex and binary representation" $ do
         let dateWereEncoding :: UTCTime
             dateWereEncoding = read "2000-08-31 18:52:30.735861"
         it "represents a hex date properly" $
            Right (timestampFromUTC dateWereEncoding)
               === runGet getOSCTimestamp
               (BS.pack [
                    -- Top 32 bits:
                    0xbd,0x59,0x27,0xee
                    -- Bottom 32:
                  , 0xbc,0x61,0x60,0x00
                  ])
         -- Of course these should be the same, but pick your poison:
         it "represents a binary date properly" $
            Right (timestampFromUTC dateWereEncoding)
               === runGet getOSCTimestamp
                  (BS.pack [
                      -- Top 32:
                      0b10111101,0b01011001,0b00100111,0b11101110
                      -- Bottom 32:
                    , 0b10111100,0b01100001,0b01100000,0b00000000
                    ])


-- TODO:
           -- also this equals, in unix (useful for when i have conversion functions):
           -- 0x39aea96e 0x000b3a75
           -- == 0b00111001101011101010100101101110
           --    0b00000000000010110011101001110101

fromRight :: Show e => Either e x -> x
fromRight = \case
   Right x -> x
   Left e -> error $ show e

-- TODO: generic-random:
instance Arbitrary OSCDatum where
   arbitrary = oneof [
       OSC_I <$> arbitrary

       -- Note no 0: OSC strings cannot contain the null character:
     , OSC_S <$> nonNullBS
     , OSC_F <$> arbitrary
     , OSC_D <$> arbitrary
     , (OSC_B . BS.pack) <$> arbitrary
     , OSC_T <$> arbitrary
     ]

nonNullBS :: Gen ByteString
nonNullBS = BS.pack <$> listOf (choose (1, maxBound :: Word8))

newtype NonNullBS = NonNullBS ByteString
 deriving (Show)

instance Arbitrary NonNullBS where
   arbitrary = NonNullBS <$> nonNullBS

instance Arbitrary Timestamp where
    -- Note cannot be negative:
   arbitrary = (Timestamp . getNonNegative) <$> arbitrary
      -- Timestamp <$> arbitrary <*> arbitrary

instance Arbitrary OSC where
   arbitrary = OSC
      <$> nonNullBS -- Again no '\NUL'
      <*> arbitrary

instance Arbitrary OSCBundle where
   arbitrary = OSCBundle
      <$> arbitrary
      <*> listOf (oneof [
           -- TODO:
           -- (Left . BS.pack) <$> arbitrary -- todo: maybe needs to be padded?
           Right <$> arbitrary -- type inference is nice!
         ])

-- UTC Arbitrary instance taken from the test suite for the 'time' package:
instance Arbitrary Day where
    arbitrary = liftM ModifiedJulianDay $
       -- From the 'time' package:
       -- choose (-313698, 2973483) -- 1000-01-1 to 9999-12-31
       choose (14802, 233802) -- (900*365)+1000-01-1 to (1500*365)+1000-01-1

instance Arbitrary DiffTime where
    arbitrary = oneof [intSecs, fracSecs] -- up to 1 leap second
        where intSecs = liftM secondsToDiffTime' $ choose (0, 86400)
              fracSecs = liftM picosecondsToDiffTime' $ choose (0, 86400 * 10^12)
              secondsToDiffTime' :: Integer -> DiffTime
              secondsToDiffTime' = fromInteger
              picosecondsToDiffTime' :: Integer -> DiffTime
              picosecondsToDiffTime' x = fromRational (x % 10^12)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary

-- POSIXTime is an alias for NominalDiffTime:
instance Arbitrary NominalDiffTime where
   arbitrary = utcTimeToPOSIXSeconds <$> arbitrary



runGet :: Get a -> ByteString -> Either String a
runGet = runGetWithNoLeftover










-- TODO: I would LOVE to get rid of this whole thing:
timestampEq :: Timestamp -> Timestamp -> Bool
timestampEq (Timestamp time0) (Timestamp time1) =
   abs (time0 - time1) < 0.000000001

datumEq :: OSCDatum -> OSCDatum -> Bool
datumEq a b = case (a, b) of
   (OSC_T t0, OSC_T t1) -> timestampEq t0 t1
   _ -> a == b

oscEq :: OSC -> OSC -> Bool
oscEq (OSC p0 args0) (OSC p1 args1) =
   (p0 == p1) && argsMatch args0 args1
 where
   argsMatch [] [] = True
   argsMatch [] (_:_) = False
   argsMatch (_:_) [] = False
   argsMatch (d0:rest0) (d1:rest1) =
      datumEq d0 d1 && argsMatch rest0 rest1

bundleEq :: OSCBundle -> OSCBundle -> Bool
bundleEq (OSCBundle t0 msgs0) (OSCBundle t1 msgs1) =
   timestampEq t0 t1 && msgsMatch msgs0 msgs1
 where
   msgsMatch [] [] = True
   msgsMatch [] (_:_) = False
   msgsMatch (_:_) [] = False
   msgsMatch (Right msg0:rest0) (Right msg1:rest1) =
      oscEq msg0 msg1 && msgsMatch rest0 rest1
   msgsMatch (Left msg0:rest0) (Left msg1:rest1) =
      (msg0 == msg1) && msgsMatch rest0 rest1
   -- This is the case where one is 'Left' and one is 'Right'.
   -- We could hypothetically test if they're equal.
   msgsMatch (_:_rest0) (_:_rest1) = False
{-
   msgsMatch (Left l:rest0) (Right r:rest1) =
      rightMatchesLeft l r
   msgsMatch (Right r:rest0) (Left l:rest1) =
      rightMatchesLeft l r
-}

utcEq :: UTCTime -> UTCTime -> Bool
utcEq a b =
   abs (diffUTCTime a b) < 0.00001

posixEq :: POSIXTime -> POSIXTime -> Bool
posixEq a b =
   abs (a - b) < 0.00001
