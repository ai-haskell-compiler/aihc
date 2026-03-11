-- | Open Sound Control data

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE
     LambdaCase
   , OverloadedStrings
   , ViewPatterns
   , ScopedTypeVariables

   , NoRebindableSyntax
   , NoIncoherentInstances
   , NoMonomorphismRestriction
   , NoUndecidableInstances
   #-}

module Vivid.OSC (
     OSC(..)
   , OSCDatum(..)

   , encodeOSC
   , decodeOSC

   , Timestamp(..)
   , OSCBundle(..)

   , encodeOSCBundle
   , decodeOSCBundle

   , encodeTimestamp
   , utcToTimestamp
   , timestampToUTC
   , timestampFromUTC
   , timestampToPOSIX
   , timestampFromPOSIX

   , addSecs
   , diffTimestamps

   , encodeOSCDatum

   -- , decodeOSCDatumWithPadding
   -- , decodeOSCData

   -- Testing/internals:
   , putOSC
   , putOSCString
   , putOSCDatum
   , getOSCString
   , getOSC
   , putOSCBlob
   , getOSCBlob
   , getOSCTimestamp
   , putOSCTimestamp

   , putOSCBundle
   , getOSCBundle
   , runGetWithNoLeftover

   -- Only need for testing old<->new
   , alignTo4'
   ) where

-- For GHC 7.8 and older
-- (Eventually remove):
import Control.Applicative

-- import Control.DeepSeq (NFData, rnf)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8 (unpack)
import Data.Fixed
import Data.Int (Int32)
import Data.Monoid
import Data.Serialize hiding (encode, decode, runGet)
-- import Data.Serialize.IEEE754
import qualified Data.Serialize.Get as Get
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime, diffUTCTime, addUTCTime)
import Data.Time.Clock.POSIX
import Data.Word

-- | An OSC message, e.g.
-- 
--   > OSC "/n_free" [OSC_I 42]
data OSC
   = OSC ByteString [OSCDatum]
 deriving (Show, Read, Eq, Ord)

data OSCDatum
   = OSC_I Int32
   | OSC_S ByteString
   | OSC_F Float
   | OSC_D Double -- ^ This isn't a base type in the OSC standard but the
                  -- scsynth response message from "/status" uses it...
{-
   | OSC_I8 Int8
   | OSC_I16 Int16
-}
   | OSC_B ByteString
   | OSC_T Timestamp -- ^ From the OSC 1.1 spec
 deriving (Show, Read, Eq, Ord)

-- | This is stored as the number of seconds since Jan 1 1900. You can get
--   it with 'Vivid.Actions.Class.getTime'
data Timestamp = Timestamp Double -- Pico --  Word32 Word32 -- Double
   deriving (Show, Read, Eq, Ord)

-- | TODO: a Bundle can also contain other bundles, recursively
data OSCBundle
   = OSCBundle Timestamp [Either ByteString OSC]
 deriving (Show, Read, Eq)

toTypeChar :: OSCDatum -> ByteString
toTypeChar = \case
   OSC_I _ -> "i"
   OSC_S _ -> "s"
   OSC_F _ -> "f"
   OSC_B _ -> "b"
   OSC_D _ -> "d"
   OSC_T _ -> "t"

-- formerly known as 'someShit':
-- TODO: if sometimes encoding can fail (e.g. if a string contains a '\NUL'), we
-- should really have a (Either String) situation (and same for 'encodeOSCDatum'):
encodeOSC :: OSC -> ByteString
encodeOSC o = runPut (putOSC o)

putOSC :: OSC -> Put
putOSC (OSC path args) = do
   putOSCString path
   putOSCString $ "," <> BS.concat (map toTypeChar args)
   mapM_ putOSCDatum args

encodeOSCDatum :: OSCDatum -> ByteString
encodeOSCDatum = runPut . putOSCDatum

putOSCDatum :: OSCDatum -> Put
putOSCDatum = \case
   OSC_S s -> putOSCString s
   OSC_I i -> putInt32be i
   OSC_F f -> putFloat32be f
   OSC_D d -> putFloat64be d
   OSC_T t -> putOSCTimestamp t
   OSC_B b -> putOSCBlob b

putOSCString :: ByteString -> Put
putOSCString s = do
    -- There must be at least one \NUL byte:
   putByteString $ padTo4Bytes (s<>"\NUL")

getOSCString :: Get ByteString
getOSCString = do
   -- There may be a more efficient way to do this:
   first <- BS.takeWhile (/= 0) <$> getByteString 4
   case BS.length first of
      4 -> do
         (first <>) <$> getOSCString
      -- Note we've already discarded the ending '\NUL' padding:
      _ -> pure first

-- | Add '\NUL' characters to the end of a ByteString until its length is
--   a multiple of 4
-- TODO: make this be a 'Put' and just get like the 'seek' count so we don't have to traverse it twice:
padTo4Bytes :: ByteString -> ByteString
padTo4Bytes b = b <> BS.replicate (alignTo4' (BS.length b)) 0

putOSCBlob :: ByteString -> Put
putOSCBlob bs = do
   -- Note: should probably really be word:
   putInt32be $ toEnum $ BS.length bs
   -- The only reason we don't account for the prefixed length is that's
   -- always 4 bytes long so it won't affect the padding:
   putByteString $ padTo4Bytes bs

getOSCBlob :: Get ByteString
getOSCBlob = do
   size <- fromEnum <$> getInt32be
   b <- getByteString size
   _ <- getByteString (alignTo4' size) -- TODO; 'seek' instead?
   pure b

decodeOSC :: ByteString -> Either String OSC
decodeOSC = runGet getOSC

getOSC :: Get OSC
getOSC = do
   path <- getOSCString
   -- For example, ",issifbt":
   (comma:typeDesc) <- BS8.unpack <$> getOSCString
   when (comma /= ',') $ fail "Malformed OSC!"
   values <- forM typeDesc $ \case
      's' -> OSC_S <$> getOSCString
      'i' -> OSC_I <$> getInt32be
      'f' -> OSC_F <$> getFloat32be
      't' -> OSC_T <$> getOSCTimestamp
      'd' -> OSC_D <$> getFloat64be
      'b' -> OSC_B <$> getOSCBlob
      c -> fail $ "Unrecognized character: " ++ show c
   pure (OSC path values)

encodeOSCBundle :: OSCBundle -> ByteString
encodeOSCBundle b = runPut (putOSCBundle b)

putOSCBundle :: OSCBundle -> Put
putOSCBundle (OSCBundle time messages) = do
   putByteString "#bundle\NUL"
   putOSCTimestamp time
   forM_ messages $ \message -> do
      let encoded = case message of
           Left b -> b
           -- TODO: Doesn't feel right that i have a 'runPut' inside a 'Put':
           Right osc -> runPut $ putOSC osc
      putWord32be $ toEnum $ BS.length encoded
      putByteString encoded

decodeOSCBundle :: ByteString -> Either String OSCBundle
decodeOSCBundle = runGet getOSCBundle

getOSCBundle :: Get OSCBundle
getOSCBundle = do
   let prefix = "#bundle\NUL"
   pre <- getByteString (BS.length prefix)
   when (pre /= prefix) $
      fail "Wrong header"
   ts <- getOSCTimestamp
   messages <- getListTillEnd $ do
      len <- fromEnum <$> getWord32be
      Right <$> isolate len getOSC
   pure $ OSCBundle ts messages

-- There might be a more efficient way to write this?:
getListTillEnd :: Get x -> Get [x]
getListTillEnd getAction = isEmpty >>= \case
   True -> pure []
   False -> do
      r <- getAction
      (r:) <$> getListTillEnd getAction

encodeTimestamp :: Timestamp -> ByteString
encodeTimestamp t = runPut (putOSCTimestamp t)

putOSCTimestamp :: Timestamp -> Put
putOSCTimestamp (Timestamp {- secs secFraction -} t) = do
   -- putWord32be secs
   -- putWord32be secFraction
   putWord64be $ round $ t * 2^(32::Int)

getOSCTimestamp :: Get Timestamp
getOSCTimestamp = do
   w <- {- realToFrac -} fromIntegral <$> getWord64be
   pure $ Timestamp $ w / 2 ** 32 -- (2^(32::Int))
   -- secs <- getWord32be
   -- secFraction <- getWord32be
   -- pure $ Timestamp secs secFraction

{-# DEPRECATED utcToTimestamp "renamed to 'timestampFromUTC'" #-}
utcToTimestamp :: UTCTime -> Timestamp
utcToTimestamp utcTime = timestampFromUTC utcTime

-- TODO: I'd like to do these 4 functions in terms of POSIX instead of UTCTime:

timestampToUTC :: Timestamp -> UTCTime
timestampToUTC (Timestamp ts) =
   -- posixSecondsToUTCTime . timestampToPOSIX
   addUTCTime (realToFrac ts) startOfTheCentury

timestampFromUTC :: UTCTime -> Timestamp
timestampFromUTC utcTime =
   -- timestampFromPOSIX . utcTimeToPOSIXSeconds
   Timestamp . realToFrac $ diffUTCTime utcTime startOfTheCentury

startOfTheCentury :: UTCTime
startOfTheCentury =
   UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)

timestampToPOSIX :: Timestamp -> POSIXTime
timestampToPOSIX =
   utcTimeToPOSIXSeconds . timestampToUTC

timestampFromPOSIX :: POSIXTime -> Timestamp
timestampFromPOSIX =
   timestampFromUTC . posixSecondsToUTCTime



addSecs :: Timestamp -> Double -> Timestamp
addSecs (Timestamp t) secs = Timestamp (t + secs)

diffTimestamps :: Timestamp -> Timestamp -> Double
diffTimestamps (Timestamp t1) (Timestamp t0) = t1 - t0

{-
instance NFData OSCDatum where
   rnf (OSC_I x) = rnf x
   rnf (OSC_F x) = rnf x
   rnf (OSC_S x) = rnf x
{-
   rnf (OSC_I8 x) = rnf x
   rnf (OSC_I16 x) = rnf x
-}
   rnf (OSC_B x) = rnf x
-}

-- Makes sure we've consumed all input:
runGet :: Get a -> ByteString -> Either String a
runGet g = Get.runGet $ do
   x <- g
   isEmpty >>= \case
      True -> pure x
      False -> fail $ "Leftover bytes #2"

-- With an exportable name:
runGetWithNoLeftover :: Get a -> ByteString -> Either String a
runGetWithNoLeftover = runGet

-- Everything in OSC has to be a multiple of 4 bytes, so this is handy:
alignTo4' :: Integral i => i -> i
alignTo4' n = (4 - (n `rem` 4)) `mod` 4

