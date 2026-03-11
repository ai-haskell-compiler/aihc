module Test.Data.Radius.Iso (
  tests,

  isoAttribute',
  isoPacket,
  isoAttributeText,
  isoAttributeString,
  isoAttributeInteger,
  -- isoAttributeIpV4,
  ) where

import Test.Data.Radius.Arbitraries ()
import Test.Data.Radius.IsoBase (isoAttribute', isoPacket)

import Test.QuickCheck.Simple (Test, qcTest)

import Control.Applicative ((<$>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.Put (Put, runPut)

import Data.Radius.Scalar (Bin128, AtText, AtString, AtInteger, AtIpV4)
import Data.Radius.Packet (Code, Header)
import Data.Radius.Attribute (Attribute', Attribute, TypedNumberSets)
import qualified Data.Radius.StreamGet as Get
import Data.Radius.StreamPut (AttributePutM)
import qualified Data.Radius.StreamPut as Put


isoCode :: Code -> Bool
isoCode c =
  runGet Get.code (runPut $ Put.code c)
  ==
  Right c

isoBin128 :: Bin128 -> Bool
isoBin128 b =
  runGet Get.bin128 (runPut $ Put.bin128 b)
  ==
  Right b

isoHeader :: Header -> Bool
isoHeader h =
  runGet Get.header (runPut $ Put.header h)
  ==
  Right h


isoAtText :: AtText -> Bool
isoAtText v =
  runGet (Get.atText $ BS.length bs) bs
  ==
  Right v
  where bs = runPut $ Put.atText v

isoAtString :: AtString -> Bool
isoAtString v =
  runGet (Get.atString $ BS.length bs) bs
  ==
  Right v
  where bs = runPut $ Put.atString v

isoAtInteger :: AtInteger -> Bool
isoAtInteger v =
  runGet Get.atInteger (runPut $ Put.atInteger v)
  ==
  Right v

isoAtIpV4 :: AtIpV4 -> Bool
isoAtIpV4 v =
  runGet Get.atIpV4 (runPut $ Put.atIpV4 v)
  ==
  Right v

putAttribute :: (v -> ByteString -> Put) -> AttributePutM v a -> Put
putAttribute vPut = mapM_ (Put.attribute' vPut) . Put.extractAttributes

isoAttributeText :: (Ord v, TypedNumberSets v)
                 => Get (Attribute' v)
                 -> (v -> ByteString -> Put)
                 -> Attribute v AtText
                 -> Bool
isoAttributeText vGet vPut at =
  ( (runMaybeT . Get.decodeAsText <$>) . runGet (Get.attribute' vGet) . runPut . putAttribute vPut $ Put.attribute at )
  ==
  Right (Right (Just at))

isoAttributeString :: (Ord v, TypedNumberSets v)
                   => Get (Attribute' v)
                   -> (v -> ByteString -> Put)
                   -> Attribute v AtString
                   -> Bool
isoAttributeString vGet vPut at =
  ( (runMaybeT . Get.decodeAsString <$>) . runGet (Get.attribute' vGet) . runPut . putAttribute vPut $ Put.attribute at )
  ==
  Right (Right (Just at))

isoAttributeInteger :: (Ord v, TypedNumberSets v)
                    => Get (Attribute' v)
                    -> (v -> ByteString -> Put)
                    -> Attribute v AtInteger
                    -> Bool
isoAttributeInteger vGet vPut at =
  ( (runMaybeT . Get.decodeAsInteger <$>) . runGet (Get.attribute' vGet) . runPut . putAttribute vPut $ Put.attribute at )
  ==
  Right (Right (Just at))

{-
isoAttributeIpV4 vGet vPut at =
  ( (runMaybeT . Get.decodeAsIpV4 <$>) . runGet (Get.attribute' vGet) . runPut . putAttribute vPut $ Put.attribute at )
  ==
  Right (Right (Just at))
 -}

tests :: [Test]
tests =
  [ qcTest "iso - code"               isoCode
  , qcTest "iso - bin128"             isoBin128
  , qcTest "iso - header"             isoHeader

  , qcTest "iso - atText"             isoAtText
  , qcTest "iso - atString"           isoAtString
  , qcTest "iso - atInteger"          isoAtInteger
  , qcTest "iso - atIpV4"             isoAtIpV4
  ]
