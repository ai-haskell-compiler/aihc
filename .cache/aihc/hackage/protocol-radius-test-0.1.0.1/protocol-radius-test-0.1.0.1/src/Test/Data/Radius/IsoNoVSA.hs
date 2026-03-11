module Test.Data.Radius.IsoNoVSA (
  tests,

  -- isoAttribute',
  -- isoPacket,
  -- isoAttributeText,
  -- isoAttributeString,
  -- isoAttributeInteger,
  -- isoAttributeIpV4,
  ) where

import Test.Data.Radius.ArbitrariesNoVSA (EmptyVSA)
import qualified Test.Data.Radius.IsoBase as Base
-- (isoAttribute', isoPacket)

import Test.QuickCheck.Simple (Test, qcTest)

import Control.Applicative ((<$>), pure)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.Put (Put, runPut)

import Data.Radius.Scalar (AtText, AtString)
import Data.Radius.Packet (Packet)
import Data.Radius.Attribute (Attribute', Attribute)
import qualified Data.Radius.StreamGet as Get
import Data.Radius.StreamPut (AttributePutM)
import qualified Data.Radius.StreamPut as Put


getEmpty :: Get (Attribute' EmptyVSA)
getEmpty = fail "Vendor Specific Result type is bottom."

putEmpty ::  EmptyVSA -> ByteString -> Put
putEmpty _ _ = pure ()


putAttribute :: AttributePutM EmptyVSA a -> Put
putAttribute = mapM_ (Put.attribute' putEmpty) . Put.extractAttributes

isoAttributeText :: Attribute EmptyVSA AtText
                 -> Bool
isoAttributeText at =
  ( (runMaybeT . Get.decodeAsText <$>) . runGet (Get.attribute' getEmpty) . runPut . putAttribute $ Put.attribute at )
  ==
  Right (Right (Just at))

isoAttributeString :: Attribute EmptyVSA AtString
                   -> Bool
isoAttributeString at =
  ( (runMaybeT . Get.decodeAsString <$>) . runGet (Get.attribute' getEmpty) . runPut . putAttribute $ Put.attribute at )
  ==
  Right (Right (Just at))

{-
isoAttributeInteger :: Attribute EmptyVSA AtInteger
                    -> Bool
isoAttributeInteger at =
  ( (runMaybeT . Get.decodeAsInteger <$>) . runGet (Get.attribute' getEmpty) . runPut . putAttribute $ Put.attribute at )
  ==
  Right (Right (Just at))
 -}

{-
isoAttributeIpV4 vGet vPut at =
  ( (runMaybeT . Get.decodeAsIpV4 <$>) . runGet (Get.attribute' vGet) . runPut . putAttribute vPut $ Put.attribute at )
  ==
  Right (Right (Just at))
 -}

isoAttribute' :: Attribute' EmptyVSA -> Bool
isoAttribute' = Base.isoAttribute' getEmpty putEmpty

isoPacket :: Packet [Attribute' EmptyVSA] -> Bool
isoPacket = Base.isoPacket getEmpty putEmpty

tests :: [Test]
tests =
  [ qcTest "iso - atText"             isoAttributeText
  , qcTest "iso - atString"           isoAttributeString
  --- , qcTest "iso - atInteger"          isoAttributeInteger
  --- , qcTest "iso - atIpV4"             isoAtIpV4

  , qcTest "iso - attribute'"         isoAttribute'
  , qcTest "iso - packet"             isoPacket
  ]
