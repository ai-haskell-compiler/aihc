{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.Examples.Protocols where

import Data.Aeson hiding (encode)
import Data.Registry
import Data.Registry.Aeson.Decoder
import Protolude hiding (Product)
import Test.Data.Registry.Aeson.Examples.Deliverix

-- | This protocol is defined for the current data model
currentProtocol :: Registry _ _
currentProtocol =
  $(makeDecoder ''Delivery)
    <: $(makeDecoder ''Order)
    <: decodeMapOf @Int @Product
    <: decodeMaybeOf @Coupon
    <: $(makeDecoder ''Coupon)
    <: $(makeDecoder ''Product)
    <: $(makeDecoder ''Customer)
    <: $(makeDecoder ''Address)
    <: $(makeDecoder ''StreetNumber)
    <: $(makeDecoder ''Price)
    <: jsonDecoder @Text
    <: decodeKey @Int parseInt
    <: decodeMaybeOf @Double
    <: jsonDecoder @Double
    <: val (defaultOptions {unwrapUnaryRecords = True}) -- to unwrap StreetNumber
    <: defaultDecoderOptions

-- | The first version of the protocol did not have coupons
--   And street numbers were implemented as an Int (but street numbers can also have letters)
--   In this registry we specify how existing decoders need to be modified / extended in order to be
--   able to still read old values
protocolV1 :: Registry _ _
protocolV1 =
       val (defaultOptions {omitNothingFields = True}) -- to allow for missing coupons
    <: fun streetNumberDecoder
    <: jsonDecoder @Int
    <: currentProtocol

-- | A street number used to be a single Int
streetNumberDecoder :: Decoder Int -> Decoder StreetNumber
streetNumberDecoder = fmap (StreetNumber . show)

-- | Read map keys that are Ints
parseInt :: Text -> Either Text Int
parseInt t = maybe (Left $ "the key " <> t <> " is not an Int") Right (readMaybe t)
