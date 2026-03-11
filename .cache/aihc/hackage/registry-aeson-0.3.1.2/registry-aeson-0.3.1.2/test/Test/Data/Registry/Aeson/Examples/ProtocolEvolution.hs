{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
   This specification shows an example of an evolving data model
   where we must still be able to read old serialized data.

   The data model describes a delivery service with customers and orders.

   The current version of the data model:
     - adds a new feature: coupons
     - fixes a modelling issue: street numbers

   Our protocol must still be able to accept values which were serialized with the previous data model

-}
module Test.Data.Registry.Aeson.Examples.ProtocolEvolution where

import Data.Registry
import Data.Registry.Aeson.Decoder
import Data.String.QQ
import Protolude
import Test.Data.Registry.Aeson.Examples.Deliverix
import Test.Data.Registry.Aeson.Examples.Protocols
import Test.Tasty.Hedgehogx

test_current_protocol = test "values created for the current protocol can be decoded" $ do
  let decoder = make @(Decoder Delivery) currentProtocol
  checkOk $
    decodeByteString
      decoder
      [s|
    {"customer":
      {"customerName":"eric",
       "customerAddress": {
        "number":"21",
        "street":"Jump St.",
        "city": "LA"
       }
      },
     "order": {
       "price":100.0,
       "coupon": "TenPercentOff",
       "items": {
        "1": "Pizza"
      }
     }
   }
  |]

test_protocol_v1 = test "values created for the old protocol can also be decoded" $ do
  let decoder = make @(Decoder Delivery) protocolV1
  checkOk $
    decodeByteString
      decoder
      [s|
    {"customer":
      {"customerName":"eric",
       "customerAddress": {
        "number":21,
        "street":"Jump St.",
        "city": "LA"
       }
      },
     "order": {
       "price":100,
       "items": {
        "1": "Pizza"
      }
     }
   }
  |]

-- * HELPERS

checkOk :: Either Text a -> PropertyT IO ()
checkOk (Left e) = withFrozenCallStack $ annotateShow e >> failure
checkOk (Right _) = success
