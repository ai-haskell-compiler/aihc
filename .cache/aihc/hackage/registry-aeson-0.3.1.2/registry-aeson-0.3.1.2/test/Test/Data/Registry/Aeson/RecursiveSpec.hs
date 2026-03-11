{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.RecursiveSpec where

import Data.Aeson.Encoding.Internal
import Data.Registry
import Data.Registry.Aeson.Encoder
import Protolude hiding (list)
import Test.Data.Registry.Aeson.DataTypes
import Test.Tasty.Hedgehogx hiding (int, list)

{-
  This module shows how to implement an encoder for a recursive data type.
  This could be supported by the makeEncoder function but it is not supported at the moment
-}
test_encode_recursive = test "encode a recursive data type" $ do
  encodeByteString (make @(Encoder Path) encoders) path1 === "[[[1],[2]],3]"

-- * HELPERS

encoders :: Registry _ _
encoders =
  fun pathEncoder
    <: jsonEncoder @Int

pathEncoder :: Encoder Int -> Encoder Path
pathEncoder intEncoder = do
  let thisEncoder = Encoder $ \case
        File n ->
          encode intEncoder n
        Directory paths -> do
          let (vs, es) = unzip (encode thisEncoder <$> paths)
          (array vs, list identity es)
  thisEncoder
