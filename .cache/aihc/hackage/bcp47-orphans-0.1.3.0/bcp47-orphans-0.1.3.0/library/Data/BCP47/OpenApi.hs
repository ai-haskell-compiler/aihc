{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.OpenApi () where

import Control.Lens ((&), (?~))
import Data.Aeson (Value (..))
import Data.BCP47
import Data.BCP47.Autodocodec ()
import Data.OpenApi
  ( NamedSchema (..)
  , OpenApiType (..)
  , ToParamSchema (..)
  , ToSchema (..)
  , description
  , example
  , format
  , toSchema
  , type_
  )
import Data.Proxy (Proxy (..))

instance ToSchema BCP47 where
  declareNamedSchema _ =
    pure
      $ NamedSchema (Just "BCP47")
      $ mempty
        & type_ ?~ OpenApiString
        & description ?~ "BCP47 language tag"
        & example ?~ String "en"
        & format ?~ "bcp47"

instance ToParamSchema BCP47 where
  toParamSchema _ = toSchema $ Proxy @BCP47
