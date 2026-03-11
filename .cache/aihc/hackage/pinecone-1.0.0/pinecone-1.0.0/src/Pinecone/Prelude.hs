module Pinecone.Prelude
    ( -- * JSON
      aesonOptions
    , labelModifier
    , stripPrefix

      -- * Re-exports
    , module Control.Applicative
    , module Data.Aeson
    , module Data.Aeson.Types
    , module Data.Map
    , module Data.Scientific
    , module Data.String
    , module Data.Text
    , module Data.Time.Clock.POSIX
    , module Data.Vector
    , module GHC.Generics
    , module Numeric.Natural
    , module Servant.API
    , module Web.HttpApiData
    ) where

import Control.Applicative (Alternative(..))
import Data.Aeson.Types (typeMismatch)
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Web.HttpApiData (ToHttpApiData(..))

import Data.Aeson
    ( genericToJSON
    , genericParseJSON
    , FromJSON(..)
    , Object
    , Options(..)
    , SumEncoding(..)
    , ToJSON(..)
    , Value(..)
    )
import Servant.API
    ( Capture
    , Delete
    , DeleteAccepted
    , Get
    , Header'
    , JSON
    , NoContent
    , Patch
    , Post
    , PostCreated
    , QueryParam
    , QueryParam'
    , QueryParams
    , ReqBody
    , Required
    , Strict
    , (:<|>)(..)
    , (:>)
    )

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Char as Char

dropTrailingUnderscore :: String -> String
dropTrailingUnderscore "_" = ""
dropTrailingUnderscore ""  = ""
dropTrailingUnderscore (c : cs) = c : dropTrailingUnderscore cs

labelModifier :: String -> String
labelModifier = map Char.toLower . dropTrailingUnderscore

stripPrefix :: String -> String -> String
stripPrefix prefix string = labelModifier suffix
  where
    suffix = case List.stripPrefix prefix string of
        Nothing -> string
        Just x  -> x

aesonOptions :: Options
aesonOptions = Aeson.defaultOptions
    { fieldLabelModifier = labelModifier
    , constructorTagModifier = labelModifier
    , omitNothingFields = True
    }
