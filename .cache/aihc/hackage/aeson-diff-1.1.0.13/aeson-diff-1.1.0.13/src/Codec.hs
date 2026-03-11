module Codec (encode, decode, ForceFormat (..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy as BL
import           Control.Applicative ((<|>))
import           Data.Char           (toLower)
import           Data.List           (isSuffixOf)

data ForceFormat = ForceYaml | AutodetectFormat deriving (Show, Eq)

isYamlPath :: [Char] -> Bool
isYamlPath fn =
     ".yaml" `isSuffixOf` map toLower fn
  || ".yml"  `isSuffixOf` map toLower fn

isJsonPath :: [Char] -> Bool
isJsonPath fn =
     ".json" `isSuffixOf` map toLower fn

decode :: Aeson.FromJSON a => ForceFormat -> Maybe FilePath -> BL.ByteString -> Maybe a
decode ForceYaml        _                         = decodeYamlFirst
decode AutodetectFormat (Just fn) | isYamlPath fn = decodeYamlFirst
decode AutodetectFormat (Just fn) | isJsonPath fn = Aeson.decode
decode AutodetectFormat _                         = decodeJsonFirst


decodeYamlFirst :: Aeson.FromJSON a => BL.ByteString -> Maybe a
decodeYamlFirst s = Yaml.decodeThrow (BL.toStrict s) <|> Aeson.decode s

decodeJsonFirst :: Aeson.FromJSON a => BL.ByteString -> Maybe a
decodeJsonFirst s = Aeson.decode s <|> Yaml.decodeThrow (BL.toStrict s)

encode :: Aeson.ToJSON a => ForceFormat -> Maybe FilePath -> a -> BL.ByteString
encode ForceYaml        _                         = BL.fromStrict . Yaml.encode
encode AutodetectFormat (Just fn) | isYamlPath fn = BL.fromStrict . Yaml.encode
encode AutodetectFormat _                         = Aeson.encode
