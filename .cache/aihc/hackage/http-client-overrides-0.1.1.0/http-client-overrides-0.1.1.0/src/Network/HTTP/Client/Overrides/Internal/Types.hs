{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Client.Overrides.Internal.Types
    ( ConfigFile(..)
    , Config(..)
    , LogOptions(..)
    , LogFormat(..)
    , RequestOverride(..)
    , URL(..)
    , parseURL
    ) where

import Data.Char (toLower)
import Data.Either (either)
import Data.List (intercalate)
import Data.Monoid ((<>))
import GHC.Generics
import Text.Read (readMaybe)

import Data.Aeson (FromJSON, Value(..), (.:), (.:?), (.!=), parseJSON, withObject, withText)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (URI)
import qualified Network.URI as URI

--------
-- Types
--------

-- | The configuration file is versioned so that it can be changed in the
-- future and the old format gradually deprecated. The parsers uses the
-- @version@ field to decide how to parse the file.
data ConfigFile
    = V1 Config
    deriving (Show, Eq, Generic)

instance FromJSON ConfigFile where
    parseJSON = withObject "ConfigFile" $ \o -> do
        version <- o .: "version"
        case version of
            "v1" -> V1 <$> parseJSON (Object o)
            _    -> fail $ "Version '" <> version <> "' not supported"

-- | This type is used when the 'ConfigFile' version is @v1@.
data Config = Config
    { logOptions       :: !LogOptions
    -- ^ Options for logging HTTP requests and responses
    , requestOverrides :: ![RequestOverride]
    -- ^ Overrides to apply to HTTP requests
    } deriving (Show, Eq, Generic)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> Config
        <$> o .:? "logOptions" .!= LogOptions Nothing Nothing Nothing
        <*> o .:? "requestOverrides" .!= []

data LogOptions = LogOptions
    { logResponses        :: !(Maybe LogFormat)
    -- ^ Log HTTP responses
    , logRequests         :: !(Maybe LogFormat)
    -- ^ Log HTTP requests
    , logRequestOverrides :: !(Maybe LogFormat)
    -- ^ Log any overridden HTTP requests
    } deriving (Show, Eq, Generic)

instance FromJSON LogOptions where
    parseJSON = Aeson.genericParseJSON opts
      where
        opts = Aeson.defaultOptions { Aeson.fieldLabelModifier = toLowerHead . drop 3 }
        toLowerHead [] = []
        toLowerHead (x:xs) = (toLower x):xs

data LogFormat
    = Simple
    -- ^ Simple log format (single-line)
    | Detailed
    -- ^ Detailed log format
    deriving (Show, Eq, Generic)

instance FromJSON LogFormat where
    parseJSON = withText "LogFormat" $ \t -> case T.toLower t of
        "simple"   -> pure $ Simple
        "detailed" -> pure $ Detailed
        _          -> fail $ "Unknown LogFormat: " <> T.unpack t

data RequestOverride = RequestOverride
    { match    :: !URL
    -- ^ Match HTTP requests according to this URL
    , override :: !URL
    -- ^ Override HTTP reuests using this URL
    } deriving (Eq, Generic)

instance Show RequestOverride where
    show x = intercalate "\n" $
        [ "RequestOverride {"
        , "  match ="
        ]
        ++
        (map ("    " ++) . lines . show $ match x)
        ++
        [ "  override ="
        ]
        ++
        (map ("    " ++) . lines . show $ override x)
        ++
        [ "}"
        ]

instance FromJSON RequestOverride

data URL = URL
    { secure      :: !(Maybe Bool)
    , host        :: !(Maybe BS.ByteString)
    , port        :: !(Maybe Int)
    , path        :: !(Maybe BS.ByteString)
    } deriving (Eq, Generic)

instance Show URL where
    show x = intercalate "\n" $
        [ "URL {"
        , "  secure = " ++ show (secure x)
        , "  host   = " ++ show (host x)
        , "  port   = " ++ show (port x)
        , "  path   = " ++ show (path x)
        , "}"
        ]

instance FromJSON URL where
    parseJSON = withText "URL" $ either fail return . parseURL

--------------
-- URL parsing
--------------

parseURL :: Text -> Either String URL
parseURL url
    | "http://"  `T.isPrefixOf` url = toURL (Just False) url
    | "https://" `T.isPrefixOf` url = toURL (Just True) url
    | "://" `T.isInfixOf` url = Left $ "Unspported scheme: " ++ T.unpack url
    | otherwise = toURL Nothing $ "https://" <> url

toURL :: Maybe Bool -> Text -> Either String URL
toURL https url = do 
    uri <- case URI.parseURIReference $ T.unpack url of
        Nothing -> Left $ "Failed to parse URL: " ++ T.unpack url
        Just x  -> Right x
    p <- getPort uri
    return $ URL https (getHost uri) p (getPath uri)

getHost :: URI -> Maybe BS.ByteString
getHost uri = do
    authority <- URI.uriAuthority uri
    case URI.uriRegName authority of
        "" -> Nothing
        x  -> Just $ BS.pack x

getPort :: URI -> Either String (Maybe Int)
getPort uri =
    case fmap URI.uriPort $ URI.uriAuthority uri of
        Just "" ->
            Right Nothing
        Just ":" ->
            Right Nothing
        Just (':':x) ->
            case readMaybe x of
                Nothing -> Left $ "Failed to parse Int: " ++ show x 
                Just p  -> Right $ Just p
        _ ->
            Right Nothing

getPath :: URI -> Maybe BS.ByteString
getPath uri = case URI.uriPath uri of
    "" -> Nothing
    x  -> Just $ BS.pack x
