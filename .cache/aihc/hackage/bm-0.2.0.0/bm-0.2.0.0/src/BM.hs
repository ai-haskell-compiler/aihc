------------------------------------------------------------------------------
-- |
-- Module      : BM
-- Description : API
-- Copyright   : Copyright (c) 2021-2023 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BM
  ( -- * Constants
    version
    -- * Types
  , Argument
  , Command
  , Error
  , Keyword
  , ParameterName
  , ParameterValue
  , Trace
  , Url
  , Config(..)
  , Bookmark(..)
  , Query(..)
  , Parameter(..)
  , Proc(..)
    -- * API
  , run
  , getCompletion
  ) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A
import Data.Aeson (FromJSON, (.:), (.:?), (.!=))
import qualified Data.Aeson.Types as AT

-- https://hackage.haskell.org/package/base
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import qualified System.Info

-- https://hackage.haskell.org/package/dlist
import qualified Data.DList as DList
import Data.DList (DList)

-- https://hackage.haskell.org/package/network-uri
import qualified Network.URI as URI

-- https://hackage.haskell.org/package/scientific
import qualified Data.Scientific as Sci

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Writer (Writer, runWriter, tell)

-- https://hackage.haskell.org/package/vector
import qualified Data.Vector as V
import Data.Vector (Vector)

-- (bm:cabal)
import qualified Paths_bm as Project

------------------------------------------------------------------------------
-- $Constants

-- | bm version string (\"@bm-haskell X.X.X.X@\")
--
-- @since 0.1.0.0
version :: String
version = "bm-haskell " ++ showVersion Project.version

------------------------------------------------------------------------------

-- | Default command, depending on the OS
defaultCommand :: Command
defaultCommand = case System.Info.os of
    "mingw32" -> "start"
    "darwin"  -> "open"
    _other    -> "xdg-open"

-- | Default query parameter name
defaultParameter :: ParameterName
defaultParameter = "q"

------------------------------------------------------------------------------
-- $Types
--
-- This implementation makes heavy use of the 'String' type.  Type aliases are
-- provided to make the API easier to read.

-- | CLI argument or process argument
--
-- @since 0.1.0.0
type Argument = String

-- | Process command
--
-- This command is executed with a single URL argument to open a
-- bookmark/query.
--
-- @since 0.1.0.0
type Command = FilePath

-- | Error message
--
-- @since 0.1.0.0
type Error = String

-- | Bookmark keyword
--
-- The configuration file defines a hierarchy of keywords that are matched
-- against CLI arguments to determine which bookmark/query to open.
--
-- @since 0.1.0.0
type Keyword = String

-- | Query parameter name
--
-- @since 0.1.0.0
type ParameterName = String

-- | Query parameter value
--
-- @since 0.1.0.0
type ParameterValue = String

-- | Trace line for debugging
--
-- @since 0.1.0.0
type Trace = String

-- | Bookmark or query action URL
--
-- @since 0.1.0.0
type Url = String

------------------------------------------------------------------------------

-- | Configuration
--
-- YAML attributes:
--
-- * @command@: top-level command (string, default depends on the OS)
-- * @args@: bookmarks (array of 'Bookmark')
--
-- Default commands:
--
-- * Linux: @xdg-open@
-- * Windows: @start@
-- * macOS: @open@
--
-- @since 0.1.0.0
data Config
  = Config
    { configCommand :: !Command
    , configArgs    :: !(Vector Bookmark)
    }
  deriving Show

instance FromJSON Config where
  parseJSON = A.withObject "Config" $ \o ->
    Config
      <$> o .:? "command" .!= defaultCommand
      <*> o .:  "args"

------------------------------------------------------------------------------

-- | Bookmark definition
--
-- YAML attributes:
--
-- * @keyword@: bookmark keyword (string)
-- * @command@: command for this bookmark and children (string, optional)
-- * @url@: bookmark URL (string, optional)
-- * @query@: bookmark query definition ('Query', optional)
-- * @args@: child bookmarks (array of 'Bookmark', optional)
--
-- A command be set to override the top-level command, but this is generally
-- not done.  If a bookmark is selected and there is no URL, the first child
-- is processed.  Only one of @query@ and @args@ may be present.
--
-- @since 0.1.0.0
data Bookmark
  = Bookmark
    { keyword     :: !Keyword
    , mCommand    :: !(Maybe Command)
    , mUrl        :: !(Maybe Url)
    , queryOrArgs :: !(Either Query (Vector Bookmark))
    }
  deriving Show

instance FromJSON Bookmark where
  parseJSON = A.withObject "Bookmark" $ \o -> do
    keyword  <- parseToString =<< o .: "keyword"
    mCommand <- o .:? "command"
    mUrl     <- o .:? "url"
    mQuery   <- o .:? "query"
    mArgs    <- o .:? "args"
    queryOrArgs <- case (mQuery, mArgs) of
      (Nothing,    Just args) -> pure $ Right args
      (Just query, Nothing)   -> pure $ Left query
      (Nothing,    Nothing)   -> pure $ Right V.empty
      (Just{},     Just{})    -> fail $
        "bookmark has both query and args: " ++ keyword
    pure Bookmark{..}

------------------------------------------------------------------------------

-- | Query definition
--
-- YAML attributes:
--
-- * @action@: URL (string)
-- * @parameter@: query parameter name (string, default: @q@)
-- * @hidden@: array of constant parameters ('Parameter')
--
-- @since 0.1.0.0
data Query
  = Query
    { action           :: !Url
    , parameter        :: !ParameterName
    , hiddenParameters :: !(Vector Parameter)
    }
  deriving Show

instance FromJSON Query where
  parseJSON = A.withObject "Query" $ \o ->
    Query
      <$> o .:  "action"
      <*> o .:? "parameter" .!= defaultParameter
      <*> o .:? "hidden"    .!= V.empty

------------------------------------------------------------------------------

-- | HTTP GET parameter definition
--
-- YAML attributes:
--
-- * @name@: parameter name
-- * @value@: constant parameter value
--
-- @since 0.1.0.0
data Parameter
  = Parameter
    { name  :: !ParameterName
    , value :: !ParameterValue
    }
  deriving Show

instance FromJSON Parameter where
  parseJSON = A.withObject "Parameter" $ \o ->
    Parameter
      <$> o .: "name"
      <*> (parseToString =<< o .: "value")

-- | Encode an HTTP GET parameter
--
-- Spaces are transformed to plus characters, and other reserved characters
-- are escaped.
encodeParameter :: Parameter -> String
encodeParameter Parameter{..} = encodePart name ++ "=" ++ encodePart value
  where
    encodePart :: String -> String
    encodePart
      = map (\c -> if c == ' ' then '+' else c)
      . URI.escapeURIString ((||) <$> URI.isUnreserved <*> (== ' '))

------------------------------------------------------------------------------

-- | Process specification
--
-- @since 0.1.0.0
data Proc
  = Proc
    { command   :: !Command
    , arguments :: ![Argument]
    }
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- $API

-- | Determine the process to execute for the given config and CLI arguments
--
-- @since 0.1.0.0
run
  :: Config
  -> [Argument]
  -> (Either Error Proc, [Trace])
run Config{..} cliArgs = fmap DList.toList . runWriter $ do
    trace $ formatCommand configCommand
    loop configCommand configArgs cliArgs
  where
    loop
      :: Command
      -> Vector Bookmark
      -> [Argument]
      -> Writer (DList Trace) (Either Error Proc)
    loop cmd bms (arg:args) = case V.find (isPrefixOf arg . keyword) bms of
      Just bm -> do
        trace $ formatBookmark bm
        case queryOrArgs bm of
          Left query
            | null args -> case mUrl bm of
                Just url -> openUrl (fromMaybe cmd $ mCommand bm) url
                Nothing -> returnError $ "no query for " ++ keyword bm
            | otherwise -> openQuery (fromMaybe cmd $ mCommand bm) query args
          Right bms'
            | null args -> case mUrl bm of
                Just url -> openUrl (fromMaybe cmd $ mCommand bm) url
                Nothing  -> case bms' V.!? 0 of
                  Just bm' ->
                    loop (fromMaybe cmd $ mCommand bm) bms' [keyword bm']
                  Nothing -> returnError $ "no URL for " ++ keyword bm
            | otherwise -> loop (fromMaybe cmd $ mCommand bm) bms' args
      Nothing -> returnError $ "unknown argument: " ++ arg
    loop _cmd _bms [] = returnError "no arguments"

    returnError :: Error -> Writer (DList Trace) (Either Error Proc)
    returnError = return . Left

    openUrl :: Command -> Url -> Writer (DList Trace) (Either Error Proc)
    openUrl cmd url = do
      trace $ unwords [cmd, url]
      return . Right $ Proc cmd [url]

    openQuery
      :: Command
      -> Query
      -> [Argument]
      -> Writer (DList Trace) (Either Error Proc)
    openQuery cmd Query{..} args
      = openUrl cmd
      . (action ++)
      . ('?' :)
      . intercalate "&"
      . map encodeParameter
      $ Parameter parameter (unwords args) : V.toList hiddenParameters

    trace :: Trace -> Writer (DList Trace) ()
    trace = tell . DList.singleton

    formatCommand :: Command -> Trace
    formatCommand = ('[' :) . (++ "]")

    formatKeyword :: Keyword -> Trace
    formatKeyword = ('<' :) . (++ ">")

    formatBookmark :: Bookmark -> Trace
    formatBookmark Bookmark{..} = case mCommand of
      Just command -> unwords [formatKeyword keyword, formatCommand command]
      Nothing      -> formatKeyword keyword

------------------------------------------------------------------------------

-- | Get CLI completion options
--
-- @since 0.1.0.0
getCompletion
  :: Config
  -> [Argument]  -- ^ current CLI arguments, last one being completed
  -> [Argument]  -- ^ completion options
getCompletion Config{..} = loop configArgs
  where
    loop :: Vector Bookmark -> [Argument] -> [Argument]
    loop bms [arg] = filter (isPrefixOf arg) . map keyword $ V.toList bms
    loop bms (arg:args) = case V.find (isPrefixOf arg . keyword) bms of
      Just bm -> case queryOrArgs bm of
        Left{} -> []
        Right bms' -> loop bms' args
      Nothing -> []
    loop _bms [] = []

------------------------------------------------------------------------------
-- $Internal

-- | Parse any scalar value as a string
--
-- Strings, numbers, booleans, and null are parsed as a string.  Arrays and
-- objects result in an error.
parseToString :: A.Value -> AT.Parser String
parseToString = \case
    (A.String t)  -> pure $ T.unpack t
    (A.Number n)  -> pure . either (show @Double) (show @Integer) $
      Sci.floatingOrInteger n
    (A.Bool b)    -> pure $ if b then "true" else "false"
    A.Null        -> pure "null"
    A.Array{}     -> fail "unexpected array"
    A.Object{}    -> fail "unexpected object"
