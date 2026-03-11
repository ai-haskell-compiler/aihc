------------------------------------------------------------------------------
-- |
-- Module      : QueueSheet.Types
-- Description : queue sheet types
-- Copyright   : Copyright (c) 2020-2025 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module QueueSheet.Types
  ( -- * Name
    Name(..)
    -- * Url
  , Url(..)
    -- * Date
  , Date(..)
    -- * Section
  , Section(..)
  , defaultSection
    -- * Tag
  , Tag(..)
    -- * Item
  , Item(..)
    -- * Queue
  , Queue(..)
    -- * Import
  , Import(..)
    -- * ImportOrQueue
  , ImportOrQueue(..)
    -- * QueuesFile
  , QueuesFile(..)
    -- * QueueSheet
  , QueueSheet(..)
  ) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A
import Data.Aeson (FromJSON(parseJSON), (.:), (.:?), (.!=))
import qualified Data.Aeson.Types as AT

-- https://hackage.haskell.org/package/base
import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
#if !MIN_VERSION_base (4,11,0)
import Data.Monoid ((<>))
#endif

-- https://hackage.haskell.org/package/ginger
import qualified Text.Ginger as Ginger
import Text.Ginger ((~>))

-- https://hackage.haskell.org/package/scientific
import qualified Data.Scientific as Sci

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/vector
import qualified Data.Vector as V

------------------------------------------------------------------------------
-- $Name

-- | Name of a queue or queue item
--
-- @since 0.3.0.0
newtype Name = Name Text
  deriving newtype (Eq, Show)

instance FromJSON Name where
  parseJSON = fmap Name . parseToString

instance Ginger.ToGVal m Name where
  toGVal (Name t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Name where
  render (Name t) = TTC.fromT t

------------------------------------------------------------------------------
-- $Url

-- | URL of queue or queue item
--
-- @since 0.3.0.0
newtype Url = Url Text
  deriving newtype (Eq, Show)

instance FromJSON Url where
  parseJSON = fmap Url . parseToString

instance Ginger.ToGVal m Url where
  toGVal (Url t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Url where
  render (Url t) = TTC.fromT t

------------------------------------------------------------------------------
-- $Date

-- | Date of last queue update
--
-- @since 0.3.0.0
newtype Date = Date Text
  deriving newtype (Eq, Show)

instance FromJSON Date where
  parseJSON = fmap Date . parseToString

instance Ginger.ToGVal m Date where
  toGVal (Date t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Date where
  render (Date t) = TTC.fromT t

------------------------------------------------------------------------------
-- $Section

-- | Section used to organize queues
--
-- @since 0.3.0.0
newtype Section = Section Text
  deriving newtype (Eq, Show)

instance FromJSON Section where
  parseJSON = fmap Section . parseToString

instance Ginger.ToGVal m Section where
  toGVal (Section t) = Ginger.toGVal $ escapeTeX t

instance TTC.Render Section where
  render (Section t) = TTC.fromT t

-- | The default section is represented as an empty string
defaultSection :: Section
defaultSection = Section ""

------------------------------------------------------------------------------
-- $Tag

-- | Queue tag
--
-- @since 0.3.0.0
newtype Tag = Tag Text
  deriving newtype (Eq, Show)

instance FromJSON Tag where
  parseJSON = A.withText "Tag" $ \t -> do
      when (T.null t) $ fail "empty string"
      unless (T.all isValidChar t) $ fail ("invalid tag: " ++ T.unpack t)
      return $ Tag t
    where
      isValidChar :: Char -> Bool
      isValidChar c
        | isAsciiLower c = True
        | isAsciiUpper c = True
        | isDigit      c = True
        | otherwise      = c `elem` ("._-" :: String)

------------------------------------------------------------------------------
-- $Item

-- | Queue item
--
-- Whitespace-separated tags are supported /instead of/ CSV from @0.8.0.0@.
--
-- @since 0.5.0.0
data Item
  = Item
    { itemName :: !Name
    , itemUrl  :: !(Maybe Url)
    , itemTags :: ![Tag]
    }
  deriving (Eq, Show)

instance FromJSON Item where
  parseJSON = \case
    (A.Object o) -> do
      itemName <- o .:  "name"
      itemUrl  <- o .:? "url"
      itemTags <- maybe (pure []) parseSSV =<< (o .:? "tags")
      return Item{..}
    value -> do
      itemName <- Name <$> parseToString value
      let itemUrl  = Nothing
          itemTags = []
      return Item{..}

instance Ginger.ToGVal m Item where
  toGVal Item{..} = Ginger.dict $
    [ "name" ~> itemName
    , "url"  ~> itemUrl
    ] ++ [("tag_" <> tag) ~> True | Tag tag <- itemTags]

------------------------------------------------------------------------------
-- $Queue

-- | Queue information
--
-- Whitespace-separated items and tags are supported /instead of/ CSV from
-- @0.8.0.0@.
--
-- @since 0.5.0.0
data Queue
  = Queue
    { queueName    :: !Name
    , queueUrl     :: !(Maybe Url)
    , queueDate    :: !(Maybe Date)
    , queueSection :: !Section
    , queueTags    :: ![Tag]
    , queueItems   :: !(Maybe (Either Item [Item]))
    }
  deriving (Eq, Show)

instance FromJSON Queue where
  parseJSON = A.withObject "Queue" $ \o -> do
    queueName    <- o .:  "name"
    queueUrl     <- o .:? "url"
    queueDate    <- o .:? "date"
    queueSection <- o .:? "section" .!= defaultSection
    queueTags    <- maybe (pure []) parseSSV =<< (o .:? "tags")
    mPrevItem    <- o .:? "prev"
    mNextValue   <- o .:? "next"
    queueItems   <- case (mPrevItem, mNextValue) of
      (_,         Just nextValue) -> Just . Right <$> parseSSV nextValue
      (Just item, Nothing)        -> pure . Just $ Left item
      (Nothing,   Nothing)        -> pure Nothing
    return Queue{..}

------------------------------------------------------------------------------
-- $Import

-- | Import declaration
--
-- @since 0.3.0.0
data Import
  = Import
    { importPath    :: !FilePath
    , importSection :: !(Maybe Section)
    }
  deriving (Eq, Show)

instance FromJSON Import where
  parseJSON = A.withObject "Import" $ \o -> do
    importPath    <- o .:  "import"
    importSection <- o .:? "section"
    return Import{..}

------------------------------------------------------------------------------
-- $ImportOrQueue

-- | Import declaration or queue information
--
-- @since 0.3.0.0
data ImportOrQueue
  = IQImport !Import
  | IQQueue  !Queue
  deriving (Eq, Show)

instance FromJSON ImportOrQueue where
  parseJSON value =
    (IQImport <$> parseJSON value) <|> (IQQueue <$> parseJSON value)

------------------------------------------------------------------------------
-- $QueuesFile

-- | Queues file
--
-- @since 0.3.0.0
data QueuesFile
  = QueuesFile
    { qfSections       :: ![Section]
    , qfImportOrQueues :: ![ImportOrQueue]
    }
  deriving (Eq, Show)

instance FromJSON QueuesFile where
  parseJSON = \case
    (A.Object o) -> do
      qfSections <- (:) defaultSection <$> (o .:? "sections" .!= [])
      qfImportOrQueues <- o .: "queues"
      return QueuesFile{..}
    a@A.Array{} -> do
      let qfSections = [defaultSection]
      qfImportOrQueues <- parseJSON a
      return QueuesFile{..}
    A.String{} -> fail "unexpected string"
    A.Number{} -> fail "unexpected number"
    A.Bool{}   -> fail "unexpected bool"
    A.Null     -> fail "unexpected null"

------------------------------------------------------------------------------
-- $QueueSheet

-- | Queue sheet
--
-- @since 0.3.0.0
data QueueSheet
  = QueueSheet
    { qsSections :: ![Section]
    , qsQueues   :: ![Queue]
    }
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- $Internal

-- | Escape a string for inclusion in a TeX document
escapeTeX :: Text -> Text
escapeTeX = T.foldl go ""
  where
    go :: Text -> Char -> Text
    go acc = \case
      '#'  -> acc <> "\\#"
      '$'  -> acc <> "\\$"
      '%'  -> acc <> "\\%"
      '&'  -> acc <> "\\&"
      '\\' -> acc <> "\\textbackslash{}"
      '^'  -> acc <> "\\textasciicircum{}"
      '_'  -> acc <> "\\_"
      '{'  -> acc <> "\\{"
      '}'  -> acc <> "\\}"
      '~'  -> acc <> "\\textasciitilde{}"
      c    -> acc `T.snoc` c

-- | Parse an array or string in space-separated-value format
--
-- Strings are split on whitespace.
parseSSV :: A.FromJSON a => A.Value -> AT.Parser [a]
parseSSV = \case
    (A.String t)
      | T.null t  -> fail "empty string"
      | otherwise -> mapM (parseJSON . A.String) $ T.words t
    (A.Array v)   -> mapM parseJSON $ V.toList v
    A.Object{}    -> fail "unexpected object"
    value         -> (: []) <$> parseJSON value

-- | Parse any scalar value as a string
--
-- Strings, numbers, booleans, and null are parsed as a string.  Empty
-- strings, arrays, and objects result in an error.
parseToString :: A.Value -> AT.Parser Text
parseToString = \case
    (A.String t)
      | T.null t  -> fail "empty string"
      | otherwise -> pure t
    (A.Number n)  -> pure . T.pack . either (show @Double) (show @Integer) $
      Sci.floatingOrInteger n
    (A.Bool b)    -> pure $ if b then "true" else "false"
    A.Null        -> pure "null"
    A.Array{}     -> fail "unexpected array"
    A.Object{}    -> fail "unexpected object"
