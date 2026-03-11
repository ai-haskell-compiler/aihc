{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | XML streaming parsers for the __Dublin Core Metadata Element Set__.
--
-- This module is meant to be imported qualified, like this:
--
-- > import qualified Text.XML.DublinCore.Conduit.Parse as DC
module Text.XML.DublinCore.Conduit.Parse (
  -- * Elements
  elementContributor,
  elementCoverage,
  elementCreator,
  elementDate,
  elementDescription,
  elementFormat,
  elementIdentifier,
  elementLanguage,
  elementPublisher,
  elementRelation,
  elementRights,
  elementSource,
  elementSubject,
  elementTitle,
  elementType,

  -- * Misc
  ParsingException (..),
) where

-- {{{ Imports
import Text.XML.DublinCore

import Conduit
import Control.Applicative
import Control.Exception.Safe as Exception
import Data.Text
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime
import Data.Time.RFC2822
import Data.Time.RFC3339
import Data.Time.RFC822
import Data.XML.Types
import GHC.Generics
import Text.XML.Stream.Parse

-- }}}

-- {{{ Utils
asDate :: MonadThrow m => Text -> m UTCTime
asDate text =
  maybe (throw $ InvalidTime text) (return . zonedTimeToUTC) $
    parseTimeRFC3339 text <|> parseTimeRFC2822 text <|> parseTimeRFC822 text <|> parseDateISO8601 text
 where
  parseDateISO8601 = iso8601ParseM . unpack

dcName :: Text -> Name
dcName string = Name string (Just "http://purl.org/dc/elements/1.1/") (Just namespacePrefix)

dcTagIgnoreAttrs :: MonadThrow m => Text -> ConduitM Event o m a -> ConduitM Event o m (Maybe a)
dcTagIgnoreAttrs name = tagIgnoreAttrs (matching (== dcName name))

-- }}}

newtype ParsingException = InvalidTime Text deriving (Eq, Generic, Ord, Show)

instance Exception ParsingException where
  displayException (InvalidTime t) = "Invalid time: " ++ unpack t

-- | Parse a @\<dc:contributor\>@ element.
elementContributor :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementContributor = dcTagIgnoreAttrs "contributor" content

-- | Parse a @\<dc:coverage\>@ element.
elementCoverage :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementCoverage = dcTagIgnoreAttrs "coverage" content

-- | Parse a @\<dc:creator\>@ element.
elementCreator :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementCreator = dcTagIgnoreAttrs "creator" content

-- | Parse a @\<dc:date\>@ element.
--
-- Throws 'InvalidTime' in case date is malformatted.
elementDate :: MonadThrow m => ConduitM Event o m (Maybe UTCTime)
elementDate = dcTagIgnoreAttrs "date" $ content >>= asDate

-- | Parse a @\<dc:description\>@ element.
elementDescription :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementDescription = dcTagIgnoreAttrs "description" content

-- | Parse a @\<dc:format\>@ element.
elementFormat :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementFormat = dcTagIgnoreAttrs "format" content

-- | Parse a @\<dc:identifier\>@ element.
elementIdentifier :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementIdentifier = dcTagIgnoreAttrs "identifier" content

-- | Parse a @\<dc:language\>@ element.
elementLanguage :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementLanguage = dcTagIgnoreAttrs "language" content

-- | Parse a @\<dc:publisher\>@ element.
elementPublisher :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementPublisher = dcTagIgnoreAttrs "publisher" content

-- | Parse a @\<dc:relation\>@ element.
elementRelation :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementRelation = dcTagIgnoreAttrs "relation" content

-- | Parse a @\<dc:rights\>@ element.
elementRights :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementRights = dcTagIgnoreAttrs "rights" content

-- | Parse a @\<dc:source\>@ element.
elementSource :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementSource = dcTagIgnoreAttrs "source" content

-- | Parse a @\<dc:subject\>@ element.
elementSubject :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementSubject = dcTagIgnoreAttrs "subject" content

-- | Parse a @\<dc:title\>@ element.
elementTitle :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementTitle = dcTagIgnoreAttrs "title" content

-- | Parse a @\<dc:type\>@ element.
elementType :: MonadThrow m => ConduitM Event o m (Maybe Text)
elementType = dcTagIgnoreAttrs "type" content
