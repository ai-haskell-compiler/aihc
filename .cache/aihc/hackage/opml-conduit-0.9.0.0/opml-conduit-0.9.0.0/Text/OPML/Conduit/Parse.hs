{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
-- | Streaming parser for the OPML 2.0 standard.
--
-- The parser tries to be as lenient as possible. All functions may throw an 'OpmlException'.
module Text.OPML.Conduit.Parse
  ( -- * Parsers
    parseOpml
  , parseOpmlHead
  , parseOpmlOutline
    -- * Exceptions
  , OpmlException(..)
  ) where

-- {{{ Imports
import           Conduit                      hiding (throwM)
import           Control.Applicative          hiding (many)
import           Control.Exception.Safe       as Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.CaseInsensitive         hiding (map)
import           Data.Either
import           Data.List.NonEmpty           (NonEmpty, nonEmpty)
import           Data.Maybe
import           Data.Monoid
import           Data.Monoid.Textual          hiding (map)
import           Data.Text                    as Text (Text, null, strip, unpack)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC822
import           Data.Tree
import           Data.Version
import           Data.XML.Types
import           Lens.Micro
import           Lens.Micro.TH
import           Numeric
import           Prelude                      hiding (last)
import           Refined                      hiding (NonEmpty)
import           Text.OPML.Types
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.XML.Stream.Parse

import           URI.ByteString
-- }}}

data OpmlException = MissingText
                   | InvalidBool Text
                   | InvalidDecimal Text
                   | InvalidTime Text
                   | InvalidURI URIParseError
                   | InvalidVersion Text

deriving instance Eq OpmlException
deriving instance Show OpmlException

instance Exception OpmlException where
  displayException MissingText        = "An outline is missing the 'text' attribute."
  displayException (InvalidBool t)    = "Invalid boolean: " ++ unpack t
  displayException (InvalidDecimal t) = "Invalid decimal: " ++ unpack t
  displayException (InvalidURI e)     = "Invalid URI: " ++ show e
  displayException (InvalidTime t)    = "Invalid time: " ++ unpack t
  displayException (InvalidVersion t) = "Invalid version: " ++ unpack t

asURI :: (MonadThrow m) => Text -> m URI
asURI t = either (throwM . InvalidURI) return . parseURI laxURIParserOptions $ encodeUtf8 t

asVersion :: MonadThrow m => Text -> m Version
asVersion v = case filter (Prelude.null . snd) . readP_to_S parseVersion $ unpack v of
  [(a, "")] -> return a
  _         -> throwM $ InvalidVersion v

asDecimal :: (MonadThrow m, Integral a) => Text -> m a
asDecimal t = case filter (Prelude.null . snd) . readSigned readDec $ unpack t of
  (result, _):_ -> return result
  _             -> throwM $ InvalidDecimal t

asExpansionState :: (MonadThrow m, Integral a) => Text -> m [a]
asExpansionState t = mapM asDecimal . filter (not . Text.null) . map strip $ split (== ',') t

asTime :: (MonadThrow m) => Text -> m UTCTime
asTime t = maybe (throwM $ InvalidTime t) (return . zonedTimeToUTC) $ parseTimeRFC822 t

-- The standard only accepts "true", and "false",
-- but it doesn't hurt to be more lenient
asBool :: (MonadThrow m) => Text -> m Bool
asBool t
  | mk t == "true" = return True
  | mk t == "false" = return False
  | otherwise = throwM $ InvalidBool t

asCategories :: Text -> [NonEmpty (Refined (Not Null) Text)]
asCategories = mapMaybe (nonEmpty . rights . map refine . split (== '/')) . split (== ',')

dateTag :: (MonadThrow m) => NameMatcher a -> ConduitM Event o m (Maybe UTCTime)
dateTag name = tagIgnoreAttrs name $ content >>= asTime

uriTag :: (MonadThrow m) => NameMatcher a -> ConduitM Event o m (Maybe URI)
uriTag name = tagIgnoreAttrs name $ content >>= asURI

expansionStateTag :: (MonadThrow m, Integral a) => ConduitM Event o m (Maybe [a])
expansionStateTag = tagIgnoreAttrs "expansionState" $ content >>= asExpansionState

textTag :: (MonadThrow m) => NameMatcher a -> ConduitM Event o m (Maybe Text)
textTag name = tagIgnoreAttrs name content

decimalTag :: (Integral i, MonadThrow m) => NameMatcher a -> ConduitM Event o m (Maybe i)
decimalTag name = tagIgnoreAttrs name $ content >>= asDecimal

projectC :: Monad m => Traversal' a b -> ConduitT a b m ()
projectC prism = fix $ \recurse -> do
  item <- await
  case (item, item ^? (_Just . prism)) of
    (_, Just a) -> yield a >> recurse
    (Just _, _) -> recurse
    _           -> return ()


data HeadPiece = HeadCreated { __headCreated :: UTCTime }
               | HeadModified { __headModified :: UTCTime }
               | HeadDocs { __headDocs :: URI }
               | HeadExpansionState { __headExpansionState :: [Int] }
               | HeadOwnerEmail { __headOwnerEmail :: Text }
               | HeadOwnerId { __headOwnerId :: URI }
               | HeadOwnerName { __headOwnerName :: Text }
               | HeadTitle { __headTitle :: Text }
               | HeadVertScrollState { __headVertScrollState :: Int }
               | HeadWindowBottom { __headWindowBottom :: Int }
               | HeadWindowLeft { __headWindowLeft :: Int }
               | HeadWindowRight { __headWindowRight :: Int }
               | HeadWindowTop { __headWindowTop :: Int }

makeLenses ''HeadPiece


-- | Parse the @\<head\>@ section.
-- This function is more lenient than what the standard demands on the following points:
--
-- - each sub-element may be repeated, in which case only the first occurrence is taken into account;
-- - each unknown sub-element is ignored.
parseOpmlHead :: (MonadCatch m) => ConduitM Event o m (Maybe OpmlHead)
parseOpmlHead = tagIgnoreAttrs "head" $ (manyYield' (choose piece) <* many ignoreAnyTreeContent) .| zipConduit where
  zipConduit = getZipConduit $ OpmlHead
    <$> ZipConduit (projectC _headTitle .| headDefC mempty)
    <*> ZipConduit (projectC _headCreated .| headC)
    <*> ZipConduit (projectC _headModified .| headC)
    <*> ZipConduit (projectC _headOwnerName .| headDefC mempty)
    <*> ZipConduit (projectC _headOwnerEmail .| headDefC mempty)
    <*> ZipConduit (projectC _headOwnerId .| headC)
    <*> ZipConduit (projectC _headDocs .| headC)
    <*> ZipConduit (projectC _headExpansionState .| concatC .| sinkList)
    <*> ZipConduit (projectC _headVertScrollState .| headC)
    <*> ZipConduit (projectC _headWindowBottom .| headC)
    <*> ZipConduit (projectC _headWindowLeft .| headC)
    <*> ZipConduit (projectC _headWindowRight .| headC)
    <*> ZipConduit (projectC _headWindowTop .| headC)
  piece = [ fmap HeadCreated <$> dateTag "dateCreated"
          , fmap HeadModified <$> dateTag "dateModified"
          , fmap HeadDocs <$> uriTag "docs"
          , fmap HeadExpansionState <$> expansionStateTag
          , fmap HeadOwnerEmail <$> textTag "ownerEmail"
          , fmap HeadOwnerId <$> uriTag "ownerId"
          , fmap HeadOwnerName <$> textTag "ownerName"
          , fmap HeadTitle <$> textTag "title"
          , fmap HeadVertScrollState <$> decimalTag "vertScrollState"
          , fmap HeadWindowBottom <$> decimalTag "windowBottom"
          , fmap HeadWindowLeft <$> decimalTag "windowLeft"
          , fmap HeadWindowRight <$> decimalTag "windowRight"
          , fmap HeadWindowTop <$> decimalTag "windowTop"
          ]


-- | Parse an @\<outline\>@ section.
-- The value of type attributes are not case-sensitive, that is @type=\"LINK\"@ has the same meaning as @type="link"@.
parseOpmlOutline :: (MonadCatch m) => ConduitM Event o m (Maybe (Tree OpmlOutline))
parseOpmlOutline = tag' "outline" attributes handler where
  attributes = do
    otype <- optional $ requireAttr "type"
    case mk <$> otype of
      Just "include" -> (,,,) otype <$> baseAttr <*> pure Nothing <*> (Just <$> linkAttr) <* ignoreAttrs
      Just "link"    -> (,,,) otype <$> baseAttr <*> pure Nothing <*> (Just <$> linkAttr) <* ignoreAttrs
      Just "rss"     -> (,,,) otype <$> baseAttr <*> (Just <$> subscriptionAttr) <*> pure Nothing <* ignoreAttrs
      _              -> (,,,) otype <$> baseAttr <*> pure Nothing <*> pure Nothing <* ignoreAttrs
  baseAttr = (,,,,) <$> (requireAttr "text" >>= refineThrow)
                    <*> optional (requireAttr "isComment" >>= asBool)
                    <*> optional (requireAttr "isBreakpoint" >>= asBool)
                    <*> optional (requireAttr "created" >>= asTime)
                    <*> optional (asCategories <$> requireAttr "category")
  linkAttr = requireAttr "url"
  subscriptionAttr = (,,,,,) <$> (requireAttr "xmlUrl" >>= asURI)
                             <*> optional (requireAttr "htmlUrl" >>= asURI)
                             <*> optional (requireAttr "description")
                             <*> optional (requireAttr "language")
                             <*> optional (requireAttr "title")
                             <*> optional (requireAttr "version")
  handler (_, b, Just s, _) = Node <$> (OpmlOutlineSubscription <$> baseHandler b <*> pure (subscriptionHandler s)) <*> pure []
  handler (_, b, _, Just l) = Node <$> (OpmlOutlineLink <$> baseHandler b <*> asURI l) <*> pure []
  handler (otype, b, _, _) = Node <$> (OpmlOutlineGeneric <$> baseHandler b <*> pure (fromMaybe mempty otype))
                                  <*> (manyYield' parseOpmlOutline .| sinkList)
  baseHandler (txt, comment, breakpoint, created, category) = return $ OutlineBase txt comment breakpoint created (fromMaybe mempty category)
  subscriptionHandler (uri, html, desc, lang, title, version) = OutlineSubscription uri html (fromMaybe mempty desc) (fromMaybe mempty lang) (fromMaybe mempty title) (fromMaybe mempty version)


data OpmlDocPiece = DocHead { __docHead :: OpmlHead }
                  | DocBody { __docBody :: [Tree OpmlOutline] }

makeLenses ''OpmlDocPiece


-- | Parse the top-level @\<opml\>@ element.
parseOpml :: (MonadCatch m) => ConduitM Event o m (Maybe Opml)
parseOpml = tag' "opml" attributes handler where
  attributes = (requireAttr "version" >>= asVersion) <* ignoreAttrs
  handler version = (manyYield' (choose piece) <* many ignoreAnyTreeContent) .| zipConduit version
  zipConduit version = getZipConduit $ Opml version
    <$> ZipConduit (projectC _docHead .| headDefC mkOpmlHead)
    <*> ZipConduit (projectC _docBody .| headDefC mempty)
  parseOpmlBody = tagIgnoreAttrs "body" $ manyYield' parseOpmlOutline .| sinkList
  piece = [ fmap DocHead <$> parseOpmlHead
          , fmap DocBody <$> parseOpmlBody
          ]
