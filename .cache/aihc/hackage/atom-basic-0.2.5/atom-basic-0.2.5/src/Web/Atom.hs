-- | @atom-basic@ lets you generate Atom Feeds and Atom Entries. It provides
-- the 'Feed' and 'Entry' types for the respective Atom document. This module is
-- intended to be imported qualified to avoid name clashes:
--
-- > import qualified Web.Atom as Atom
--
-- XML generation is not built in because there are several Haskell XML
-- libraries that you might want to use depending on your circumstances. To
-- allow for this, you need to provide an 'XMLGen' record to the 'feedXML' or
-- 'entryXML' functions. An 'XMLGen' record contains functions that generate
-- XML of the type you prefer.
-- /Thanks to <https:\/\/ocharles.org.uk\/blog\/ Ollie Charles> for this suggestion./
--
-- A minimal example using the <http://hackage.haskell.org/package/xml xml> package looks like this (<https://github.com/cbaatz/atom-basic/blob/master/examples/atom-xml/Main.hs GitHub>):
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import qualified Data.Text      as T
-- > import           Data.Time      (UTCTime (..), fromGregorian)
-- > import           Text.XML.Light
-- > import qualified Web.Atom       as Atom
-- >
-- > feed :: Atom.Feed Element
-- > feed = Atom.makeFeed
-- >     (Atom.unsafeURI "https://haskell.org/")
-- >     (Atom.TextHTML "The <em>Title</em>")
-- >     (UTCTime (fromGregorian 2015 7 8) 0)
-- >
-- > xmlgen :: Atom.XMLGen Element Content QName Attr
-- > xmlgen = Atom.XMLGen
-- >     { Atom.xmlElem     = \n as ns    -> Element n as ns Nothing
-- >     , Atom.xmlName     = \nsMay name -> QName (T.unpack name)
-- >                                           (fmap T.unpack nsMay) Nothing
-- >     , Atom.xmlAttr     = \k v        -> Attr k (T.unpack v)
-- >     , Atom.xmlTextNode = \t          -> Text $ CData CDataText (T.unpack t) Nothing
-- >     , Atom.xmlElemNode = Elem
-- >     }
-- >
-- > main = putStr $ ppTopElement $ Atom.feedXML xmlgen feed
--
-- Another example that uses the
-- <http://hackage.haskell.org/package/xml-conduit xml-conduit> package instead
-- is also available
-- <https://github.com/cbaatz/atom-basic/blob/master/examples/atom-xml-conduit/Main.hs in the GitHub repository>.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Web.Atom
    ( makeFeed
    , makeEntry
    , feedXML
    , entryXML
    , XMLGen (..)
    , Feed(..)
    , Entry(..)
    , Source(..)
    , Content(..)
    , Category(..)
    , Generator(..)
    , Person(..)
    , Email(..)
    , Rel(..)
    , Text(..)
    , Link(..)
    , LanguageTag(..)
    , MediaType(..)
    , UTCTime
    , unsafeURI
    , URI(..)
    ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import           Data.Maybe             (catMaybes)
import           Data.String            (IsString (..))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T (decodeUtf8)
import           Data.Time              (UTCTime, formatTime)
import           Data.Time.Format       (defaultTimeLocale)
import           Network.URI            (URI (..), parseURI)

-- ------------------------
-- Convenience constructors
-- ------------------------

-- | Convenience constructor with defaults for all non-required fields.
makeFeed :: URI     -- ^ Feed ID
         -> Text e  -- ^ Feed Title
         -> UTCTime -- ^ Updated timestamp
         -> Feed e
makeFeed uri title updated = Feed
    { feedId           = uri
    , feedTitle        = title
    , feedUpdated      = updated
    , feedSubtitle     = Nothing
    , feedIcon         = Nothing
    , feedLogo         = Nothing
    , feedRights       = Nothing
    , feedGenerator    = Nothing
    , feedAuthors      = []
    , feedContributors = []
    , feedCategories   = []
    , feedLinks        = []
    , feedEntries      = []
    }

-- | Convenience constructor with defaults for all non-required fields.
makeEntry :: URI    -- ^ Entry ID
         -> Text e  -- ^ Entry Title
         -> UTCTime -- ^ Updated timestamp
         -> Entry e
makeEntry uri title updated = Entry
    { entryId           = uri
    , entryTitle        = title
    , entryUpdated      = updated
    , entryPublished    = Nothing
    , entrySummary      = Nothing
    , entryContent      = Nothing
    , entryRights       = Nothing
    , entrySource       = Nothing
    , entryAuthors      = []
    , entryContributors = []
    , entryCategories   = []
    , entryLinks        = []
    }

-- | Convenience function to create a URIs from hardcoded strings. /This
-- function is partial so only use this if you're hardcoding the URI string and
-- you're sure that it's valid./
unsafeURI :: String -> URI
unsafeURI s = case parseURI s of
    Just uri-> uri
    Nothing -> error $ "Unable to parse URI string: " ++ s

-- -------------------------
-- External XML construction
-- -------------------------

-- | Generate an XML value from a 'Feed'.
feedXML :: XMLGen e node name attr -> Feed e -> e
feedXML xmlgen feed = toXML xmlgen "feed" feed

-- | Generate an XML value from an 'Entry'.
entryXML :: XMLGen e node name attr -> Entry e -> e
entryXML xmlgen entry = toXML xmlgen "entry" entry

-- | This record defines what kind of XML we should construct. A valid
-- definition of this record must be provided to the 'feedXML' and 'entryXML'
-- functions. This lets users use the XML library of their choice for the Atom
-- feed XML. A couple of concrete examples are provided at the top of this
-- page. Here's an example that uses the
-- <http://hackage.haskell.org/package/xml-conduit xml-conduit> package:
--
-- > xmlgen :: Atom.XMLGen Element Node Name (Name, T.Text)
-- > xmlgen = Atom.XMLGen
-- >     { Atom.xmlElem     = \n as ns    -> Element n (fromList as) ns
-- >     , Atom.xmlName     = \nsMay name -> Name name nsMay Nothing
-- >     , Atom.xmlAttr     = \k v        -> (k, v)
-- >     , Atom.xmlTextNode = NodeContent
-- >     , Atom.xmlElemNode = NodeElement
-- >     }
data XMLGen elem node name attr = XMLGen { xmlElem     :: name -> [attr] ->
[node] -> elem
    -- ^ Create element from name, attributes, and nodes/contents.
    , xmlName     :: Maybe T.Text -> T.Text -> name
    -- ^ Create qualified name from optional namespace and name.
    , xmlAttr     :: name -> T.Text -> attr
    -- ^ Create attribute from qualified name and text value.
    , xmlTextNode :: T.Text -> node
    -- ^ Create text node/content from text value.
    , xmlElemNode :: elem -> node
    -- ^ Create element node/content from element.
    }

-- ----------
-- Atom Types
-- ----------

-- | Langauge tag as per <https://tools.ietf.org/html/rfc3066>.
data LanguageTag = LanguageTag T.Text deriving (Show, Eq)

-- | An email address. @xsd:string { pattern = ".+@.+" }@
data Email = Email T.Text deriving (Show, Eq)

-- | A media type. @xsd:string { pattern = ".+/.+" }@
data MediaType = MediaType ByteString deriving (Show, Eq)

-- | @rel@ attribute for link elements as per
-- <https://tools.ietf.org/html/rfc4287#section-4.2.7.2>.
data Rel = RelText T.Text | RelURI URI deriving (Eq)

instance Show Rel where
    show (RelText t) = T.unpack t
    show (RelURI u)  = show u

instance IsString MediaType where
    fromString s = MediaType (BC.pack s)

-- | Human readable text as per
-- <https://tools.ietf.org/html/rfc4287#section-3.1>.
data Text e = TextPlain T.Text
            | TextHTML  T.Text
            | TextXHTML e
            deriving (Show, Eq)

instance IsString (Text e) where
    fromString s = TextPlain (T.pack s)

-- | Content or link to content of an Atom entry as per
-- <https://tools.ietf.org/html/rfc4287#section-4.1.3>.
data Content e = InlinePlainContent  T.Text
               | InlineHTMLContent   T.Text
               | InlineXHTMLContent  e
               | InlineXMLContent    e          (Maybe MediaType)
               | InlineTextContent   T.Text     (Maybe MediaType)
               | InlineBase64Content ByteString (Maybe MediaType)
               | OutOfLineContent    URI        (Maybe MediaType)
               deriving (Show, Eq)

-- | Describes a person as per
-- <https://tools.ietf.org/html/rfc4287#section-3.2>.
data Person = Person
    { personName  :: T.Text
    , personURI   :: Maybe URI
    , personEmail :: Maybe Email
    } deriving (Show, Eq)

-- | Identifies the agent used to generate the feed, for debugging and other
-- purposes as per <https://tools.ietf.org/html/rfc4287#section-4.2.4>.
data Generator = Generator
    { generatorName :: T.Text
    , generatorURI  :: Maybe URI
    , version       :: Maybe T.Text
    } deriving (Show, Eq)

-- | Information about a feed or entry category as per
-- <https://tools.ietf.org/html/rfc4287#section-4.2.2>.
data Category = Category
    { categoryTerm   :: T.Text
    , categoryScheme :: Maybe URI
    , categoryLabel  :: Maybe T.Text
    } deriving (Show, Eq)

-- | Defines a reference to a web resource as per
-- <https://tools.ietf.org/html/rfc4287#section-4.2.7>.
data Link = Link
    { linkHref     :: URI
    , linkRel      :: Maybe Rel
    , linkType     :: Maybe MediaType
    , linkHrefLang :: Maybe LanguageTag
    , linkTitle    :: Maybe T.Text
    , linkLength   :: Maybe Integer
    } deriving (Show, Eq)

-- | Top-level element for an Atom Feed Document as per
-- <https://tools.ietf.org/html/rfc4287#section-4.1.1>.
data Feed e = Feed
    { feedId           :: URI
    , feedTitle        :: Text e
    , feedUpdated      :: UTCTime
    , feedSubtitle     :: Maybe (Text e)
    , feedIcon         :: Maybe URI
    , feedLogo         :: Maybe URI
    , feedRights       :: Maybe (Text e)
    , feedGenerator    :: Maybe Generator
    , feedAuthors      :: [Person] -- At least one unless all child entries have at least one author
    , feedContributors :: [Person]
    , feedCategories   :: [Category]
    , feedLinks        :: [Link]
    , feedEntries      :: [Entry e]
    } deriving (Show, Eq)

-- | If an Atom entry is copied into a different feed, 'Source' can be used to
-- preserve the metadata of the original feed as per
-- <https://tools.ietf.org/html/rfc4287#section-4.2.11>.
data Source e = Source
    { sourceId           :: Maybe URI
    , sourceTitle        :: Maybe (Text e)
    , sourceUpdated      :: Maybe UTCTime
    , sourceSubtitle     :: Maybe (Text e)
    , sourceIcon         :: Maybe URI
    , sourceLogo         :: Maybe URI
    , sourceRights       :: Maybe (Text e)
    , sourceGenerator    :: Maybe Generator
    , sourceAuthors      :: [Person] -- At least one unless all child entries have at least one author
    , sourceContributors :: [Person]
    , sourceCategories   :: [Category]
    , sourceLinks        :: [Link]
    } deriving (Show, Eq)

-- | An individual Atom entry that can be used either as a child of 'Feed' or
-- as the top-level element of a stand-alone Atom Entry Document as per
-- <https://tools.ietf.org/html/rfc4287#section-4.1.2>.
data Entry e = Entry
    { entryId           :: URI
    , entryTitle        :: (Text e)
    , entryUpdated      :: UTCTime
    , entryPublished    :: Maybe UTCTime
    , entrySummary      :: Maybe (Text e)
    , entryContent      :: Maybe (Content e)
    , entryRights       :: Maybe (Text e)
    , entrySource       :: Maybe (Source e)
    , entryAuthors      :: [Person] -- At least one unless source child contains author or parent contains author
    , entryContributors :: [Person]
    , entryCategories   :: [Category]
    , entryLinks        :: [Link] -- Required if no content element
    } deriving (Show, Eq)

-- -------------------------
-- Internal helper functions
-- -------------------------

textShow :: (Show a) => a -> T.Text
textShow = T.pack . show

atomNS :: Maybe T.Text
atomNS = Just "http://www.w3.org/2005/Atom"

media :: XMLGen e n m a -> MediaType -> a
media XMLGen{..} (MediaType m) = xmlAttr (xmlName Nothing "type") (T.decodeUtf8 m)

attr :: XMLGen e n m a -> T.Text -> T.Text -> a
attr XMLGen{..} name value = xmlAttr (xmlName Nothing name) value

-- -------------------------
-- Internal XML construction
-- -------------------------

-- | Type class for recursively generating the XML value from a Feed or Entry.
-- This is an internal function used by 'feedXML' and 'entryXML'.
class ToXML e b where
    toXML :: XMLGen e n m a -> T.Text -> b -> e

instance ToXML e (Feed e) where
    toXML g@XMLGen{..} tag Feed{..} = xmlElem (xmlName atomNS tag) [] $
      map xmlElemNode (
          [ toXML g "id"      feedId
          , toXML g "title"   feedTitle
          , toXML g "updated" feedUpdated
          ]
          ++ catMaybes
          [ fmap (toXML g "subtitle")  feedSubtitle
          , fmap (toXML g "icon")      feedIcon
          , fmap (toXML g "logo")      feedLogo
          , fmap (toXML g "rights")    feedRights
          , fmap (toXML g "generator") feedGenerator
          ]
          ++ map (toXML g "author")      feedAuthors
          ++ map (toXML g "contributor") feedContributors
          ++ map (toXML g "category")    feedCategories
          ++ map (toXML g "link")        feedLinks
          ++ map (toXML g "entry")       feedEntries
          )

instance ToXML e (Entry e) where
    toXML g@XMLGen{..} tag Entry{..} = xmlElem (xmlName atomNS tag) [] $
      map xmlElemNode (
          [ toXML g "id"      entryId
          , toXML g "title"   entryTitle
          , toXML g "updated" entryUpdated
          ]
          ++ catMaybes
          [ fmap (toXML g "published") entryPublished
          , fmap (toXML g "summary")   entrySummary
          , fmap (toXML g "content")   entryContent
          , fmap (toXML g "rights")    entryRights
          , fmap (toXML g "source")    entrySource
          ]
          ++ map (toXML g "author")      entryAuthors
          ++ map (toXML g "contributor") entryContributors
          ++ map (toXML g "category")    entryCategories
          ++ map (toXML g "link")        entryLinks
          )

instance ToXML e (Source e) where
    toXML g@XMLGen{..} tag Source{..} = xmlElem (xmlName atomNS tag) [] $
      map xmlElemNode (
          catMaybes
          [ fmap (toXML g "id")        sourceId
          , fmap (toXML g "title")     sourceTitle
          , fmap (toXML g "updated")   sourceUpdated
          , fmap (toXML g "subtitle")  sourceSubtitle
          , fmap (toXML g "icon")      sourceIcon
          , fmap (toXML g "logo")      sourceLogo
          , fmap (toXML g "rights")    sourceRights
          , fmap (toXML g "generator") sourceGenerator
          ]
          ++ map (toXML g "author")      sourceAuthors
          ++ map (toXML g "contributor") sourceContributors
          ++ map (toXML g "category")    sourceCategories
          ++ map (toXML g "link")        sourceLinks
          )

instance ToXML e (Text e) where
    toXML g@XMLGen{..} tag x = case x of
        TextPlain c -> el [attr g "type" "text"]  [xmlTextNode c]
        TextHTML  c -> el [attr g "type" "html"]  [xmlTextNode c]
        TextXHTML c -> el [attr g "type" "xhtml"] [xmlElemNode c]
      where el = xmlElem (xmlName atomNS tag)

instance ToXML e (Content e) where
    toXML g@XMLGen{..} tag x = case x of
        InlinePlainContent t          -> el [attr g "type" "text"]  [xmlTextNode t]
        InlineHTMLContent t           -> el [attr g "type" "html"]  [xmlTextNode t]
        InlineXHTMLContent xml        -> el [attr g "type" "xhtml"] [xmlElemNode xml]
        InlineTextContent t mmedia    -> el (mediaAttrs mmedia)     [xmlTextNode t]
        InlineXMLContent xml mmedia   -> el (mediaAttrs mmedia)     [xmlElemNode xml]
        InlineBase64Content bs mmedia -> el (mediaAttrs mmedia)     [xmlTextNode (T.decodeUtf8 $ B64.encode bs)]
        OutOfLineContent uri mmedia   -> el (mediaAttrs mmedia ++ [attr g "src" $ textShow uri]) []
      where el = xmlElem (xmlName atomNS tag)
            mediaAttrs = maybe [] (\m -> [media g m])

instance ToXML e Person where
    toXML XMLGen{..} tag (Person name mu me) = xmlElem (xmlName atomNS tag) [] $
      map xmlElemNode (catMaybes
            [ Just $ el "name" [] [xmlTextNode name]
            , fmap (\u -> el "uri" [] [xmlTextNode $ textShow u]) mu
            , fmap (\(Email e) -> el "email" [] [xmlTextNode e])  me
            ])
      where el n = xmlElem (xmlName atomNS n)

instance ToXML e Category where
    toXML g@XMLGen{..} tag (Category term mscheme mlabel) = el attrs []
      where el = xmlElem (xmlName atomNS tag)
            attrs = catMaybes
                [ Just (attr g "term" term)
                , fmap (attr g "scheme" . textShow) mscheme
                , fmap (attr g "label") mlabel
                ]

instance ToXML e Generator where
    toXML g@XMLGen{..} tag (Generator name muri mversion) = el attrs [xmlTextNode name]
      where el = xmlElem (xmlName atomNS tag)
            attrs = catMaybes
                [ fmap (attr g "uri" . textShow) muri
                , fmap (attr g "version")        mversion
                ]

instance ToXML e Link where
    toXML g@XMLGen{..} tag (Link href mrel mmedia mlang mtitle mlen) = el attrs []
      where el = xmlElem (xmlName atomNS tag)
            attrs = catMaybes
                [ Just . attr g "href" . textShow $ href
                , fmap (attr g "rel" . textShow)    mrel
                , fmap (media g)                    mmedia
                , fmap (\(LanguageTag lang) -> attr g "hreflang" lang) mlang
                , fmap (attr g "title")             mtitle
                , fmap (attr g "length" . textShow) mlen
                ]

instance ToXML e URI where
    toXML XMLGen{..} tag uri = xmlElem
        (xmlName atomNS tag)
        []
        [xmlTextNode (textShow uri)]

instance ToXML e UTCTime where
    toXML XMLGen{..} tag utc = xmlElem
        (xmlName atomNS tag)
        []
        [xmlTextNode (T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" utc)]
