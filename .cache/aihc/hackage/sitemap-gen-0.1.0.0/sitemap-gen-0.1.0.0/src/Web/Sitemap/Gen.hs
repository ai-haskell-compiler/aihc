{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-| The @Web.Sitemap.Gen@ module contains types & rendering functions
to generate XML compliant with the sitemaps.org specification.

For more information see https://www.sitemaps.org/protocol.html

-}
module Web.Sitemap.Gen
    ( -- * Sitemaps
      Sitemap(..)
    , renderSitemap
    , SitemapUrl(..)
    , renderSitemapUrl
    , ChangeFrequency(..)
    , renderChangeFrequency
      -- * Sitemap Indexes
    , SitemapIndex(..)
    , renderSitemapIndex
    , IndexEntry(..)
    , renderIndexEntry
      -- * Utilities
    , sitemapNamespace
    , formatSitemapTime
    , renderLastModified
    )
where

import           Data.Maybe                     ( catMaybes )
import           Data.Time                      ( UTCTime
                                                , formatTime
                                                , defaultTimeLocale
                                                )
import           GHC.Generics                   ( Generic )
import           Text.XML.Generator             ( Xml
                                                , Elem
                                                , XmlOutput
                                                , Namespace
                                                , xrender
                                                , doc
                                                , defaultDocInfo
                                                , xelemQ
                                                , namespace
                                                , xelems
                                                , xelemWithText
                                                , xtext
                                                , xelem
                                                )

import qualified Data.Text                     as T


-- SITEMAPS


-- | A 'Sitemap' contains multiple 'SitemapUrl' elements which describe
-- crawlable locations for search engines.
newtype Sitemap =
    Sitemap
        { sitemapUrls :: [SitemapUrl]
        } deriving (Show, Read, Eq, Generic)

-- | Render a Sitemap into a output format supported by the @xmlgen@ package.
--
-- In most cases you will want to generate a @ByteString@.
renderSitemap :: XmlOutput x => Sitemap -> x
renderSitemap sitemap =
    xrender
        $ doc defaultDocInfo
        $ xelemQ sitemapNamespace "urlset"
        $ xelems
        $ map renderSitemapUrl
        $ sitemapUrls sitemap

-- | A 'SitemapUrl' describes a single URL in a 'Sitemap'.
data SitemapUrl =
    SitemapUrl
        { sitemapLocation :: T.Text
        -- ^ The full URL of the page, including the protocol and
        -- domain name.
        , sitemapLastModified :: Maybe UTCTime
        -- ^ The time the page's content was last changed.
        , sitemapChangeFrequency :: Maybe ChangeFrequency
        -- ^ How often does the content at the URL change?
        , sitemapPriority :: Maybe Double
        -- ^ The relative priority of this URL compared to other URLs in
        -- the sitemap.
        } deriving (Show, Read, Eq, Generic)

-- | Render a 'SitemapUrl' as a @url@ XML element.
renderSitemapUrl :: SitemapUrl -> Xml Elem
renderSitemapUrl url = xelem "url" $ xelems $ catMaybes
    [ Just $ xelemWithText "loc" $ sitemapLocation url
    , renderLastModified <$> sitemapLastModified url
    , xelem "changefreq" . renderChangeFrequency <$> sitemapChangeFrequency url
    , xelemWithText "priority" . T.pack . show <$> sitemapPriority url
    ]

-- | Describes how often a SitemapUrl' is updated. This is considered
-- a hint for crawlers and may or may not be respected.
data ChangeFrequency
    = Always
    -- ^ The page changes every time it is visited.
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly
    | Never
    -- ^ The page is archived and will never change from now on.
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

-- | Build the XML text content for a 'ChangeFrequency'.
renderChangeFrequency :: ChangeFrequency -> Xml Elem
renderChangeFrequency = xtext . \case
    Always  -> "always"
    Hourly  -> "hourly"
    Daily   -> "daily"
    Weekly  -> "weekly"
    Monthly -> "monthly"
    Yearly  -> "yearly"
    Never   -> "never"


-- INDEXES


-- | A 'SitemapIndex' allows informing crawlers of multiple sitemap files
-- hosted on the same domain.
--
-- See https://www.sitemaps.org/protocol.html#index
newtype SitemapIndex =
    SitemapIndex
        { indexEntries :: [IndexEntry]
        } deriving (Show, Read, Eq, Generic)


-- | Render a 'SitemapIndex' into an output format supported by the
-- @xmlgen@ package.
renderSitemapIndex :: XmlOutput x => SitemapIndex -> x
renderSitemapIndex index =
    xrender
        $ doc defaultDocInfo
        $ xelemQ sitemapNamespace "sitemapindex"
        $ xelems
        $ map renderIndexEntry
        $ indexEntries index

-- | A single sitemap entry for a sitemap index.
data IndexEntry =
    IndexEntry
        { indexLocation :: T.Text
        -- ^ The Full URL of a Sitemap, including the protocol.
        --
        -- E.g., @https://www.southernexposure.com/sitemap.xml@
        , indexLastModified :: Maybe UTCTime
        -- ^ The time the sitemap was last changed.
        } deriving (Show, Read, Eq, Generic)

-- | Render an 'IndexEntry' as a @sitemap@ element.
renderIndexEntry :: IndexEntry -> Xml Elem
renderIndexEntry entry = xelem "sitemap" $ xelems $ catMaybes
    [ Just $ xelemWithText "loc" $ indexLocation entry
    , renderLastModified <$> indexLastModified entry
    ]


-- UTILS


-- | An XML Namespace for the sitemaps.org @v0.9@ schema.
sitemapNamespace :: Namespace
sitemapNamespace = namespace "" "http://www.sitemaps.org/schemas/sitemap/0.9"

-- | Render the 'UTCTime' in @YYYY-MM-DDTHH:MM:SS+00:00@ format.
formatSitemapTime :: UTCTime -> T.Text
formatSitemapTime = T.pack . formatTime defaultTimeLocale "%FT%T+00:00"

-- | Render a 'UTCTime' in a @lastmod@ element.
renderLastModified :: UTCTime -> Xml Elem
renderLastModified = xelemWithText "lastmod" . formatSitemapTime
