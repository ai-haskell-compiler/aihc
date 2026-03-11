{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | Streaming renderers for the Atom 1.0 standard.
module Text.Atom.Conduit.Render
  ( -- * Top-level
    renderAtomFeed
    -- * Elements
  , renderAtomEntry
  , renderAtomContent
  , renderAtomSource
  , renderAtomGenerator
  , renderAtomLink
  , renderAtomCategory
    -- * Constructs
  , renderAtomPerson
  , renderAtomText
  ) where

-- {{{ Imports
import           Text.Atom.Lens
import           Text.Atom.Types

import           Conduit
import           Control.Monad
import           Data.Monoid
import           Data.Text              as Text
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.XML.Types
import           Lens.Micro
import           Refined
import           Text.XML               as XML
import           Text.XML.Stream.Render
import qualified Text.XML.Unresolved    as Unresolved
import           URI.ByteString
-- }}}

-- | Render the top-level @atom:feed@ element.
renderAtomFeed :: (Monad m) => AtomFeed -> ConduitT () Event m ()
renderAtomFeed f = atomTag "feed" (attr "xmlns" "http://www.w3.org/2005/Atom") $ do
  forM_ (f^.feedAuthorsL) $ renderAtomPerson "author"
  forM_ (f^.feedCategoriesL) renderAtomCategory
  forM_ (f^.feedContributorsL) $ renderAtomPerson "contributor"
  forM_ (f^.feedEntriesL) renderAtomEntry
  forM_ (f^.feedGeneratorL) renderAtomGenerator
  forM_ (feedIcon f) $ atomTag "icon" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'
  atomTag "id" mempty . content $ f^.feedIdL
  forM_ (f^.feedLinksL) renderAtomLink
  forM_ (feedLogo f) $ atomTag "logo" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'
  forM_ (f^.feedRightsL) $ renderAtomText "rights"
  forM_ (f^.feedSubtitleL) $ renderAtomText "subtitle"
  renderAtomText "title" $ f^.feedTitleL
  dateTag "updated" $ f^.feedUpdatedL

-- | Render an @atom:entry@ element.
renderAtomEntry :: (Monad m) => AtomEntry -> ConduitT () Event m ()
renderAtomEntry e = atomTag "entry" mempty $ do
  forM_ (e^.entryAuthorsL) $ renderAtomPerson "author"
  forM_ (e^.entryCategoriesL) renderAtomCategory
  forM_ (e^.entryContentL) renderAtomContent
  forM_ (e^.entryContributorsL) $ renderAtomPerson "contributor"
  atomTag "id" mempty . content $ e^.entryIdL
  forM_ (e^.entryLinksL) renderAtomLink
  forM_ (e^.entryPublishedL) $ dateTag "published"
  forM_ (e^.entryRightsL) $ renderAtomText "rights"
  forM_ (e^.entrySourceL) renderAtomSource
  forM_ (e^.entrySummaryL) $ renderAtomText "summary"
  renderAtomText "title" (e^.entryTitleL)
  dateTag "updated" (e^.entryUpdatedL)

-- | Render an @atom:content@ element.
renderAtomContent :: (Monad m) => AtomContent -> ConduitT () Event m ()
renderAtomContent (AtomContentInlineXHTML element) = atomTag "content" (attr "type" "xhtml") $ yieldMany $ Unresolved.elementToEvents element
renderAtomContent (AtomContentOutOfLine ctype uri) = atomTag "content" (nonEmptyAttr "type" ctype <> attr "src" (decodeUtf8 $ withAtomURI serializeURIRef' uri)) $ return ()
renderAtomContent (AtomContentInlineText TypeHTML t) = atomTag "content" (attr "type" "html") $ content t
renderAtomContent (AtomContentInlineText TypeText t) = atomTag "content" mempty $ content t
renderAtomContent (AtomContentInlineOther ctype t) = atomTag "content" (attr "type" ctype) $ content t

-- | Render an @atom:source@ element.
renderAtomSource :: (Monad m) => AtomSource -> ConduitT () Event m ()
renderAtomSource s = atomTag "source" mempty $ do
  forM_ (s^.sourceAuthorsL) $ renderAtomPerson "author"
  forM_ (s^.sourceCategoriesL) renderAtomCategory
  forM_ (s^.sourceContributorsL) $ renderAtomPerson "contributor"
  forM_ (s^.sourceGeneratorL) renderAtomGenerator
  forM_ (sourceIcon s) $ atomTag "icon" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'
  unless (Text.null $ s^.sourceIdL) . atomTag "id" mempty . content $ s^.sourceIdL
  forM_ (s^.sourceLinksL) renderAtomLink
  forM_ (sourceLogo s) $ atomTag "logo" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'
  forM_ (s^.sourceRightsL) $ renderAtomText "rights"
  forM_ (s^.sourceSubtitleL) $ renderAtomText "subtitle"
  forM_ (s^.sourceTitleL) $ renderAtomText "title"
  forM_ (s^.sourceUpdatedL) $ dateTag "updated"

-- | Render an @atom:generator@ element.
renderAtomGenerator :: (Monad m) => AtomGenerator -> ConduitT () Event m ()
renderAtomGenerator g = atomTag "generator" attributes . content . unrefine $ g^.generatorContentL
  where attributes = optionalAttr "uri" (decodeUtf8 . withAtomURI serializeURIRef' <$> generatorUri g)
                     <> nonEmptyAttr "version" (g^.generatorVersionL)

-- | Render an @atom:link@ element.
renderAtomLink :: (Monad m) => AtomLink -> ConduitT () Event m ()
renderAtomLink l = atomTag "link" linkAttrs $ return ()
  where linkAttrs = attr "href" (decodeUtf8 . withAtomURI serializeURIRef' $ linkHref l)
                    <> nonEmptyAttr "rel" (l^.linkRelL)
                    <> nonEmptyAttr "type" (l^.linkTypeL)
                    <> nonEmptyAttr "hreflang" (l^.linkLangL)
                    <> nonEmptyAttr "title" (l^.linkTitleL)
                    <> nonEmptyAttr "length" (l^.linkLengthL)

-- | Render an @atom:category@ element.
renderAtomCategory :: (Monad m) => AtomCategory -> ConduitT () Event m ()
renderAtomCategory c = atomTag "category" attributes $ return ()
  where attributes = attr "term" (unrefine $ c^.categoryTermL)
                     <> nonEmptyAttr "scheme" (c^.categorySchemeL)
                     <> nonEmptyAttr "label" (c^.categoryLabelL)

-- | Render an atom person construct.
renderAtomPerson :: (Monad m) => Text -> AtomPerson -> ConduitT () Event m ()
renderAtomPerson name p = atomTag name mempty $ do
  atomTag "name" mempty . content . unrefine $ p^.personNameL
  unless (Text.null $ p^.personEmailL) $ atomTag "email" mempty . content $ p^.personEmailL
  forM_ (personUri p) $ atomTag "uri" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'

-- | Render an atom text construct.
renderAtomText :: Monad m => Text -> AtomText -> ConduitT () Event m ()
renderAtomText name (AtomXHTMLText element) = atomTag name (attr "type" "xhtml") $ yieldMany $ Unresolved.elementToEvents element
renderAtomText name (AtomPlainText TypeHTML t) = atomTag name (attr "type" "html") $ content t
renderAtomText name (AtomPlainText TypeText t) = atomTag name mempty $ content t

atomTag :: Monad m => Text -> Attributes -> ConduitT () Event m () -> ConduitT () Event m ()
atomTag name = tag (Name name (Just "http://www.w3.org/2005/Atom") (Just "atom"))

dateTag :: (Monad m) => Text -> UTCTime -> ConduitT () Event m ()
dateTag name = atomTag name mempty . content . formatTimeRFC3339 . utcToZonedTime utc

nonEmptyAttr :: Name -> Text -> Attributes
nonEmptyAttr name value
  | value == mempty = mempty
  | otherwise = attr name value
