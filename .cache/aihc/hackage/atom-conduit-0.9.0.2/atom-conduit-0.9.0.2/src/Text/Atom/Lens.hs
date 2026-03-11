{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.Atom.Lens (module Text.Atom.Lens) where

-- {{{ Imports
import           Lens.Micro.TH

import           Text.Atom.Types

import           URI.ByteString
-- }}}

makeLensesFor
  [ ("categoryTerm", "categoryTermL")
  , ("categoryScheme", "categorySchemeL")
  , ("categoryLabel", "categoryLabelL")
  ] ''AtomCategory

makeLensesFor
  [ ("entryAuthors", "entryAuthorsL")
  , ("entryCategories", "entryCategoriesL")
  , ("entryContent", "entryContentL")
  , ("entryContributors", "entryContributorsL")
  , ("entryId", "entryIdL")
  , ("entryLinks", "entryLinksL")
  , ("entryPublished", "entryPublishedL")
  , ("entryRights", "entryRightsL")
  , ("entrySource", "entrySourceL")
  , ("entrySummary", "entrySummaryL")
  , ("entryTitle", "entryTitleL")
  , ("entryUpdated", "entryUpdatedL")
  ] ''AtomEntry


makeLensesFor
  [ ("feedAuthors", "feedAuthorsL")
  , ("feedCategories", "feedCategoriesL")
  , ("feedContributors", "feedContributorsL")
  , ("feedEntries", "feedEntriesL")
  , ("feedGenerator", "feedGeneratorL")
  , ("feedIcon", "feedIconL")
  , ("feedId", "feedIdL")
  , ("feedLinks", "feedLinksL")
  , ("feedLogo", "feedLogoL")
  , ("feedRights", "feedRightsL")
  , ("feedSubtitle", "feedSubtitleL")
  , ("feedTitle", "feedTitleL")
  , ("feedUpdated", "feedUpdatedL")
  ] ''AtomFeed

makeLensesFor
  [ ("generatorVersion", "generatorVersionL")
  , ("generatorContent", "generatorContentL")
  , ("generatorUri", "generatorUriL")
  ] ''AtomGenerator

makeLensesFor
  [ ("linkHref", "linkHrefL")
  , ("linkType", "linkTypeL")
  , ("linkRel", "linkRelL")
  , ("linkLang", "linkLangL")
  , ("linkTitle", "linkTitleL")
  , ("linkLength", "linkLengthL")
  ] ''AtomLink

makeLensesFor
  [ ("personName", "personNameL")
  , ("personEmail", "personEmailL")
  , ("personUri", "personUriL")
  ] ''AtomPerson

makeLensesFor
  [ ("sourceAuthors", "sourceAuthorsL")
  , ("sourceCategories", "sourceCategoriesL")
  , ("sourceContributors", "sourceContributorsL")
  , ("sourceGenerator", "sourceGeneratorL")
  , ("sourceIcon", "sourceIconL")
  , ("sourceId", "sourceIdL")
  , ("sourceLinks", "sourceLinksL")
  , ("sourceLogo", "sourceLogoL")
  , ("sourceRights", "sourceRightsL")
  , ("sourceSubtitle", "sourceSubtitleL")
  , ("sourceTitle", "sourceTitleL")
  , ("sourceUpdated", "sourceUpdatedL")
  ] ''AtomSource

--makeTraversals ''AtomText
