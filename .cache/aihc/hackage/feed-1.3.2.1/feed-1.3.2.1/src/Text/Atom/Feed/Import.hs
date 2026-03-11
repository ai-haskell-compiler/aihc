--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Feed.Import
-- Copyright : (c) Galois, Inc. 2007-2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability:: portable
-- Description: Convert from XML to Atom
--
-- Convert from XML to Atom
--
--------------------------------------------------------------------
module Text.Atom.Feed.Import
  ( pNodes
  , pQNodes
  , pNode
  , pQNode
  , pLeaf
  , pQLeaf
  , pAttr
  , pAttrs
  , pQAttr
  , pMany
  , children
  , elementFeed
  , pTextContent
  , pPerson
  , pCategory
  , pGenerator
  , pSource
  , pLink
  , pEntry
  , pContent
  , pInReplyTotal
  , pInReplyTo
  ) where

import Prelude.Compat

import Control.Monad.Compat (guard, mplus)
import Data.List.Compat (find)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Read
import Data.XML.Types as XML

import Text.Atom.Feed
import Text.Atom.Feed.Export (atomName, atomThreadName)

import qualified Data.Text as T

pNodes :: Text -> [XML.Element] -> [XML.Element]
pNodes x = filter ((atomName x ==) . elementName)

pQNodes :: Name -> [XML.Element] -> [XML.Element]
pQNodes x = filter ((x ==) . elementName)

pNode :: Text -> [XML.Element] -> Maybe XML.Element
pNode x es = listToMaybe (pNodes x es)

pQNode :: Name -> [XML.Element] -> Maybe XML.Element
pQNode x es = listToMaybe (pQNodes x es)

pLeaf :: Text -> [XML.Element] -> Maybe Text
pLeaf x es = (T.concat . elementText) `fmap` pNode x es

pQLeaf :: Name -> [XML.Element] -> Maybe Text
pQLeaf x es = (T.concat . elementText) `fmap` pQNode x es

pAttr :: Text -> XML.Element -> Maybe Text
pAttr x e = (`attributeText` e) =<< find (sameAtomAttr x) (map fst $ elementAttributes e)

pAttrs :: Text -> XML.Element -> [Text]
pAttrs x e = [t | ContentText t <- cnts]
  where
    cnts = concat [v | (k, v) <- elementAttributes e, sameAtomAttr x k]

sameAtomAttr :: Text -> Name -> Bool
sameAtomAttr x k = k == ax || (isNothing (nameNamespace k) && nameLocalName k == x)
  where
    ax = atomName x

pQAttr :: Name -> XML.Element -> Maybe Text
pQAttr = attributeText

pMany :: Text -> (XML.Element -> Maybe a) -> [XML.Element] -> [a]
pMany p f es = mapMaybe f (pNodes p es)

children :: XML.Element -> [XML.Element]
children = elementChildren

elementTexts :: Element -> Text
elementTexts = T.concat . elementText

elementFeed :: XML.Element -> Maybe Feed
elementFeed e = do
  guard (elementName e == atomName "feed")
  let es = children e
  i <- pLeaf "id" es
  t <- pTextContent "title" es `mplus` return (TextString "<no-title>")
  u <- pLeaf "updated" es
  return
    Feed
      { feedId = i
      , feedTitle = t
      , feedSubtitle = pTextContent "subtitle" es
      , feedUpdated = u
      , feedAuthors = pMany "author" pPerson es
      , feedContributors = pMany "contributor" pPerson es
      , feedCategories = pMany "category" pCategory es
      , feedGenerator = pGenerator `fmap` pNode "generator" es
      , feedIcon = pLeaf "icon" es
      , feedLogo = pLeaf "logo" es
      , feedRights = pTextContent "rights" es
      , feedLinks = pMany "link" pLink es
      , feedEntries = pMany "entry" pEntry es
      , feedOther = other_es es
      , feedAttrs = other_as (elementAttributes e)
      }
  where
    other_es = filter ((`notElem` known_elts) . elementName)
    other_as = filter ((`notElem` known_attrs) . fst)
    -- let's have them all (including xml:base and xml:lang + xmlns: stuff)
    known_attrs = []
    known_elts =
      map
        atomName
        [ "author"
        , "category"
        , "contributor"
        , "generator"
        , "icon"
        , "id"
        , "link"
        , "logo"
        , "rights"
        , "subtitle"
        , "title"
        , "updated"
        , "entry"
        ]

pTextContent :: Text -> [XML.Element] -> Maybe TextContent
pTextContent tag es = do
  e <- pNode tag es
  case pAttr "type" e of
    Nothing -> return (TextString (elementTexts e))
    Just "text" -> return (TextString (elementTexts e))
    Just "html" -> return (HTMLString (elementTexts e))
    Just "xhtml" ->
      case children e -- hmm...
            of
        [c] -> return (XHTMLString c)
        _ -> Nothing -- Multiple XHTML children.
    _ -> Nothing -- Unknown text content type.

pPerson :: XML.Element -> Maybe Person
pPerson e = do
  let es = children e
  name <- pLeaf "name" es -- or missing "name"
  return
    Person
      { personName = name
      , personURI = pLeaf "uri" es
      , personEmail = pLeaf "email" es
      , personOther = [] -- XXX?
      }

pCategory :: XML.Element -> Maybe Category
pCategory e = do
  term <- pAttr "term" e -- or missing "term" attribute
  return
    Category
      { catTerm = term
      , catScheme = pAttr "scheme" e
      , catLabel = pAttr "label" e
      , catOther = [] -- XXX?
      }

pGenerator :: XML.Element -> Generator
pGenerator e =
  Generator {genURI = pAttr "href" e, genVersion = pAttr "version" e, genText = elementTexts e}

pSource :: XML.Element -> Source
pSource e =
  let es = children e
   in Source
        { sourceAuthors = pMany "author" pPerson es
        , sourceCategories = pMany "category" pCategory es
        , sourceGenerator = pGenerator `fmap` pNode "generator" es
        , sourceIcon = pLeaf "icon" es
        , sourceId = pLeaf "id" es
        , sourceLinks = pMany "link" pLink es
        , sourceLogo = pLeaf "logo" es
        , sourceRights = pTextContent "rights" es
        , sourceSubtitle = pTextContent "subtitle" es
        , sourceTitle = pTextContent "title" es
        , sourceUpdated = pLeaf "updated" es
        , sourceOther = [] -- XXX ?
        }

pLink :: XML.Element -> Maybe Link
pLink e = do
  uri <- pAttr "href" e
  return
    Link
      { linkHref = uri
      , linkRel = Right `fmap` pAttr "rel" e
      , linkType = pAttr "type" e
      , linkHrefLang = pAttr "hreflang" e
      , linkTitle = pAttr "title" e
      , linkLength = pAttr "length" e
      , linkAttrs = other_as (elementAttributes e)
      , linkOther = []
      }
  where
    other_as = filter ((`notElem` known_attrs) . fst)
    known_attrs = map atomName ["href", "rel", "type", "hreflang", "title", "length"]

pEntry :: XML.Element -> Maybe Entry
pEntry e = do
  let es = children e
  i <- pLeaf "id" es
  t <- pTextContent "title" es
  u <- pLeaf "updated" es `mplus` pLeaf "published" es
  return
    Entry
      { entryId = i
      , entryTitle = t
      , entryUpdated = u
      , entryAuthors = pMany "author" pPerson es
      , entryContributor = pMany "contributor" pPerson es
      , entryCategories = pMany "category" pCategory es
      , entryContent = pContent =<< pNode "content" es
      , entryLinks = pMany "link" pLink es
      , entryPublished = pLeaf "published" es
      , entryRights = pTextContent "rights" es
      , entrySource = pSource `fmap` pNode "source" es
      , entrySummary = pTextContent "summary" es
      , entryInReplyTo = pInReplyTo es
      , entryInReplyTotal = pInReplyTotal es
      , entryAttrs = other_as (elementAttributes e)
      , entryOther = [] -- ?
      }
  where
    other_as = filter ((`notElem` known_attrs) . fst)
    -- let's have them all (including xml:base and xml:lang + xmlns: stuff)
    known_attrs = []

pContent :: XML.Element -> Maybe EntryContent
pContent e =
  case pAttr "type" e of
    Nothing -> return (TextContent (elementTexts e))
    Just "text" -> return (TextContent (elementTexts e))
    Just "html" -> return (HTMLContent (elementTexts e))
    Just "xhtml" ->
      case children e of
        [] -> return (TextContent "")
        [c] -> return (XHTMLContent c)
        _ -> Nothing
    Just ty ->
      case pAttr "src" e of
        Nothing -> return (MixedContent (Just ty) (elementNodes e))
        Just uri -> return (ExternalContent (Just ty) uri)

pInReplyTotal :: [XML.Element] -> Maybe InReplyTotal
pInReplyTotal es = do
  t <- pQLeaf (atomThreadName "total") es
  case decimal t of
    Right (x, _) -> do
      n <- pQNode (atomThreadName "total") es
      return InReplyTotal {replyToTotal = x, replyToTotalOther = elementAttributes n}
    _ -> fail "no parse"

pInReplyTo :: [XML.Element] -> Maybe InReplyTo
pInReplyTo es = do
  t <- pQNode (atomThreadName "reply-to") es
  case pQAttr (atomThreadName "ref") t of
    Just ref ->
      return
        InReplyTo
          { replyToRef = ref
          , replyToHRef = pQAttr (atomThreadName "href") t
          , replyToType = pQAttr (atomThreadName "type") t
          , replyToSource = pQAttr (atomThreadName "source") t
          , replyToOther = elementAttributes t -- ToDo: snip out matched ones.
          , replyToContent = elementNodes t
          }
    _ -> fail "no parse"
