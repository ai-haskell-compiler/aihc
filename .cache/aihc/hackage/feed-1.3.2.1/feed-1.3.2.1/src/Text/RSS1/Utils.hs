--------------------------------------------------------------------
-- |
-- Module    : Text.RSS1.Utils
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.RSS1.Utils
  ( pQNodes
  , pNode
  , pQNode
  , pLeaf
  , pQLeaf
  , pQLeaf'
  , pAttr
  , pAttr'
  , pMany
  , children
  , qualName
  , qualName'
  , rss10NS
  , rdfPrefix
  , rdfNS
  , synPrefix
  , synNS
  , taxPrefix
  , taxNS
  , conPrefix
  , conNS
  , dcPrefix
  , dcNS
  , rdfName
  , rssName
  , synName
  , known_rss_elts
  , known_syn_elts
  , known_dc_elts
  , known_tax_elts
  , known_con_elts
  , removeKnownElts
  , removeKnownAttrs
  ) where

import Prelude.Compat

import Data.XML.Compat
import Data.XML.Types as XML
import Text.DublinCore.Types

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)

pQNodes :: Name -> XML.Element -> [XML.Element]
pQNodes = findChildren

pNode :: Text -> XML.Element -> Maybe XML.Element
pNode x e = listToMaybe (pQNodes (qualName (Just rss10NS, Nothing) x) e)

pQNode :: Name -> XML.Element -> Maybe XML.Element
pQNode x e = listToMaybe (pQNodes x e)

pLeaf :: Text -> XML.Element -> Maybe Text
pLeaf x e = strContent `fmap` pQNode (qualName (Just rss10NS, Nothing) x) e

pQLeaf' :: (Text, Text) -> Text -> XML.Element -> Maybe Text
pQLeaf' (ns, pre) = pQLeaf (ns, Just pre)

pQLeaf :: (Text, Maybe Text) -> Text -> XML.Element -> Maybe Text
pQLeaf (ns, pre) x e = strContent `fmap` pQNode (qualName (Just ns, pre) x) e

pAttr :: (Maybe Text, Maybe Text) -> Text -> XML.Element -> Maybe Text
pAttr ns x = attributeText (qualName ns x)

pAttr' :: (Text, Text) -> Text -> XML.Element -> Maybe Text
pAttr' (ns, pre) = pAttr (Just ns, Just pre)

pMany :: (Maybe Text, Maybe Text) -> Text -> (XML.Element -> Maybe a) -> XML.Element -> [a]
pMany ns p f e = mapMaybe f (pQNodes (qualName ns p) e)

children :: XML.Element -> [XML.Element]
children = elementChildren

qualName :: (Maybe Text, Maybe Text) -> Text -> Name
qualName (ns, pre) x = Name x ns pre

qualName' :: (Text, Text) -> Text -> Name
qualName' (ns, pre) x = Name x (Just ns) (Just pre)

rss10NS :: Text
rss10NS = "http://purl.org/rss/1.0/"

rdfPrefix, rdfNS :: Text
rdfNS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

rdfPrefix = "rdf"

synPrefix, synNS :: Text
synNS = "http://purl.org/rss/1.0/modules/syndication/"

synPrefix = "sy"

taxPrefix, taxNS :: Text
taxNS = "http://purl.org/rss/1.0/modules/taxonomy/"

taxPrefix = "taxo"

conPrefix, conNS :: Text
conNS = "http://purl.org/rss/1.0/modules/content/"

conPrefix = "content"

dcPrefix, dcNS :: Text
dcNS = "http://purl.org/dc/elements/1.1/"

dcPrefix = "dc"

rdfName :: Text -> Name
rdfName x = Name x (Just rdfNS) (Just rdfPrefix)

rssName :: Text -> Name
rssName x = Name x (Just rss10NS) Nothing

synName :: Text -> Name
synName x = Name x (Just synNS) (Just synPrefix)

known_rss_elts :: [Name]
known_rss_elts = map rssName ["channel", "item", "image", "textinput"]

known_syn_elts :: [Name]
known_syn_elts = map synName ["updateBase", "updateFrequency", "updatePeriod"]

known_dc_elts :: [Name]
known_dc_elts = map (qualName' (dcNS, dcPrefix)) dc_element_names

known_tax_elts :: [Name]
known_tax_elts = map (qualName' (taxNS, taxPrefix)) ["topic", "topics"]

known_con_elts :: [Name]
known_con_elts = map (qualName' (conNS, conPrefix)) ["items", "item", "format", "encoding"]

removeKnownElts :: XML.Element -> [XML.Element]
removeKnownElts e = filter (\e1 -> elementName e1 `notElem` known_elts) (elementChildren e)
  where
    known_elts =
      concat [known_rss_elts, known_syn_elts, known_dc_elts, known_con_elts, known_tax_elts]

removeKnownAttrs :: XML.Element -> [Attr]
removeKnownAttrs e = filter ((`notElem` known_attrs) . fst) (elementAttributes e)
  where
    known_attrs = map rdfName ["about"]
