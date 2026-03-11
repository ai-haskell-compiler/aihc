{-# LANGUAGE LambdaCase #-}

module Xmlbf.XmlHtml
 ( fromXmlHtmlNode
 , fromRawXml
 , fromRawHtml
 ) where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Text.XmlHtml as XmlHtml
import qualified Xmlbf

--------------------------------------------------------------------------------
-- XmlHtml support

-- | Convert a 'XmlHtml.Node' from "Text.XmlHtml" into an 'Node' from "Xmlbf",
-- if possible.
fromXmlHtmlNode
  :: XmlHtml.Node -- ^ A 'XmlHtml.Node' from "Text.XmlHtml".
  -> Either String Xmlbf.Node -- ^ A 'Xmlbf.Node' from "Xmlbf", if possible.
fromXmlHtmlNode = \case
  XmlHtml.Comment _ -> Left "Comments not supported"
  XmlHtml.TextNode t -> Xmlbf.text' t
  XmlHtml.Element t as cs -> do
    cs' <- traverse fromXmlHtmlNode cs
    Xmlbf.element' t (HM.fromList as) cs'

-- | Parses a given UTF8-encoded raw XML fragment into @a@, using the @xmlhtml@
-- Haskell library, so all of @xmlhtml@'s parsing quirks apply.
--
-- You can provide the output of this function as input to "Xmlbf"'s
-- 'Xmlbf.parse'.
--
-- The given XML can contain more zero or more text or element nodes.
--
-- Comments are discarded from the resulting nodes and their children.
--
-- Surrounding whitespace is not stripped.
fromRawXml
  :: B.ByteString                 -- ^ Raw XML fragment.
  -> Either String [Xmlbf.Node]   -- ^ 'Xmlbf.Node's from "Xmlbf"
fromRawXml = \bs -> case XmlHtml.parseXML "xmlbf-xmlhtml-input.xml" bs of
  Left e -> Left ("Malformed XML: " ++ e)
  Right d -> traverse fromXmlHtmlNode (XmlHtml.docContent d)

-- | Like 'fromRawXml', but parses using @xmlhtml@'s quirks HTML mode.
fromRawHtml
  :: B.ByteString                 -- ^ Raw HTML fragment.
  -> Either String [Xmlbf.Node]   -- ^ 'Xmlbf.Node's from "Xmlbf"
fromRawHtml = \bs -> case XmlHtml.parseHTML "xmlbf-xmlhtml-input.html" bs of
  Left e -> Left ("Malformed HTML: " ++ e)
  Right d -> traverse fromXmlHtmlNode (XmlHtml.docContent d)

