module Text.Html.IsLink
    ( isLinkAttr
    , allLinkAttrs

    -- * Example with HXT
    -- $example
    ) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

-- | @isLinkAttr tag attr@ returns 'True' if the attribute @attr@ of an HTML
-- element with tag name @tag@ points to an external resource, and 'False'
-- otherwise. So for example @isLinkAttr \"a\" \"href\"@ returns 'True' whereas
-- @isLinkAttr \"a\" \"class\"@ returns 'False'. Note that 'isLinkAttr'
-- expects both @tag@ and @attr@ to be in lowercase, so for example
-- @isLinkAttr \"A\" \"HREF\"@ returns 'False'.
isLinkAttr :: String -> String -> Bool
isLinkAttr tag attr = HS.member (tag, attr) allLinkAttrs

-- sources:
--     * The HTML 4.01 transitional DTD
--     * The HTML 4.01 strict DTD
--     * The HTML 3.2 DTD
--     * The HTML 3.0 DTD
--     * The XMLmind XML Editor 6.0.0 Evaluation Edition contains
--       a BSD licensed W3C XML Schema file for HTML 5
--     * The HTML::Tagset perl module (version 3.20)
-- see the scripts/ directory for more details
-- | A 'HashSet' that contains all combinations of tag names and attributes
-- that correspond to links.
allLinkAttrs :: HashSet (String, String)
allLinkAttrs = HS.fromList
    [ ("a", "href")
    , ("applet", "archive")
    , ("applet", "code")
    , ("applet", "codebase")
    , ("area", "href")
    , ("audio", "src")
    , ("base", "href")
    , ("bgsound", "src")
    , ("blockquote", "cite")
    , ("body", "background")
    , ("button", "formaction")
    , ("command", "icon")
    , ("del", "cite")
    , ("embed", "pluginspage")
    , ("embed", "src")
    , ("fig", "src")
    , ("form", "action")
    , ("frame", "longdesc")
    , ("frame", "src")
    , ("head", "profile")
    , ("hr", "src")
    , ("html", "manifest")
    , ("iframe", "longdesc")
    , ("iframe", "src")
    , ("ilayer", "background")
    , ("img", "longdesc")
    , ("img", "lowsrc")
    , ("img", "src")
    , ("img", "usemap")
    , ("input", "formaction")
    , ("input", "src")
    , ("input", "usemap")
    , ("ins", "cite")
    , ("isindex", "action")
    , ("layer", "background")
    , ("layer", "src")
    , ("link", "href")
    , ("note", "src")
    , ("object", "archive")
    , ("object", "classid")
    , ("object", "codebase")
    , ("object", "data")
    , ("object", "usemap")
    , ("overlay", "src")
    , ("q", "cite")
    , ("script", "for")
    , ("script", "src")
    , ("source", "src")
    , ("table", "background")
    , ("td", "background")
    , ("th", "background")
    , ("track", "src")
    , ("tr", "background")
    , ("video", "poster")
    , ("video", "src")
    , ("xmp", "href")
    ]

-- $example
-- Here's an example illustrating how to use 'isLinkAttr' with @hxt@ in
-- order to extract all links from an HTML document:
--
-- > {-# LANGUAGE Arrows #-}
-- >
-- > import Text.Html.IsLink
-- > import Text.XML.HXT.Core
-- >
-- > -- returns a list of tuples containing the tag name, attribute name,
-- > -- attribute value of all links
-- > getAllLinks :: FilePath -> IO [(String, String, String)]
-- > getAllLinks path = runX $ doc >>> multi getLink
-- >   where
-- >     doc = readDocument [withParseHTML yes, withWarnings no] path
-- >
-- > getLink :: ArrowXml a => a XmlTree (String, String, String)
-- > getLink = proc node -> do
-- >     tag <- getName -< node
-- >     attrbNode <- getAttrl -< node
-- >     attrb <- getName -< attrbNode
-- >     val <- xshow getChildren -< attrbNode
-- >     isLinkA -< (tag, attrb, val)
-- >   where
-- >     isLinkA = isLink `guardsP` this
-- >     isLink (tag, attrb, _) = isLinkAttr tag attrb
