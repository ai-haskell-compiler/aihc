{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines the HTML render.
module Zenacy.HTML.Internal.Render
  ( htmlPrint
  , htmlPrintPretty
  , htmlRender
  , htmlRenderContent
  , htmlRenderNodes
  , htmlRenderPretty
  ) where

import Zenacy.HTML.Internal.Core
import Zenacy.HTML.Internal.HTML
import Zenacy.HTML.Internal.Oper
import Data.Monoid
  ( (<>)
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( append
  , concat
  , empty
  , intercalate
  , replace
  )
import qualified Data.Text.IO as T
  ( putStrLn
  )

-- | The rendering mode.
data HTMLRenderMode
  = HTMLRenderNormal
  | HTMLRenderPretty
    deriving (Show, Eq, Ord)

-- | Prints an HTML document.
htmlPrint :: HTMLNode -> IO ()
htmlPrint = T.putStrLn . htmlRender

-- | Pretty prints an HTML document.
htmlPrintPretty :: HTMLNode -> IO ()
htmlPrintPretty = T.putStrLn . htmlRenderPretty

-- | Renders an HTML document.
htmlRender :: HTMLNode -> Text
htmlRender = renderModal HTMLRenderNormal

-- | Renders the contents of a node
htmlRenderContent :: HTMLNode -> Text
htmlRenderContent = htmlRenderNodes . htmlNodeContent

-- | Renders a list of nodes.
htmlRenderNodes :: [HTMLNode] -> Text
htmlRenderNodes = T.concat . map htmlRender

-- | Renders an HTML document using pretty printing.
htmlRenderPretty :: HTMLNode -> Text
htmlRenderPretty = renderModal HTMLRenderPretty

-- | Renders an HTML document with a styling mode.
renderModal :: HTMLRenderMode -> HTMLNode -> Text
renderModal m = go 0 ""
  where
    go level parent node =
      case node of
        HTMLDocument _ _ c ->
          join $ map (go level parent) c
        HTMLDoctype n p s ->
          indent <> renderDoctype n p s
        HTMLFragment n c ->
          join $ map (go level parent) c
        HTMLElement n s a c ->
          indent
          <> renderElemStart n a
          <> if voidTag n
             then T.empty
             else (if | genLF n c -> "\n"
                      | oneLine c -> T.empty
                      | otherwise -> sep)
                  <> (join $ map (go (if oneLine c then 0 else level') n) c)
                  <> (if | oneLine c -> T.empty
                         | null c -> indent
                         | otherwise -> sep <> indent)
                  <> renderElemEnd n
        HTMLTemplate s a c ->
          indent <> renderElemStart tmp a
          <> sep <> go level' tmp c
          <> sep <> indent <> renderElemEnd tmp
        HTMLText t ->
          indent <> renderText t parent
        HTMLComment c ->
          indent <> renderComment c
      where
        level' = level + 1
        join = T.intercalate sep
        indent = case m of
          HTMLRenderNormal -> T.empty
          HTMLRenderPretty -> textBlank level
        sep = case m of
          HTMLRenderNormal -> T.empty
          HTMLRenderPretty -> "\n"
        tmp = "template"
        voidTag x = elem x
          ["area", "base", "basefont", "bgsound", "br", "col",
           "embed", "frame", "hr", "img", "input", "keygen",
           "link", "meta", "param", "source", "track", "wbr"]
        genLF x c = elem x ["pre", "textarea", "listing"] && oneText c
        oneText (HTMLText {}:[]) = True
        oneText _ = False
        oneLine x = oneText x || null x

-- | Renders text for a doctype.
renderDoctype :: Text -> Maybe Text -> Maybe Text -> Text
renderDoctype x y z = "<!DOCTYPE " <> x <> f y <> f z <> ">"
  where
    f = maybe "" (T.append " ")

-- | Returns text for an attribute.
renderAttr :: HTMLAttr -> Text
renderAttr (HTMLAttr n v s) =
  " " <> n' <> "=\"" <> escapeString True v <> "\""
  where
    n' = case s of
      HTMLAttrNamespaceNone -> n
      HTMLAttrNamespaceXLink -> "xlink:" <> n
      HTMLAttrNamespaceXML -> "xml:" <> n
      HTMLAttrNamespaceXMLNS ->
        if n == "xmlns" then n else "xmlns:" <> n

-- | Returns text for a list of attributes.
renderAttrList :: [HTMLAttr] -> Text
renderAttrList = T.concat . map renderAttr

-- | Returns text for a start element.
renderElemStart :: Text -> [HTMLAttr] -> Text
renderElemStart x y = "<" <> x <> renderAttrList y <> ">"

-- | Returns text for an end element.
renderElemEnd :: Text -> Text
renderElemEnd x = "</" <> x <> ">"

-- | Renders a text value.
renderText :: Text -> Text -> Text
renderText x parent =
  if parent `elem` a then x else escapeString False x
  where a = ["style", "script", "xmp", "iframe",
             "noembed", "noframes", "plaintext"]

-- | Renders text for a comment element.
renderComment :: Text -> Text
renderComment x = "<!--" <> x <> "-->"

-- | Escapes a string for serialization.
escapeString :: Bool -> Text -> Text
escapeString attributeMode =
  f . T.replace "\x00A0"  "&nbsp;"
    -- . T.replace "&" "&amp;" -- TODO: consider if this is needed
  where
    f = if attributeMode
        then T.replace "\"" "&quot;"
        else T.replace ">" "&gt;"
           . T.replace "<" "&lt;"
