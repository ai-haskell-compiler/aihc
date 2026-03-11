{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Page rendering
module Web.Rep.Render
  ( renderPage,
    renderPageWith,
    renderPageHtmlWith,
    renderPageAsByteString,
    renderPageToFile,
    renderPageHtmlToFile,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Foldable
import MarkupParse
import Optics.Core hiding (element)
import Web.Rep.Html
import Web.Rep.Page

-- | Render a Page with the default configuration into Html.
renderPage :: Page -> Markup
renderPage p =
  (\(_, _, x) -> x) $ renderPageWith (defaultPageConfig "default") p

-- | Render a Page into Html.
renderPageHtmlWith :: PageConfig -> Page -> Markup
renderPageHtmlWith pc p =
  (\(_, _, x) -> x) $ renderPageWith pc p

-- | Render a Page into css text, js text and html.
renderPageWith :: PageConfig -> Page -> (ByteString, ByteString, Markup)
renderPageWith pc p =
  case pc ^. #concerns of
    Inline -> (mempty, mempty, h)
    Separated -> (css, js, h)
  where
    h =
      case pc ^. #structure of
        HeaderBody ->
          doctypeHtml
            <> element
              "html"
              [Attr "lang" "en"]
              ( element
                  "head"
                  []
                  (element_ "meta" [Attr "charset" "utf-8"])
                  <> libsCss'
                  <> cssInline
                  <> view #htmlHeader p
              )
            <> element
              "body"
              []
              ( view #htmlBody p
                  <> libsJs'
                  <> jsInline
              )
        Headless ->
          doctypeHtml
            <> element_ "meta" [Attr "charset" "utf-8"]
            <> libsCss'
            <> cssInline
            <> view #htmlHeader p
            <> p ^. #htmlBody
            <> libsJs'
            <> jsInline
        Snippet ->
          cssInline
            <> libsCss'
            <> view #htmlHeader p
            <> view #htmlBody p
            <> libsJs'
            <> jsInline

    css :: ByteString
    css = renderCss (view #renderStyle pc) (p ^. #cssBody)

    js :: ByteString
    js = jsByteString (p ^. #jsGlobal <> onLoad (p ^. #jsOnLoad))
    cssInline
      | pc ^. #concerns == Separated || css == mempty = mempty
      | otherwise = elementc "style" [Attr "type" "text/css"] css
    jsInline
      | pc ^. #concerns == Separated || js == mempty = mempty
      | otherwise = elementc "script" [] js
    libsCss' =
      case pc ^. #concerns of
        Inline -> view #libsCss p
        Separated ->
          view #libsCss p
            <> libCss (strToUtf8 $ pc ^. #filenames % #cssConcern)
    libsJs' =
      case pc ^. #concerns of
        Inline -> p ^. #libsJs
        Separated ->
          view #libsJs p
            <> libJs (strToUtf8 $ pc ^. #filenames % #jsConcern)

-- | Render Page concerns to files.
renderPageToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageToFile dir pc page =
  sequenceA_ $ liftA2 writeFile' (pc ^. #filenames) (renderPageAsByteString pc page)
  where
    writeFile' fp s = unless (s == mempty) (B.writeFile (dir <> "/" <> fp) s)

-- | Render a page to just a Html file.
renderPageHtmlToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageHtmlToFile file pc page =
  B.writeFile file (markdown_ (view #renderStyle pc) Html $ renderPageHtmlWith pc page)

-- | Render a Page as Text.
renderPageAsByteString :: PageConfig -> Page -> Concerns ByteString
renderPageAsByteString pc p =
  case pc ^. #concerns of
    Inline -> Concerns mempty mempty (markdown_ (view #renderStyle pc) Html h)
    Separated -> Concerns css js (markdown_ (view #renderStyle pc) Html h)
  where
    (css, js, h) = renderPageWith pc p
