{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Text.HTML.QQ

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module provides a quasi-quoter for HTML 'Document's.  See the 'html'
function for some examples.

See "Text.XML.QQ" for an explanation of the difference between "Text.HTML.QQ"
and "Text.XML.QQ".
-}

module Text.HTML.QQ
  ( html
  , htmlRaw
    -- * Types
  , Document
  ) where

import Data.Text.Lazy (pack)
import Language.Haskell.TH (appE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Heterocephalus (compileFromString, textSetting)
import Text.HTML.DOM (parseLT)
import Text.XML (Document)

import Text.XMLHTML.Internal (createExpQuasiQuoter)

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XTemplateHaskell


-- | This 'QuasiQuoter' produces HTML 'Document's.
--
-- This 'QuasiQuoter' produces expressions of type 'Document'.
--
-- Here's a simple example of using it:
--
-- >>> [html|<html></html>|] :: Document
-- Document {documentPrologue = Prologue {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}, documentRoot = Element {elementName = Name {nameLocalName = "html", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = fromList [], elementNodes = []}, documentEpilogue = []}
--
-- Internally, this function is using the
-- <https://hackage.haskell.org/package/heterocephalus heterocephalus> package.
-- This means you can use variable interpolation, as well as @forall@, @if@,
-- and @case@ control statements.  Checkout the
-- <https://github.com/arowM/heterocephalus#syntax heterocephalus README> for
-- more info.
--
-- >>> let a = "hello world"
-- >>> [html|<html>#{a}</html>|]
-- Document ...
--
-- Even invalid HTML will still parse.
--
-- >>> [html|<html </html>|]
-- Document ...
--
-- Here's an example of a template that can be parsed as an HTML 'Document', but
-- not as an XML 'Document':
--
-- >>> [html|<html><br></html>|]
-- Document ...
html :: QuasiQuoter
html =
  createExpQuasiQuoter $ \string ->
    appE [|parseLT . renderMarkup|] $ compileFromString textSetting string

-- | This function is the same as 'html', but doesn't allow variable
-- interpolation or control statements.  It also produces expressions of type
-- 'Document'.
--
-- Here's a simple example of using it:
--
-- >>> [htmlRaw|<html></html>|] :: Document
-- Document ...
htmlRaw :: QuasiQuoter
htmlRaw = createExpQuasiQuoter $ lift . parseLT . pack
