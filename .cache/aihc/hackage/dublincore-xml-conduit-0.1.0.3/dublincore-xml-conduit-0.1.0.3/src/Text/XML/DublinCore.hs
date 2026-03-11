{-# LANGUAGE OverloadedStrings #-}

-- | Common XML definitions for the __Dublin Core Metadata Element Set__.
--
-- This module is meant to be imported qualified, like this:
--
-- > import qualified Text.XML.DublinCore as DC
module Text.XML.DublinCore where

-- {{{ Imports
import Data.Text
import URI.ByteString

-- }}}

-- | Standard XML prefix is @dc@.
namespacePrefix :: Text
namespacePrefix = "dc"

-- | Standard XML namespace is @http://purl.org/dc/elements/1.1/@.
namespaceURI :: URIRef Absolute
namespaceURI = uri where Right uri = parseURI laxURIParserOptions "http://purl.org/dc/elements/1.1/"
