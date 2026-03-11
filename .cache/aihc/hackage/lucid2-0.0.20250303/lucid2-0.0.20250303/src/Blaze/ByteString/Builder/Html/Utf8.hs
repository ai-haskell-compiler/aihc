{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# OPTIONS_GHC -fsimpl-tick-factor=40000 #-}
#endif

------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Html.Utf8
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  https://github.com/blaze-builder
-- Stability:   stable
--
-- 'Write's and 'Builder's for serializing HTML escaped and UTF-8 encoded
-- characters.
--
-- This module is used by both the 'blaze-html' and the \'hamlet\' HTML
-- templating libraries. If the 'Builder' from 'blaze-builder' replaces the
-- 'Data.Binary.Builder' implementation, this module will most likely keep its
-- place, as it provides a set of very specialized functions.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Html.Utf8
    (
      module Blaze.ByteString.Builder.Char.Utf8

      -- * Creating Builders from HTML escaped and UTF-8 encoded characters
    , fromHtmlEscapedChar
    , fromHtmlEscapedString
    , fromHtmlEscapedShow
    , fromHtmlEscapedText
    , fromHtmlEscapedLazyText
    ) where

import Data.ByteString.Char8 ()  -- for the 'IsString' instance of bytesrings

import qualified Data.Text      as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import qualified Data.ByteString.Builder       as B
import           Data.ByteString.Builder.Prim ((>*<), (>$<), condB)
import qualified Data.ByteString.Builder.Prim  as P

import Blaze.ByteString.Builder.Char.Utf8
import Blaze.ByteString.Builder.Html.Word

-- | /O(1)./ Serialize a HTML escaped Unicode character using the UTF-8
-- encoding.
fromHtmlEscapedChar :: Char -> B.Builder
fromHtmlEscapedChar = P.primBounded charUtf8HtmlEscaped
{-# INLINE fromHtmlEscapedChar #-}

{-# INLINE charUtf8HtmlEscaped #-}
charUtf8HtmlEscaped :: P.BoundedPrim Char
charUtf8HtmlEscaped =
    condB (>  '>' ) (condB (== '\DEL') P.emptyB P.charUtf8) $
    condB (== '<' ) (fixed4 ('&',('l',('t',';')))) $        -- &lt;
    condB (== '>' ) (fixed4 ('&',('g',('t',';')))) $        -- &gt;
    condB (== '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $  -- &amp;
    condB (== '"' ) (fixed6 ('&',('q',('u',('o',('t',';')))))) $  -- &#quot;
    condB (== '\'') (fixed5 ('&',('#',('3',('9',';'))))) $  -- &#39;
    condB (\c -> c >= ' ' || c == '\t' || c == '\n' || c == '\r')
          (P.liftFixedToBounded P.char7) $
    P.emptyB
  where
    {-# INLINE fixed4 #-}
    fixed4 x = P.liftFixedToBounded $ const x >$<
      P.char7 >*< P.char7 >*< P.char7 >*< P.char7

    {-# INLINE fixed5 #-}
    fixed5 x = P.liftFixedToBounded $ const x >$<
      P.char7 >*< P.char7 >*< P.char7 >*< P.char7 >*< P.char7

    {-# INLINE fixed6 #-}
    fixed6 x = P.liftFixedToBounded $ const x >$<
      P.char7 >*< P.char7 >*< P.char7 >*< P.char7 >*< P.char7 >*< P.char7

-- | /O(n)/. Serialize a HTML escaped Unicode 'String' using the UTF-8
-- encoding.
--
fromHtmlEscapedString :: String -> B.Builder
fromHtmlEscapedString = P.primMapListBounded charUtf8HtmlEscaped

-- | /O(n)/. Serialize a value by 'Show'ing it and then, HTML escaping and
-- UTF-8 encoding the resulting 'String'.
--
fromHtmlEscapedShow :: Show a => a -> B.Builder
fromHtmlEscapedShow = fromHtmlEscapedString . show

-- | /O(n)/. Serialize a HTML escaped strict Unicode 'TS.Text' value using the
-- UTF-8 encoding.
--
fromHtmlEscapedText :: TS.Text -> B.Builder
#if MIN_VERSION_text(1,1,2) && MIN_VERSION_bytestring(0,10,4)
fromHtmlEscapedText = TE.encodeUtf8BuilderEscaped wordHtmlEscaped
#else
fromHtmlEscapedText = fromHtmlEscapedString . TS.unpack
#endif

-- | /O(n)/. Serialize a HTML escaped Unicode 'TL.Text' using the UTF-8 encoding.
--
fromHtmlEscapedLazyText :: TL.Text -> B.Builder
#if MIN_VERSION_text(1,1,2) && MIN_VERSION_bytestring(0,10,4)
fromHtmlEscapedLazyText = TLE.encodeUtf8BuilderEscaped wordHtmlEscaped
#else
fromHtmlEscapedLazyText = fromHtmlEscapedString . TL.unpack
#endif
