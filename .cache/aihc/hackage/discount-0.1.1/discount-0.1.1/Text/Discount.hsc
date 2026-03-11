{-# LANGUAGE ForeignFunctionInterface #-}
-- | This module is a thin wrapper around the discount
-- Markdown-processing library, by David Parsons
-- <http://www.pell.portland.or.us/~orc/Code/discount/>. It exposes
-- options that can be passed to the parser, as well as 'ByteString'
-- and 'Text' interfaces to the parser itself.

module Text.Discount (
    DiscountOption
  , module Text.Discount
  ) where

#include <mkdio.h>

import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Discount.Internal

#{let mkopt opt,val = opt ":: DiscountOption;\n" opt " = DiscountOption %d", val}

-- * Parser interface
-- | Convert the ByteString String input into well-formed HTML
-- output. Note that an empty set of flags will not enable "strict"
-- markdown behavior; instead, use 'compatOptions', which will cause
-- discount to pass the markdown tests.
parseMarkdown :: [DiscountOption] -> ByteString -> ByteString
parseMarkdown opts markdown = unsafePerformIO . alloca $ \out_buf -> useAsCStringLen markdown $ \(markdown_c, len) -> do
  mmioptr <- mkd_string markdown_c (toEnum len) flag
  mkd_compile mmioptr flag
  mkd_document mmioptr out_buf
  result <- peek out_buf >>= packCString
  mkd_cleanup mmioptr
  return result

  where flag = unDiscountOption $ combineOptions opts

-- | As 'parseMarkdown', but taking 'Text' values instead. Uses UTF-8 internally.
parseMarkdownUtf8 :: [DiscountOption] -> Text -> Text
parseMarkdownUtf8 opts = decodeUtf8 . parseMarkdown opts . encodeUtf8

-- * Parser options
-- | Disables processing of links. Note that this will produce invalid
-- HTML due to a bug in discount!
#mkopt "noLinks", MKD_NOLINKS

-- | Disables image processing. Note that this will produce invalid
-- HTML due to a bug in discount!
#mkopt "noImages", MKD_NOIMAGE

-- | Disables SmartyPants processing. SmartyPants replaces quotes with
-- curly quotes (except in code blocks), replaces @(tm)@, @(r)@, and
-- @(c)@ with the relevant symbols, and replaces ellipses and
-- em/en-dashes with the appropriate symbols.
#mkopt "noSmartyPants", MKD_NOPANTS

-- | Disables raw HTML. Note that this will produce invalid HTML due
-- to a bug in discount!
#mkopt "noHtml", MKD_NOHTML

-- | Disables both superscript and relaxed emphasis (see 'noRelaxedEmphasis').
#mkopt "strict", MKD_STRICT

-- | Disable pseudoprotocol wrapping. If this is not enabled, then
-- links of the form @[foo bar](class:glarch)@ will be replaced by
-- @\<span class=\"glarch\"\>foo bar\</span\>@, and similarly for
-- @abbr:desc@ (uses @\<abbr title=\"desc\"\>@) and @id:name@ (uses @\<a
-- id=\"name\"\>@)
#mkopt "noPseudoProtocols", MKD_NO_EXT

-- | Disables converstion of @A^B@ into @A\<sup\>B\</sup\>@.
#mkopt "noSuperscripts", MKD_NOSUPERSCRIPT

-- | Disables relaxed emphasis, allowing underscores to indicate
-- emphasis in the middle of a word. With relaxed emphasis on
-- (i.e. without this option) @foo_bar_@ will parse as
-- @foo_bar_@. With it off, it parses as @foo\<em\>bar\</em\>@.
#mkopt "noRelaxedEmphasis", MKD_NORELAXED

-- | Disables PHP Markdown Extra-style tables. See the documentation
-- on PHP Markdown Extra at
-- <http://michelf.com/projects/php-markdown/extra/#table>.
#mkopt "noTables", MKD_NOTABLES

-- | Disables @~~strikethrough~~@.
#mkopt "noStrikethrough", MKD_NOSTRIKETHROUGH


-- | Disables Pandoc-style header processing. This does not disable
-- headers like
--
-- > This
-- > ====
-- > # or this

#mkopt "noHeaders", MKD_NOHEADER

-- | Disables div-style quotes. Div-style quotes translates
--
-- > > %class%
-- > > foo
--
-- as @\<div class=\"class\"\>foo\</div\>@.
#mkopt "noDivQuotes", MKD_NODIVQUOTE

-- | Disables alphanumeric-ordered lists.
#mkopt "noAlphaLists", MKD_NOALPHALIST


-- | Disables definition lists.
#mkopt "noDefinitionLists", MKD_NODLIST

-- | Process Markdown even inside an HTML tag.
#mkopt "tagText", MKD_TAGTEXT

-- | Only allow links that are local or that point to @http@, @https@,
-- @news@, or @ftp@ schemes.
#mkopt "safeLinks", MKD_SAFELINK

-- | Expand tabs to 4 spaces.
#mkopt "tabStop", MKD_TABSTOP

-- | Enable Markdown Extra style footnotes. See
-- <http://michelf.com/projects/php-markdown/extra/#footnotes>. For example:
--
-- > Here's some text with a footnote.[^1]
-- >
-- > [^1]: Here's a footnote with some text.
--
-- Footnotes have backlinks to their parent.
#mkopt "footnotes", MKD_EXTRA_FOOTNOTE

-- | Disables all discount features not in the original Markdown spec:
-- SmartyPants, relaxed emphasis, pseudo-protocols, strikethrough,
-- headers, alphabetical lists, definition lists, superscripts, and
-- tables.
compatOptions :: [DiscountOption]
compatOptions = [noSmartyPants, noRelaxedEmphasis, noPseudoProtocols, noStrikethrough, noHeaders, noAlphaLists, noDefinitionLists, noSuperscripts, noTables]

