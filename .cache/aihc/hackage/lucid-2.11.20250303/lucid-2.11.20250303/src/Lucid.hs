-- | Clear to write, read and edit DSL for writing HTML
--
-- See "Lucid.Html5" for a complete list of Html5 combinators. That
-- module is re-exported from this module for your convenience.
--
-- See "Lucid.Base" for lower level functions like
-- `makeElement`, `makeAttribute`, 'termRaw', etc.
--
-- To convert html to the lucid DSL, use the (experimental) program
-- <https://github.com/dbaynard/lucid-from-html lucid-from-html>
-- which may eventually be integrated into lucid itself.

module Lucid
  (-- * Intro
   -- $intro
   renderText
  ,renderBS
  ,renderTextT
  ,renderBST
  ,renderToFile
   -- * Running
   -- $running
  ,execHtmlT
  ,evalHtmlT
  ,runHtmlT
   -- * Types
  ,Html
  ,HtmlT
  ,Attribute
   -- * Classes
   -- $overloaded
  ,Term(..)
  ,ToHtml(..)
  ,With(..)
  -- * Re-exports
  ,module Lucid.Html5)
 where

import Lucid.Base
import Lucid.Html5

-- $intro
--
-- HTML terms in Lucid are written with a postfix ‘@_@’ to indicate data
-- rather than code. Some examples:
--
-- 'p_', 'class_', 'table_', 'style_'
--
-- Note: If you're testing in the REPL you need to add a type annotation to
-- indicate that you want HTML. In normal code your top-level
-- declaration signatures handle that.
--
-- For GHCi:
--
-- @
-- :set -XOverloadedStrings -XExtendedDefaultRules@
-- import Lucid
-- @
--
-- In a module: @{-\# LANGUAGE OverloadedStrings, ExtendedDefaultRules \#-}@
--
-- Plain text is written like this, and is automatically escaped:
--
-- >>> "123 < 456" :: Html ()
-- 123 &lt; 456
--
-- Except some elements, like 'script_':
--
-- >>> script_ "alert('Hello!' > 12)" :: Html ()
-- <script>alert('Hello!' > 12)</script>
--
-- Elements nest by function application:
--
-- >>> table_ (tr_ (td_ (p_ "Hello, World!"))) :: Html ()
-- <table><tr><td><p>Hello, World!</p></td></tr></table>
--
-- Elements are juxtaposed via monoidal append (remember to import "Data.Monoid"):
--
-- >>> p_ "hello" <> p_ "sup" :: Html ()
-- <p>hello</p><p>sup</p>
--
-- Or monadic sequencing:
--
-- >>> div_ (do p_ "hello"; p_ "sup") :: Html ()
-- <div><p>hello</p><p>sup</p></div>
--
-- Attributes are set by providing an argument list:
--
-- >>> p_ [class_ "brand"] "Lucid Inc" :: Html ()
-- <p class="brand">Lucid Inc</p>
--
-- >>> p_ [data_ "zot" "foo",checked_] "Go!" :: Html ()
-- <p data-zot="foo" checked>go</p>
--
-- Attribute and element terms are not conflicting:
--
-- >>> style_ [style_ "inception"] "Go deeper." :: Html ()
-- <style style="inception">Go deeper.</style>
--
-- Here is a fuller example of Lucid:
--
-- @
-- table_ [rows_ "2"]
--        (tr_ (do td_ [class_ "top",colspan_ "2",style_ "color:red"]
--                     (p_ "Hello, attributes!")
--                 td_ "yay!"))
-- @
--
-- Elements (and some attributes) are variadic and overloaded, see the
-- 'Term' class for more explanation about that.
--
-- For proper rendering you can easily run some HTML immediately with:
--
-- >>> renderText (p_ "Hello!")
-- > "<p>Hello!</p>"
--
-- >>> renderBS (p_ [style_ "color:red"] "Hello!")
-- "<p style=\"color:red\">Hello!</p>"
--
-- For ease of use in GHCi, there is a 'Show' instance, as
-- demonstrated above.

-- $overloaded
--
-- To support convenient use of HTML terms, HTML terms are
-- overloaded. Here are the following types possible for an element
-- term accepting attributes and/or children:
--
-- @
-- p_ :: Term arg result => arg -> result
-- p_ :: Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()
-- p_ :: Monad m => HtmlT m () -> HtmlT m ()
-- @
--
-- The first is the generic form. The latter two are the possible
-- types for an element.
--
-- Elements that accept no content are always concrete:
--
-- @
-- input_ :: Monad m => [Attribute] -> HtmlT m ()
-- @
--
-- And some elements share the same name as attributes, so you can
-- also overload them as attributes:
--
-- @
-- style_ :: TermRaw arg result => arg -> result
-- style_ :: Monad m => [Attribute] -> Text -> HtmlT m ()
-- style_ :: Monad m => Text -> HtmlT m ()
-- style_ :: Text -> Attribute
-- @

-- $running
--
-- If the above rendering functions aren't suited for your purpose,
-- you can run the monad directly and use the more low-level blaze
-- `Builder`, which has a plethora of output modes in
-- "Blaze.ByteString.Builder".
