{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TupleSections        #-}
{- |

Marshal values of types that make up 'Inline' elements.
-}
module Text.Pandoc.Lua.Marshal.Inline
  ( typeInline
    -- * Single Inline elements
  , peekInline
  , peekInlineFuzzy
  , pushInline
    -- * List of Inlines
  , peekInlines
  , peekInlinesFuzzy
  , pushInlines
    -- * Constructors
  , inlineConstructors
  , mkInlines
    -- * Walking
  , walkInlineSplicing
  , walkInlinesStraight
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad.Catch (throwM)
import Control.Monad ((<$!>))
import Data.Aeson (encode)
import Data.Data (showConstr, toConstr)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HsLua
import Text.Pandoc.Definition (Inline (..), nullAttr)
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Block (peekBlocksFuzzy)
import Text.Pandoc.Lua.Marshal.Citation (peekCitation, pushCitation)
import Text.Pandoc.Lua.Marshal.Content
  ( Content (..), contentTypeDescription, peekContent, pushContent )
import Text.Pandoc.Lua.Marshal.Filter (Filter, peekFilter)
import Text.Pandoc.Lua.Marshal.Format (peekFormat, pushFormat)
import Text.Pandoc.Lua.Marshal.List (pushPandocList, newListMetatable)
import Text.Pandoc.Lua.Marshal.MathType (peekMathType, pushMathType)
import Text.Pandoc.Lua.Marshal.QuoteType (peekQuoteType, pushQuoteType)
import Text.Pandoc.Lua.Marshal.Shared (walkBlocksAndInlines)
import Text.Pandoc.Lua.Walk (SpliceList, Walkable, walkSplicing, walkStraight)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B

-- | Pushes an Inline value as userdata object.
pushInline :: LuaError e => Pusher e Inline
pushInline = pushUD typeInline
{-# INLINE pushInline #-}

-- | Retrieves an Inline value.
peekInline :: LuaError e => Peeker e Inline
peekInline = peekUD typeInline
{-# INLINE peekInline #-}

-- | Retrieves a list of Inline values.
peekInlines :: LuaError e
            => Peeker e [Inline]
peekInlines = peekList peekInline
{-# INLINABLE peekInlines #-}

-- | Pushes a list of Inline values.
pushInlines :: LuaError e
            => Pusher e [Inline]
pushInlines xs = do
  pushList pushInline xs
  newListMetatable "Inlines" $ do
    pushName "walk"
    pushDocumentedFunction $ lambda
      ### flip walkBlocksAndInlines
      <#> parameter peekInlinesFuzzy "Blocks" "self" ""
      <#> parameter peekFilter "Filter" "lua_filter" "table of filter functions"
      =#> functionResult pushInlines "Blocks" "modified list"
    rawset (nth 3)

    pushName "clone"
    pushDocumentedFunction $ lambda
      ### return
      <#> parameter peekInlinesFuzzy "Inlines" "self" ""
      =#> functionResult pushInlines "Inlines" "deep copy"
    rawset (nth 3)

    pushName "__tostring"
    pushDocumentedFunction $ lambda
      ### liftPure show
      <#> parameter peekInlinesFuzzy "Inlines" "self" ""
      =#> functionResult pushString "string" "native Haskell representation"
    rawset (nth 3)

    pushName "__tojson"
    pushDocumentedFunction $ lambda
      ### liftPure encode
      <#> parameter peekInlinesFuzzy "Inlines" "self" ""
      =#> functionResult pushLazyByteString "string" "JSON representation"
    rawset (nth 3)
  setmetatable (nth 2)
{-# INLINABLE pushInlines #-}

-- | Unmarshal a value as Inline object by first calling the @__toinline@
-- metamethod on that object.
peekInlineMetamethod :: LuaError e
                    => Peeker e Inline
peekInlineMetamethod idx = do
  absidx <- liftLua $ absindex idx
  liftLua (getmetafield absidx "__toinline") >>= \case
    TypeNil      -> failPeek "object has no __toinline metamethod"
    TypeFunction -> do
      liftLua (pushvalue absidx)
      liftLua (pcall 1 1 Nothing) >>= \case
        OK   -> peekInline top `lastly` pop 1
        _err -> do
          msg <- peekByteString top `lastly` pop 1
          failPeek $ "failure in __toinline: " <> msg
    _otherType   -> do
      liftLua (pop 1)   -- drop "__toinline" field
      failPeek "__toinline metafield does not contain a function"

-- | Try extra hard to retrieve an Inline value from the stack. Treats
-- bare strings as @Str@ values.
peekInlineFuzzy :: LuaError e => Peeker e Inline
peekInlineFuzzy idx = retrieving "Inline" $ liftLua (ltype idx) >>= \case
  TypeString   -> Str <$!> peekText idx
  TypeTable    -> peekInlineMetamethod idx <|> peekInline idx
  TypeUserdata -> peekInline idx <|> peekInlineMetamethod idx
  _type        -> failPeek =<<
                  typeMismatchMessage "Inline-ish" idx
{-# INLINABLE peekInlineFuzzy #-}

-- | Try extra-hard to return the value at the given index as a list of
-- inlines.
peekInlinesFuzzy :: LuaError e
                 => Peeker e [Inline]
peekInlinesFuzzy idx = liftLua (ltype idx) >>= \case
  TypeString   -> B.toList . B.text <$> peekText idx
  TypeTable    -> ((:[]) <$> peekInlineMetamethod idx)
                  <|> peekList peekInlineFuzzy idx
  TypeUserdata -> ((:[]) <$> peekInlineFuzzy idx)
  _type        -> failPeek =<<
                  typeMismatchMessage "Inline, list of Inlines, or string" idx
{-# INLINABLE peekInlinesFuzzy #-}

-- | Inline object type.
typeInline :: forall e. LuaError e => DocumentedType e Inline
typeInline = deftype "Inline"
  [ operation Tostring $ lambda
    ### liftPure (show @Inline)
    <#> parameter peekInline "inline" "Inline" "Object"
    =#> functionResult pushString "string" "stringified Inline"
  , operation Eq $ defun "__eq"
      ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
      <#> parameter (optional . peekInline) "a" "Inline" ""
      <#> parameter (optional . peekInline) "b" "Inline" ""
      =#> functionResult pushBool "boolean" "whether the two are equal"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typeInline "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ possibleProperty "attr" "element attributes"
      (pushAttr, \case
          Code attr _    -> Actual attr
          Image attr _ _ -> Actual attr
          Link attr _ _  -> Actual attr
          Span attr _    -> Actual attr
          _              -> Absent)
      (peekAttr, \case
          Code _ cs       -> Actual . (`Code` cs)
          Image _ cpt tgt -> Actual . \attr -> Image attr cpt tgt
          Link _ cpt tgt  -> Actual . \attr -> Link attr cpt tgt
          Span _ inlns    -> Actual . (`Span` inlns)
          _               -> const Absent)

  , possibleProperty "caption" "image caption"
      (pushInlines, \case
          Image _ capt _ -> Actual capt
          _              -> Absent)
      (peekInlinesFuzzy, \case
          Image attr _ target -> Actual . (\capt -> Image attr capt target)
          _                   -> const Absent)

  , possibleProperty "citations" "list of citations"
      (pushPandocList pushCitation, \case
          Cite cs _    -> Actual cs
          _            -> Absent)
      (peekList peekCitation, \case
          Cite _ inlns -> Actual . (`Cite` inlns)
          _            -> const Absent)

  , possibleProperty "content" "element contents"
      (pushContent, \case
          Cite _ inlns      -> Actual $ ContentInlines inlns
          Emph inlns        -> Actual $ ContentInlines inlns
          Link _ inlns _    -> Actual $ ContentInlines inlns
          Quoted _ inlns    -> Actual $ ContentInlines inlns
          SmallCaps inlns   -> Actual $ ContentInlines inlns
          Span _ inlns      -> Actual $ ContentInlines inlns
          Strikeout inlns   -> Actual $ ContentInlines inlns
          Strong inlns      -> Actual $ ContentInlines inlns
          Subscript inlns   -> Actual $ ContentInlines inlns
          Superscript inlns -> Actual $ ContentInlines inlns
          Underline inlns   -> Actual $ ContentInlines inlns
          Note blks         -> Actual $ ContentBlocks blks
          _                 -> Absent)
      (peekContent,
        let inlineContent = \case
              ContentInlines inlns -> inlns
              c -> throwM . luaException @e $
                   "expected Inlines, got " <> contentTypeDescription c
            blockContent = \case
              ContentBlocks blks -> blks
              ContentInlines []  -> []
              c -> throwM . luaException @e $
                   "expected Blocks, got " <> contentTypeDescription c
        in \case
          -- inline content
          Cite cs _     -> Actual . Cite cs . inlineContent
          Emph _        -> Actual . Emph . inlineContent
          Link a _ tgt  -> Actual . (\inlns -> Link a inlns tgt) . inlineContent
          Quoted qt _   -> Actual . Quoted qt . inlineContent
          SmallCaps _   -> Actual . SmallCaps . inlineContent
          Span attr _   -> Actual . Span attr . inlineContent
          Strikeout _   -> Actual . Strikeout . inlineContent
          Strong _      -> Actual . Strong . inlineContent
          Subscript _   -> Actual . Subscript . inlineContent
          Superscript _ -> Actual . Superscript . inlineContent
          Underline _   -> Actual . Underline . inlineContent
          -- block content
          Note _        -> Actual . Note . blockContent
          _             -> const Absent
      )

  , possibleProperty "format" "format of raw text"
      (pushFormat, \case
          RawInline fmt _ -> Actual fmt
          _               -> Absent)
      (peekFormat, \case
          RawInline _ txt -> Actual . (`RawInline` txt)
          _               -> const Absent)

  , possibleProperty "mathtype" "math rendering method"
      (pushMathType, \case
          Math mt _  -> Actual mt
          _          -> Absent)
      (peekMathType, \case
          Math _ txt -> Actual . (`Math` txt)
          _          -> const Absent)

  , possibleProperty "quotetype" "type of quotes (single or double)"
      (pushQuoteType, \case
          Quoted qt _     -> Actual qt
          _               -> Absent)
      (peekQuoteType, \case
          Quoted _ inlns  -> Actual . (`Quoted` inlns)
          _               -> const Absent)

  , possibleProperty "src" "image source"
      (pushText, \case
          Image _ _ (src, _) -> Actual src
          _                  -> Absent)
      (peekText, \case
          Image attr capt (_, title) -> Actual . Image attr capt . (,title)
          _                          -> const Absent)

  , possibleProperty "target" "link target URL"
      (pushText, \case
          Link _ _ (tgt, _) -> Actual tgt
          _                 -> Absent)
      (peekText, \case
          Link attr capt (_, title) -> Actual . Link attr capt . (,title)
          _                         -> const Absent)
  , possibleProperty "title" "title text"
      (pushText, \case
          Image _ _ (_, tit) -> Actual tit
          Link _ _ (_, tit)  -> Actual tit
          _                  -> Absent)
      (peekText, \case
          Image attr capt (src, _) -> Actual . Image attr capt . (src,)
          Link attr capt (src, _)  -> Actual . Link attr capt . (src,)
          _                        -> const Absent)

  , possibleProperty "text" "text contents"
      (pushText, getInlineText)
      (peekText, setInlineText)

  , readonly "tag" "type of Inline"
      (pushString, showConstr . toConstr )

  , alias "t" "tag" ["tag"]
  , alias "c" "content" ["content"]
  , alias "identifier" "element identifier"       ["attr", "identifier"]
  , alias "classes"    "element classes"          ["attr", "classes"]
  , alias "attributes" "other element attributes" ["attr", "attributes"]

  , method $ defun "clone"
      ### return
      <#> parameter peekInline "inline" "Inline" "self"
      =#> functionResult pushInline "Inline" "cloned Inline"

  , method $ defun "walk"
    ### flip walkBlocksAndInlines
    <#> parameter peekInline "Inline" "self" ""
    <#> parameter peekFilter "Filter" "lua_filter" "table of filter functions"
    =#> functionResult pushInline "Inline" "modified element"
  ]

--
-- Text
--

-- | Gets the text property of an Inline, if present.
getInlineText :: Inline -> Possible Text
getInlineText = \case
  Code _ lst      -> Actual lst
  Math _ str      -> Actual str
  RawInline _ raw -> Actual raw
  Str s           -> Actual s
  _               -> Absent

-- | Sets the text property of an Inline, if present.
setInlineText :: Inline -> Text -> Possible Inline
setInlineText = \case
  Code attr _     -> Actual . Code attr
  Math mt _       -> Actual . Math mt
  RawInline f _   -> Actual . RawInline f
  Str _           -> Actual . Str
  _               -> const Absent

-- | Constructor functions for 'Inline' elements.
inlineConstructors :: LuaError e =>  [DocumentedFunction e]
inlineConstructors =
  [ defun "Cite"
    ### liftPure2 (flip Cite)
    <#> parameter peekInlinesFuzzy "Inlines" "content" "placeholder content"
    <#> parameter (peekList peekCitation) "{Citation,...}" "citations"
        "List of Citations"
    =#> functionResult pushInline "Inline" "cite element"
    #? "Creates a Cite inline element"
  , defun "Code"
    ### liftPure2 (\text mattr -> Code (fromMaybe nullAttr mattr) text)
    <#> textParam "code" "code string"
    <#> opt (parameter peekAttr "Attr" "attr" "additional attributes")
    =#> functionResult pushInline "Inline" "code element"
    #? "Creates a Code inline element"
  , mkInlinesConstr "Emph" Emph
    #? "Creates an inline element representing emphasized text."
  , defun "Image"
    ### liftPure4 (\caption src mtitle mattr ->
                     let attr = fromMaybe nullAttr mattr
                         title = fromMaybe mempty mtitle
                     in Image attr caption (src, title))
    <#> parameter peekInlinesFuzzy "Inlines" "caption"
        "text used to describe the image"
    <#> textParam "src" "path to the image file"
    <#> opt (textParam "title" "brief image description")
    <#> opt (parameter peekAttr "Attr" "attr" "image attributes")
    =#> functionResult pushInline "Inline" "Image element"
    #? "Creates an Image element"
  , defun "LineBreak"
    ### return LineBreak
    =#> functionResult pushInline "Inline" "line break"
    #? "Create a LineBreak inline element"
  , defun "Link"
    ### liftPure4 (\content target mtitle mattr ->
                     let attr = fromMaybe nullAttr mattr
                         title = fromMaybe mempty mtitle
                     in Link attr content (target, title))
    <#> parameter peekInlinesFuzzy "Inlines" "content" "text for this link"
    <#> textParam "target" "the link target"
    <#> opt (textParam "title" "brief link description")
    <#> opt (parameter peekAttr "Attr" "attr" "link attributes")
    =#> functionResult pushInline "Inline" "link element"
    #? "Creates a link inline element, usually a hyperlink."
  , defun "Math"
    ### liftPure2 Math
    <#> parameter peekMathType "MathType" "mathtype" "rendering specifier"
    <#> textParam "text" "math content"
    =#> functionResult pushInline "Inline" "math element"
    #? "Creates a Math element, either inline or displayed."
  , defun "Note"
    ### liftPure Note
    <#> parameter peekBlocksFuzzy "Blocks" "content" "footnote block content"
    =#> functionResult pushInline "Inline" "note"
    #? "Creates a Note inline element"
  , defun "Quoted"
    ### liftPure2 Quoted
    <#> parameter peekQuoteType "QuoteType" "quotetype" "type of quotes"
    <#> parameter peekInlinesFuzzy "Inlines" "content" "inlines in quotes"
    =#> functionResult pushInline "Inline" "quoted element"
    #? ("Creates a Quoted inline element given the quote type and " <>
        "quoted content.")
  , defun "RawInline"
    ### liftPure2 RawInline
    <#> parameter peekFormat "string" "format" "format of content"
    <#> textParam "text" "string content"
    =#> functionResult pushInline "Inline" "raw inline element"
    #? "Creates a raw inline element"
  , mkInlinesConstr "SmallCaps" SmallCaps
    #? "Creates text rendered in small caps"
  , defun "SoftBreak"
    ### return SoftBreak
    =#> functionResult pushInline "Inline" "soft break"
    #? "Creates a SoftBreak inline element."
  , defun "Space"
    ### return Space
    =#> functionResult pushInline "Inline" "new space"
    #? "Create a Space inline element"
  , defun "Span"
    ### liftPure2 (\inlns mattr -> Span (fromMaybe nullAttr mattr) inlns)
    <#> parameter peekInlinesFuzzy "Inlines" "content" "inline content"
    <#> opt (parameter peekAttr "Attr" "attr" "additional attributes")
    =#> functionResult pushInline "Inline" "[[Span]] object"
    #? "Creates a Span inline element"
  , defun "Str"
    ### liftPure Str
    <#> textParam "text" ""
    =#> functionResult pushInline "Inline" "[[Str]] object"
    #? "Creates a Str inline element"
  , mkInlinesConstr "Strikeout" Strikeout
    #? "Creates text which is struck out."
  , mkInlinesConstr "Strong" Strong
    #? ("Creates a Strong element, whose text is usually displayed in " <>
        "a bold font.")
  , mkInlinesConstr "Subscript" Subscript
    #? "Creates a Subscript inline element"
  , mkInlinesConstr "Superscript" Superscript
    #? "Creates a Superscript inline element"
  , mkInlinesConstr "Underline" Underline
    #? "Creates an Underline inline element"
  ]
 where
   mkInlinesConstr name constr = defun name
     ### liftPure (\x -> x `seq` constr x)
     <#> parameter peekInlinesFuzzy "Inlines" "content" "inline content"
     =#> functionResult pushInline "Inline" "new object"

-- | Constructor for a list of `Inline` values.
mkInlines :: LuaError e => DocumentedFunction e
mkInlines = defun "Inlines"
  ### liftPure id
  <#> parameter peekInlinesFuzzy "Inlines" "inline_like_elements"
      ("List where each element can be treated as an [[Inline]] " <>
       "value, or just a single such value.")
  =#> functionResult pushInlines "Inlines" "list of inline elements"
  #? T.unlines
  [ "Converts its argument into an [[Inlines]] list:"
  , ""
  , "-   copies a list of [[Inline]] elements into a fresh list; any"
  , "    string `s` within the list is treated as `pandoc.Str(s)`;"
  , "-   turns a single [[Inline]] into a singleton list;"
  , "-   splits a string into `Str`-wrapped words, treating"
  , "    interword spaces as `Space`s or `SoftBreak`s."
  ]

-- | Walks an element of type @a@ and applies the filter to all 'Inline'
-- elements.  The filter result is spliced back into the list.
walkInlineSplicing :: (LuaError e, Walkable (SpliceList Inline) a)
                   => Filter -> a -> LuaE e a
walkInlineSplicing = walkSplicing pushInline peekInlinesFuzzy

-- | Walks an element of type @a@ and applies the filter to all lists of
-- 'Inline' elements.
walkInlinesStraight :: (LuaError e, Walkable [Inline] a)
                    => Filter -> a -> LuaE e a
walkInlinesStraight = walkStraight "Inlines" pushInlines peekInlinesFuzzy
