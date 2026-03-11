{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : HsLua.Module.DocLayout
Copyright   : © 2020-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Provides a Lua module which wraps @'Text.DocLayout'@. The @'Doc'@
type is specialized to @'Text'@.

This module defines orphan instances for @Doc Text@.
-}
module HsLua.Module.DocLayout (
  -- * Module
    documentedModule
  , pushModule
  , preloadModule
  , description
  , fields
  , functions

  -- * Doc constructors and combinators
  , after_break
  , before_non_blank
  , blankline
  , blanklines
  , braces
  , brackets
  , cblock
  , chomp
  , concat
  , cr
  , double_quotes
  , empty
  , flush
  , hang
  , inside
  , lblock
  , literal
  , nest
  , nestle
  , nowrap
  , parens
  , prefixed
  , quotes
  , rblock
  , space
  , vfill

  -- * Rendering
  , render

  -- * Document Querying
  , is_empty
  , height
  , min_offset
  , offset
  , real_length
  , update_column

  -- * Styling
  , bold
  , italic
  , underlined
  , strikeout
  , fg
  , bg

  -- * Marshaling
  , peekDoc
  , pushDoc
  )
where

import Prelude hiding (concat)
import Data.List (intersperse)
import Data.Text (Text)
import HsLua as Lua hiding (concat)
import Text.DocLayout (Doc, (<+>), ($$), ($+$))

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.DocLayout as Doc

--
-- Module
--

-- | Textual description of the "doclayout" module.
description :: Text
description = "Plain-text document layouting."

-- | Self-documenting module.
documentedModule :: LuaError e => Module e
documentedModule = Module
  { moduleName = "doclayout"
  , moduleFields = fields
  , moduleDescription = description
  , moduleFunctions = functions
  , moduleOperations = []
  , moduleTypeInitializers = [initType typeDoc]
  }

--
-- Fields
--

-- | Exposed fields.
fields :: forall e. LuaError e => [Field e]
fields =
  [ blankline
  , cr
  , empty
  , space
  ]

-- | Wrapped and documented 'Doc.blankline' value.
blankline :: forall e. LuaError e => Field e
blankline = Field
  { fieldName = "blankline"
  , fieldDescription = "Inserts a blank line unless one exists already."
  , fieldType = udTypeSpec @e typeDoc
  , fieldPushValue = pushDoc Doc.blankline
  }

-- | Wrapped and documented 'Doc.cr' value.
cr :: forall e. LuaError e => Field e
cr = Field
  { fieldName = "cr"
  , fieldDescription = "A carriage return. Does nothing if we're at " <>
                       "the beginning of a line; " <>
                       "otherwise inserts a newline."
  , fieldType = udTypeSpec @e typeDoc
  , fieldPushValue = pushDoc Doc.cr
  }

-- | Wrapped and documented 'Doc.empty' value.
empty :: forall e. LuaError e => Field e
empty = Field
  { fieldName = "empty"
  , fieldDescription = "The empty document."
  , fieldType = udTypeSpec @e typeDoc
  , fieldPushValue = pushDoc Doc.empty
  }

-- | Wrapped and documented 'Doc.space' value.
space :: forall e. LuaError e => Field e
space = Field
  { fieldName = "space"
  , fieldDescription = "A breaking (reflowable) space."
  , fieldType = udTypeSpec @e typeDoc
  , fieldPushValue = pushDoc Doc.space
  }

--
-- Functions
--

-- | Exposed module functions.
functions :: LuaError e => [DocumentedFunction e]
functions =
  [ -- Constructors
    after_break
  , before_non_blank
  , blanklines
  , braces
  , brackets
  , cblock
  , chomp
  , concat
  , double_quotes
  , flush
  , hang
  , inside
  , lblock
  , literal
  , nest
  , nestle
  , nowrap
  , parens
  , prefixed
  , quotes
  , rblock
  , vfill
    -- rendering
  , render
    -- querying
  , is_empty
  , height
  , min_offset
  , offset
  , real_length
  , update_column
    -- styling
  , bold
  , italic
  , underlined
  , strikeout
  , fg
  , bg
  ]

typeDoc :: LuaError e => DocumentedType e (Doc Text)
typeDoc = deftype "Doc"
      [ operation Add    $ binaryOp (<+>)
        "Concatenated docs, with breakable space between them."
      , operation Concat $ binaryOp (<>) "Concatenation of the input docs"
      , operation Div    $ binaryOp ($$) "Puts a above b"
      , operation Eq     $ lambda
        ### liftPure2 (==)
        <#> docParam "a"
        <#> docParam "b"
        =#> boolResult "whether the two Docs are equal"
      , operation Idiv   $ binaryOp ($+$) "Puts a above b"
      , operation Tostring $ lambda
        ### liftPure (Doc.render Nothing)
        <#> docParam "doc"
        =#> textResult "Rendered Doc without reflowing."
      ]
      [ method before_non_blank
      , method braces
      , method brackets
      , method cblock
      , method chomp
      , method double_quotes
      , method is_empty
      , method flush
      , method hang
      , method height
      , method inside
      , method lblock
      , method min_offset
      , method nest
      , method nestle
      , method nowrap
      , method offset
      , method parens
      , method prefixed
      , method quotes
      , method rblock
      , method render
      , method update_column
      , method vfill
      ]
  where
    binaryOp op descr = lambda
      ### liftPure2 op
      <#> docParam "a"
      <#> docParam "b"
      =#> docResult descr

-- | Render a @'Doc'@. The text is reflowed on breakable spaces
-- to match the given line length. Text is not reflowed if the
-- line length parameter is omitted or nil.
render :: LuaError e => DocumentedFunction e
render = defun "render"
  ### (\doc mbcolwidth useAnsi ->
          if useAnsi == Just True
          then pure $ TL.toStrict $ Doc.renderANSI mbcolwidth doc
          else pure $ Doc.renderPlain mbcolwidth doc)
  <#> docParam "doc"
  <#> opt (integralParam "colwidth" $
           "Maximum number of characters per line.\n" <>
           "A value of `nil`, the default, means that the text " <>
           "is not reflown.")
  <#> opt (parameter peekRenderStyle "string" "style" $
           "Whether to generate plain text or ANSI terminal output.\n" <>
           "Must be either `'plain'` or `'ansi'`.\n" <>
           "Defaults to `'plain'`.")
  =#> functionResult pushText "string" "rendered doc"
  #? T.unlines
     [ "Render a [[Doc]]. The text is reflowed on breakable spaces to"
     , "match the given line length. Text is not reflowed if the line"
     , "line length parameter is omitted or nil."
     ]
 where
   peekRenderStyle idx = peekByteString idx >>= \case
     "ansi"  -> pure True
     "ANSI"  -> pure True
     "plain" -> pure False
     style   -> failPeek $ "Unknown rendering style: " <> style

--
-- Querying
--

-- | @True@ iff the document is empty.
is_empty :: LuaError e => DocumentedFunction e
is_empty = defun "is_empty"
  ### liftPure Doc.isEmpty
  <#> docParam "doc"
  =#> boolResult "`true` iff `doc` is the empty document, `false` otherwise."
  #? "Checks whether a doc is empty."

-- | Returns the width of a @'Doc'@.
offset :: LuaError e => DocumentedFunction e
offset = defun "offset"
  ### liftPure Doc.offset
  <#> docParam "doc"
  =#> integralResult "doc width"
  #? "Returns the width of a [[Doc]] as number of characters."

-- | Returns the minimal width of a @'Doc'@ when reflowed at
-- breakable spaces.
min_offset :: LuaError e => DocumentedFunction e
min_offset = defun "min_offset"
  ### liftPure Doc.minOffset
  <#> docParam "doc"
  =#> integralResult "minimal possible width"
  #? ("Returns the minimal width of a [[Doc]] when reflowed at " <>
      "breakable spaces.")

-- | Returns the column that would be occupied by the last laid
-- out character.
update_column :: LuaError e => DocumentedFunction e
update_column = defun "update_column"
  ### liftPure2 Doc.updateColumn
  <#> docParam "doc"
  <#> integralParam "i" "start column"
  =#> integralResult "column number"
  #? ("Returns the column that would be occupied by the last " <>
      "laid out character.")

-- | Returns the height of a block or other Doc.
height :: LuaError e => DocumentedFunction e
height = defun "height"
  ### liftPure Doc.height
  <#> docParam "doc"
  =#> integralResult "doc height"
  #? "Returns the height of a block or other Doc."


-- | Returns the real length of a string in a monospace font: 0
-- for a combining character, 1, for a regular character, 2 for
-- an East Asian wide character.
real_length :: DocumentedFunction e
real_length = defun "real_length"
  ### liftPure Doc.realLength
  <#> textParam "str" "UTF-8 string to measure"
  =#> integralResult "text length"
  #? ("Returns the real length of a string in a monospace font: " <>
      "0 for a combining character, 1 for a regular character, " <>
      "2 for an East Asian wide character.")

--
-- Constructors
--

-- | Creates a @'Doc'@ which is conditionally included only if it
-- comes at the beginning of a line.
after_break :: LuaError e => DocumentedFunction e
after_break = defun "after_break"
  ### liftPure Doc.afterBreak
  <#> textParam "text" "content to include when placed after a break"
  =#> docResult "new doc"
  #? ("Creates a [[Doc]] which is conditionally included only if it " <>
      "comes at the beginning of a line.\n\n" <>
      "An example where this is useful is for escaping line-initial " <>
      "`.` in roff man.")

-- | Conditionally includes the given @'Doc'@ unless it is
-- followed by a blank space.
before_non_blank :: LuaError e => DocumentedFunction e
before_non_blank = defun "before_non_blank"
  ### liftPure Doc.beforeNonBlank
  <#> docParam "doc"
  =#> docResult "conditional doc"
  #? ("Conditionally includes the given `doc` unless it is " <>
      "followed by a blank space.")

-- | Insert blank lines unless they exist already.
blanklines :: LuaError e => DocumentedFunction e
blanklines = defun "blanklines"
  ### liftPure Doc.blanklines
  <#> integralParam "n" "number of blank lines"
  =#> docResult "conditional blank lines"
  #? "Inserts blank lines unless they exist already."

-- | Puts a @'Doc'@ in curly braces.
braces :: LuaError e => DocumentedFunction e
braces = defun "braces"
  ### liftPure Doc.braces
  <#> docParam "doc"
  =#> docResult "`doc` enclosed by {}."
  #? "Puts the `doc` in curly braces."

-- | Puts a @'Doc'@ in square brackets.
brackets :: LuaError e => DocumentedFunction e
brackets = defun "brackets"
  ### liftPure Doc.brackets
  <#> docParam "doc"
  =#> docResult "doc enclosed by []."
  #? "Puts the `doc` in square brackets"

-- | Like @'lblock'@ but aligned centered.
cblock :: LuaError e => DocumentedFunction e
cblock = defun "cblock"
  ### liftPure2 (flip Doc.cblock)
  <#> docParam "doc"
  <#> integralParam "width" "block width in chars"
  =#> docResult ("doc, aligned centered in a block with max " <>
                 "`width` chars per line.")
  #? ("Creates a block with the given width and content, " <>
      "aligned centered.")

-- | Chomps trailing blank space off of a @'Doc'@.
chomp :: LuaError e => DocumentedFunction e
chomp = defun "chomp"
  ### liftPure Doc.chomp
  <#> docParam "doc"
  =#> docResult "`doc` without trailing blanks"
  #? "Chomps trailing blank space off of the `doc`."

-- | Concatenates a list of @'Doc'@s.
concat :: LuaError e => DocumentedFunction e
concat = defun "concat"
  ### liftPure2 (\docs optSep -> mconcat $
                  maybe docs (`intersperse` docs) optSep)
  <#> parameter (peekList peekDoc) "`{Doc,...}`" "docs" "list of Docs"
  <#> opt (parameter peekDoc "Doc" "sep" "separator (default: none)")
  =#> docResult "concatenated doc"
  #? "Concatenates a list of `Doc`s."

-- | Wraps a @'Doc'@ in double quotes
double_quotes :: LuaError e => DocumentedFunction e
double_quotes = defun "double_quotes"
  ### liftPure Doc.doubleQuotes
  <#> docParam "doc"
  =#> docResult "`doc` enclosed by `\"` chars"
  #? "Wraps a `Doc` in double quotes."

-- | Makes a @'Doc'@ flush against the left margin.
flush :: LuaError e => DocumentedFunction e
flush = defun "flush"
  ### liftPure Doc.flush
  <#> docParam "doc"
  =#> docResult "flushed `doc`"
  #? "Makes a `Doc` flush against the left margin."

-- | Creates a hanging indent.
hang :: LuaError e => DocumentedFunction e
hang = defun "hang"
  ### liftPure3 (\doc ind start -> Doc.hang ind start doc)
  <#> docParam "doc"
  <#> integralParam "ind" "indentation width"
  <#> docParam "start"
  =#> docResult ("`doc` prefixed by `start` on the first line, " <>
                 "subsequent lines indented by `ind` spaces.")
  #? "Creates a hanging indent."

-- | Encloses a @'Doc'@ inside a start and end @'Doc'@.
inside :: LuaError e => DocumentedFunction e
inside = defun "inside"
  ### liftPure3 (\contents start end -> Doc.inside start end contents)
  <#> docParam "contents"
  <#> docParam "start"
  <#> docParam "end"
  =#> docResult "enclosed contents"
  #? "Encloses a [[Doc]] inside a start and end [[Doc]]."

-- | Creates a block with the given width and content, aligned to
-- the left.
lblock :: LuaError e => DocumentedFunction e
lblock = defun "lblock"
  ### liftPure2 (flip Doc.lblock)
  <#> docParam "doc"
  <#> integralParam "width" "block width in chars"
  =#> docResult "doc put into block with max `width` chars per line."
  #? ("Creates a block with the given width and content, " <>
      "aligned to the left.")

-- | Creates a @'Doc'@ from a string.
literal :: LuaError e => DocumentedFunction e
literal = defun "literal"
  ### liftPure Doc.literal
  <#> textParam "text" "literal value"
  =#> docResult "doc contatining just the literal string"
  #? "Creates a `Doc` from a string."

-- | Indents a @'Doc'@ by the specified number of spaces.
nest :: LuaError e => DocumentedFunction e
nest = defun "nest"
  ### liftPure2 (flip Doc.nest)
  <#> docParam "doc"
  <#> integralParam "ind" "indentation size"
  =#> docResult "`doc` indented by `ind` spaces"
  #? "Indents a `Doc` by the specified number of spaces."

-- | Removes leading blank lines from a @'Doc'@.
nestle :: LuaError e => DocumentedFunction e
nestle = defun "nestle"
  ### liftPure Doc.nestle
  <#> docParam "doc"
  =#> docResult "`doc` with leading blanks removed"
  #? "Removes leading blank lines from a `Doc`."

-- | Makes a @'Doc'@ non-reflowable.
nowrap :: LuaError e => DocumentedFunction e
nowrap = defun "nowrap"
  ### liftPure Doc.nowrap
  <#> docParam "doc"
  =#> docResult "same as input, but non-reflowable"
  #? "Makes a `Doc` non-reflowable."

-- | Puts a @'Doc'@ in parentheses.
parens :: LuaError e => DocumentedFunction e
parens = defun "parens"
  ### liftPure Doc.parens
  <#> docParam "doc"
  =#> docResult "doc enclosed by ()."
  #? "Puts the `doc` in parentheses."


-- | Uses the specified string as a prefix for every line of the
-- inside document (except the first, if not at the beginning of
-- the line).
prefixed :: LuaError e => DocumentedFunction e
prefixed = defun "prefixed"
  ### liftPure2 (flip Doc.prefixed)
  <#> docParam "doc"
  <#> stringParam "prefix" "prefix for each line"
  =#> docResult "prefixed `doc`"
  #? ("Uses the specified string as a prefix for every line of " <>
      "the inside document (except the first, if not at the " <>
      "beginning of the line).")

-- | Wraps a @'Doc'@ in single quotes.
quotes :: LuaError e => DocumentedFunction e
quotes = defun "quotes"
  ### liftPure Doc.quotes
  <#> docParam "doc"
  =#> docResult "doc enclosed in `'`."
  #? "Wraps a `Doc` in single quotes."

-- | Like @'rblock'@ but aligned to the right.
rblock :: LuaError e => DocumentedFunction e
rblock = defun "rblock"
  ### liftPure2 (flip Doc.rblock)
  <#> docParam "doc"
  <#> integralParam "width" "block width in chars"
  =#> docResult ("doc, right aligned in a block with max " <>
                 "`width` chars per line.")
  #? ("Creates a block with the given width and content, " <>
      "aligned to the right.")

-- | An expandable border that, when placed next to a box,
-- expands to the height of the box.  Strings cycle through the
-- list provided.
vfill :: LuaError e => DocumentedFunction e
vfill = defun "vfill"
  ### liftPure Doc.vfill
  <#> textParam "border" "vertically expanded characters"
  =#> docResult "automatically expanding border Doc"
  #? ("An expandable border that, when placed next to a box, " <>
      "expands to the height of the box.  Strings cycle through the " <>
      "list provided.")

--
-- Styling
--

bold :: LuaError e => DocumentedFunction e
bold = defun "bold"
  ### liftPure Doc.bold
  <#> docParam "doc"
  =#> docResult "bolded Doc"
  #? "Puts a [[Doc]] in boldface."

italic :: LuaError e => DocumentedFunction e
italic = defun "italic"
  ### liftPure Doc.italic
  <#> docParam "doc"
  =#> docResult "styled Doc"
  #? "Puts a [[Doc]] in italics."

underlined :: LuaError e => DocumentedFunction e
underlined = defun "underlined"
  ### liftPure Doc.underlined
  <#> docParam "doc"
  =#> docResult "styled Doc"
  #? "Underlines a [[Doc]]."

strikeout :: LuaError e => DocumentedFunction e
strikeout = defun "strikeout"
  ### liftPure Doc.strikeout
  <#> docParam "doc"
  =#> docResult "styled Doc"
  #? "Puts a line through the [[Doc]]."

fg :: LuaError e => DocumentedFunction e
fg = defun "fg"
  ### liftPure2 (flip Doc.fg)
  <#> docParam "doc"
  <#> colorParam
  =#> docResult "styled Doc"
  #? "Set the foreground color."

bg :: LuaError e => DocumentedFunction e
bg = defun "bg"
  ### liftPure2 (flip Doc.bg)
  <#> docParam "doc"
  <#> colorParam
  =#> docResult "styled Doc"
  #? "Set the background color."

--
-- Marshaling
--

-- | Retrieve a @Doc Text@ value from the Lua stack. Strings are
-- converted to plain @'Doc'@ values.
peekDoc :: LuaError e => Peeker e (Doc Text)
peekDoc idx = liftLua (Lua.ltype idx) >>= \case
  Lua.TypeString   -> let stringToDoc s = if T.null s
                                          then Doc.empty
                                          else Doc.literal s
                      in stringToDoc <$> Lua.peekText idx
  Lua.TypeNumber   -> Doc.literal <$> Lua.peekText idx
  _                -> peekUD typeDoc idx

-- | Push a @Doc Text@ value to the Lua stack.
pushDoc :: LuaError e => Pusher e (Doc Text)
pushDoc = pushUD typeDoc

instance Peekable (Doc Text) where
  safepeek = peekDoc

instance Pushable (Doc Text) where
  push = pushDoc

--
-- Parameters
--

-- | @Doc@ typed function parameter.
docParam :: LuaError e => Text -> Parameter e (Doc Text)
docParam name = parameter peekDoc "Doc" name "document"

-- | @Color@ function parameter
colorParam :: Parameter e Doc.Color
colorParam = parameter peekColor "string" "color"
  ("One of 'black', 'red', 'green', 'yellow', 'blue', 'magenta' " <>
   "'cyan', or 'white'.")
  where
    peekColor idx = peekByteString idx >>= \case
      "black"   -> pure Doc.black
      "red"     -> pure Doc.red
      "green"   -> pure Doc.green
      "yellow"  -> pure Doc.yellow
      "blue"    -> pure Doc.blue
      "magenta" -> pure Doc.magenta
      "cyan"    -> pure Doc.cyan
      "white"   -> pure Doc.white
      color -> failPeek $ "Unknown color: " <> color

--
-- Results
--

-- | Function result of type @'Doc'@.
docResult :: LuaError e
          => Text -- ^ Description
          -> FunctionResults e (Doc Text)
docResult = functionResult pushDoc "Doc"
