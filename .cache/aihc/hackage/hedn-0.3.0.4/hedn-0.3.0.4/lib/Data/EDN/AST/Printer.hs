{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.AST.Printer
  ( renderText
  , prettyTaggedValue
  , prettyValue
  ) where

import Data.Char (ord)
import Data.Foldable (toList)
#if MIN_VERSION_base(4,13,0)
#else
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import Text.Printf (printf)

import qualified Data.Map as Map
import qualified Data.Text as Text
#if MIN_VERSION_prettyprinter(1,7,1)
import Prettyprinter (Doc, pretty, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
#else
import Data.Text.Prettyprint.Doc (Doc, pretty, (<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP
#endif

import qualified Data.EDN.AST.Types as EDN

-- | Render EDN document to 'Text'
renderText :: EDN.TaggedValue -> Text
renderText =
  PP.renderStrict . PP.layoutPretty options . prettyTaggedValue
  where
    options = PP.defaultLayoutOptions

-- | Prepare 'EDN.TaggedValue'
prettyTaggedValue :: EDN.TaggedValue -> Doc a
prettyTaggedValue = \case
  EDN.Tagged "" tag value ->
    "#" <> pretty tag <+> prettyValue value
  EDN.Tagged ns tag value ->
    "#" <> pretty ns <> "/" <> pretty tag <+> prettyValue value
  EDN.NoTag value ->
    prettyValue value

-- | Prepare 'EDN.Value'
prettyValue :: EDN.Value -> Doc a
prettyValue = PP.fuse PP.Shallow . \case
  EDN.Nil ->
    "nil"
  EDN.Boolean bool ->
    if bool then "true" else "false"
  EDN.Character char ->
    mappend "\\" $ case char of
      '\n' -> "newline"
      '\t' -> "tab"
      '\r' -> "return"
      ' '  -> "space"
      _ ->
        case show char of
          '\'' : '\\' : _ ->
            pretty (printf "u%04X" (ord char) :: String)
          _ ->
            pretty char
  EDN.Symbol "" name ->
    pretty name
  EDN.Symbol ns name ->
    pretty ns <> "/" <> pretty name
  EDN.Keyword ident ->
    ":" <> pretty ident
  EDN.Integer int ->
    pretty int
  EDN.Floating double ->
    pretty double
  EDN.String str ->
    PP.enclose "\"" "\"" . pretty $ escapeText str
  EDN.List items ->
    PP.parens . PP.hsep $
      map prettyTaggedValue items
  EDN.Vec items ->
    PP.brackets . PP.hsep $
      map prettyTaggedValue (toList items)
  EDN.Set items ->
    mappend "#" . PP.braces . PP.hsep $
      map prettyTaggedValue (toList items)
  EDN.Map pairs ->
    PP.braces . PP.hsep $
      [ prettyTaggedValue k <+> prettyTaggedValue v
      | (k, v) <- Map.toList pairs
      ]

escapeText :: Text -> Text
escapeText = Text.concatMap escape
  where
    escape = \case
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '"'  -> "\\\""
      c    -> Text.singleton c
