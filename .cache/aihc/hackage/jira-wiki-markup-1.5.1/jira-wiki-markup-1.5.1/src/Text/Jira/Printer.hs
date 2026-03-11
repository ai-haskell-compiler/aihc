{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Text.Jira.Parser
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Generate Jira wiki markup text from an abstract syntax tree.
-}
module Text.Jira.Printer
  ( pretty
  , renderBlock
  , renderInline
  , prettyBlocks
  , prettyInlines
  , JiraPrinter
  , PrinterState (..)
  , startState
  , withDefault
  ) where

import Data.Char (isAlphaNum)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Control.Monad ((<=<))
import Control.Monad.Reader (Reader, runReader, asks, local)
import Data.Text (Text)
import Text.Jira.Markup
import qualified Data.Text as T

-- | Render Jira document as Jira wiki formatted text.
pretty :: Doc -> Text
pretty (Doc blks) = prettyBlocks blks

-- | Render a list of Jira blocks as Jira wiki formatted text.
prettyBlocks :: [Block] -> Text
prettyBlocks blks = runReader (renderBlocks blks) startState

-- | Renders a list of Jira inline markup elements.
prettyInlines :: [Inline] -> Text
prettyInlines = \case
  [] ->
    ""
  s@Str{} : Styled style inlns : rest ->
    renderInline s <> renderStyledSafely style inlns <> prettyInlines rest
  Styled style inlns : s@(Str t) : rest | startsWithAlphaNum t ->
    renderStyledSafely style inlns <> renderInline s <> prettyInlines rest
  -- Most special chars don't need escaping when surrounded by spaces or within
  -- a word. Braces are the exception, they should always be escaped.
  s@Str{} : SpecialChar c : rest@(Str {}:_) | not (isBrace c) ->
    (renderInline s `T.snoc` c) <> prettyInlines rest
  s@Space : SpecialChar c : rest@(Space {}:_) | not (isBrace c) ->
    (renderInline s `T.snoc` c) <> prettyInlines rest
  s@Linebreak : SpecialChar c : rest@(Space {}:_) | not (isBrace c) ->
    (renderInline s `T.snoc` c) <> prettyInlines rest
  -- Colon and semicolon only need escaping if they could otherwise
  -- become part of a smiley.
  SpecialChar c : rest@(x : _) | c `elem` [':', ';'] && not (isSmileyStr x) ->
    T.singleton c <> prettyInlines rest
  [SpecialChar c] | c `elem` [':', ';'] ->
    T.singleton c
  -- Questionmarks don't have to be escaped unless in groups of two
  SpecialChar '?' : rest | not (startsWithQuestionMark rest) ->
    "?" <> prettyInlines rest
  (x:xs) ->
    renderInline x <> prettyInlines xs

  where
    isBrace = \case
      '{' -> True
      '}' -> True
      _   -> False

    startsWithAlphaNum t = case T.uncons t of
      Just (c, _) -> isAlphaNum c
      _           -> False
    isSmileyStr = \case
      Str x | x `elem` ["D", ")", "(", "P"] -> True
      _                                     -> False

    startsWithQuestionMark = \case
      SpecialChar '?' : _ -> True
      _                   -> False

-- | Internal state used by the printer.
data PrinterState = PrinterState
  { stateInTable   :: Bool
  , stateListLevel :: Text
  }

type JiraPrinter a = Reader PrinterState a

-- | Run with default state.
withDefault :: JiraPrinter a -> a
withDefault = flip runReader startState

-- | Default start state of the printer.
startState :: PrinterState
startState = PrinterState
  { stateInTable = False
  , stateListLevel = ""
  }

-- | Render a block as Jira wiki format.
renderBlocks :: [Block] -> JiraPrinter Text
renderBlocks = concatBlocks <=< mapM renderBlock

-- | Combine the texts produced from rendering a list of blocks.
concatBlocks :: [Text] -> JiraPrinter Text
concatBlocks = return . T.intercalate "\n"

-- | Add a newline character unless we are within a list or table.
appendNewline :: Text -> JiraPrinter Text
appendNewline text = do
  listLevel <- asks stateListLevel
  inTable   <- asks stateInTable
  return $
    -- add final newline only if we are neither within a table nor a list.
    if inTable || not (T.null listLevel)
    then text
    else text <> "\n"

-- | Render a block as Jira wiki format.
renderBlock :: Block -> JiraPrinter Text
renderBlock = \case
  Code lang params content -> return $ T.concat
                              [ "{code:"
                              , T.intercalate "|"
                                (renderLang lang : map renderParam params)
                              , "}\n"
                              , content
                              , "\n{code}"
                              ]
  Color colorName blocks   -> renderBlocks blocks >>= \blks -> return $ T.concat
                              [ "{color:", colorText colorName, "}\n"
                              , blks
                              , "{color}"
                              ]
  BlockQuote [Para xs] | Linebreak `notElem` xs
                           -> return $ "bq. " <> prettyInlines xs
  BlockQuote blocks        -> renderBlocks blocks >>= \blks -> return $ T.concat
                              [ "{quote}\n"
                              , blks
                              , "{quote}"]
  Header lvl inlines       -> return $ T.concat
                              [ "h",  T.pack (show lvl), ". "
                              , prettyInlines inlines
                              ]
  HorizontalRule           -> return "----"
  List style items         -> listWithMarker items (styleChar style) >>=
                              appendNewline
  NoFormat params content  -> return $ T.concat
                              [ "{noformat"
                              , renderBlockParams params
                              , "}\n"
                              , content
                              , "{noformat}"
                              ]
  Panel params blocks     -> renderBlocks blocks >>= \blks ->
                             return $ T.concat
                             [ "{panel"
                             , renderBlockParams params
                             , "}\n"
                             , blks
                             , "{panel}"
                             ]
  Para inlines              -> appendNewline $ prettyInlines inlines
  Table rows                ->
    local (\st -> st { stateInTable = True }) $
      fmap T.unlines (mapM renderRow rows)

-- | Returns the ext representation of a color
colorText :: ColorName -> Text
colorText (ColorName c) = c

renderLang :: Language -> Text
renderLang (Language lang) = lang

renderBlockParams :: [Parameter] -> Text
renderBlockParams = \case
  [] -> mempty
  xs -> T.cons ':' (renderParams xs)

renderParams :: [Parameter] -> Text
renderParams = T.intercalate "|" . map renderParam

renderParam :: Parameter -> Text
renderParam (Parameter key value) = key <> "=" <> value

renderRow :: Row -> JiraPrinter Text
renderRow (Row cells) = do
  rendered <- mapM renderCell cells
  let closing = if all isHeaderCell cells then " ||" else " |"
  return $ T.unwords rendered <> closing
  where
    isHeaderCell HeaderCell {} = True
    isHeaderCell BodyCell {}   = False

renderCell :: Cell -> JiraPrinter Text
renderCell cell = let (cellStart, blocks) = case cell of
                        (HeaderCell bs) -> ("|| ", bs)
                        (BodyCell bs) -> ("| ", bs)
                  in (cellStart <>) <$> renderBlocks blocks

styleChar :: ListStyle -> Char
styleChar = \case
  CircleBullets -> '*'
  SquareBullets -> '-'
  Enumeration   -> '#'

-- | Create a list using the given character as bullet item marker.
listWithMarker :: [[Block]]
               -> Char
               -> JiraPrinter Text
listWithMarker items marker = do
  let addItem s = s { stateListLevel = stateListLevel s `T.snoc` marker }
  renderedBlocks <- local addItem $ mapM listItemToJira items
  return $ T.intercalate "\n" renderedBlocks

-- | Convert bullet or ordered list item (list of blocks) to Jira.
listItemToJira :: [Block]
               -> JiraPrinter Text
listItemToJira items = do
  contents <- renderBlocks items
  marker <- asks stateListLevel
  return $ case items of
    List{} : _ -> contents
    _          -> marker <> " " <> contents

-- | Renders a single inline item as Jira markup.
renderInline :: Inline -> Text
renderInline = \case
  Anchor name            -> "{anchor:" <> name <> "}"
  AutoLink url           -> fromURL url
  Citation ils           -> "??" <> prettyInlines ils <> "??"
  ColorInline color ils  -> "{color:" <> colorText color <> "}" <>
                            prettyInlines ils <> "{color}"
  Emoji icon             -> iconText icon
  Entity entity          -> "&" <> entity <> ";"
  Image ps url           -> "!" <> fromURL url <> renderImageParams ps <> "!"
  Linebreak              -> "\n"
  Link lt ils url        -> renderLink lt ils url
  Monospaced inlines     -> "{{" <> prettyInlines inlines <> "}}"
  Space                  -> " "
  SpecialChar c          -> case c of
                              -- backslash is unescapable, render as entity
                              '\\' -> "&bsol;"
                              _    -> "\\" `T.snoc` c
  Str txt                -> txt
  Styled style inlines   -> renderWrapped (delimiterChar style) inlines

renderStyledSafely :: InlineStyle -> [Inline] -> Text
renderStyledSafely style =
  let delim = T.pack ['{', delimiterChar style, '}']
  in (delim <>) . (<> delim) . prettyInlines

renderLink :: LinkType -> [Inline] -> URL -> Text
renderLink linkType inlines url = case linkType of
  Attachment -> "[" <> prettyInlines inlines <> "^" <> fromURL url <> "]"
  Email      -> link' $ "mailto:" <> fromURL url
  External   -> link' $ fromURL url
  SmartCard  -> smartLink (fromURL url) "smart-card"
  SmartLink  -> smartLink (fromURL url) "smart-link"
  User       -> link' $ "~" <> fromURL url
 where
  link' urlText = case inlines of
    [] -> "[" <> urlText <> "]"
    _  -> "[" <> prettyInlines inlines <> "|" <> urlText <> "]"
  smartLink urlText smartType =
    "[" <> prettyInlines inlines <> "|" <> urlText <> "|" <> smartType <> "]"

delimiterChar :: InlineStyle -> Char
delimiterChar = \case
  Emphasis -> '_'
  Insert -> '+'
  Strong -> '*'
  Strikeout -> '-'
  Subscript -> '~'
  Superscript -> '^'

-- | Render image parameters (i.e., separate by comma).
renderImageParams :: [Parameter] -> Text
renderImageParams = \case
  [] -> ""
  ps | "thumbnail" `elem` map parameterKey ps -> "|thumbnail"
  ps -> "|" <> T.intercalate ", " (map renderParam ps)

renderWrapped :: Char -> [Inline] -> Text
renderWrapped c = T.cons c . flip T.snoc c . prettyInlines
