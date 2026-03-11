{-|
Module      : Text.Jira.Parser.Block
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Parse Jira wiki blocks.
-}
module Text.Jira.Parser.Block
  ( block
    -- * Parsers for block types
  , blockQuote
  , code
  , color
  , header
  , horizontalRule
  , list
  , noformat
  , panel
  , para
  , table
  ) where

import Control.Monad (guard, void, when)
import Data.Char (digitToInt)
import Data.Text (pack)
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline
import Text.Jira.Parser.Shared (colorName)
import Text.Parsec

-- | Parses any block element.
block :: JiraParser Block
block = choice
  [ header
  , list
  , table
  , blockQuote
  , horizontalRule
  , code
  , noformat
  , panel
  , color
  , para
  ] <* skipWhitespace

-- | Parses a paragraph into a @Para@.
para :: JiraParser Block
para = (<?> "para") . try $ do
  isInList <- stateInList <$> getState
  when isInList $ do
    notFollowedBy' blankline
    notFollowedBy' horizontalRule
  Para . normalizeInlines <$> many1 inline

-- | Parses a header line into a @Header@.
header :: JiraParser Block
header = (<?> "header") . try $ do
  level <- digitToInt <$> (char 'h' *> oneOf "123456" <* char '.')
  content <- skipMany (char ' ') *> inline `manyTill` (void newline <|> eof)
  return $ Header level (normalizeInlines content)

-- | Parses a list into @List@.
list :: JiraParser Block
list = (<?> "list") . try $ do
  guard . not . stateInList =<< getState
  withStateFlag (\b st -> st { stateInList = b }) $
    listAtDepth 0
  where
    listAtDepth :: Int -> JiraParser Block
    listAtDepth depth = try $ atDepth depth *> listAtDepth' depth

    listAtDepth' :: Int -> JiraParser Block
    listAtDepth' depth = try $ do
      bulletChar <- anyBulletMarker
      first <- firstItemAtDepth depth
      rest  <- many (try $ listItemAtDepth depth (char bulletChar))
      return $ List (style bulletChar) (first:rest)

    style :: Char -> ListStyle
    style c = case c of
      '-' -> SquareBullets
      '*' -> CircleBullets
      '#' -> Enumeration
      _   -> error ("the impossible happened: unknown style for bullet " ++ [c])

    atDepth :: Int -> JiraParser ()
    atDepth depth = try $ skipSpaces <* count depth anyBulletMarker

    firstItemAtDepth :: Int -> JiraParser [Block]
    firstItemAtDepth depth = try $ listContent (depth + 1) <|>
      do
        blocks <- nonListContent depth
        nestedLists <- try . many $ listAtDepth (depth + 1)
        return $ blocks ++ nestedLists

    listItemAtDepth :: Int -> JiraParser Char -> JiraParser [Block]
    listItemAtDepth depth bulletChar = atDepth depth *>
      (try (bulletChar *> nonListContent depth) <|>
       try (anyBulletMarker *> listContent depth))

    listContent :: Int -> JiraParser [Block]
    listContent depth = do
        first <- listAtDepth' depth
        rest <- many (listAtDepth depth)
        return (first : rest)

    anyBulletMarker :: JiraParser Char
    anyBulletMarker = oneOf "*-#"

    nonListContent :: Int -> JiraParser [Block]
    nonListContent depth = try $
      let nonListBlock = do
            notFollowedBy' (skipSpaces *> many1 (oneOf "#-*"))
            block
      in char ' ' *> do
        first   <- block
        nonList <- many nonListBlock
        lists   <- many (listAtDepth (depth + 1))
        return (first : nonList ++ lists)

-- | Parses a table into a @Table@ element.
table :: JiraParser Block
table = do
  guard . not . stateInTable =<< getState
  withStateFlag (\b st -> st { stateInTable = b }) $
    Table <$> many1 row

-- | Parses a table row.
row :: JiraParser Row
row = fmap Row . try $
  many1 cell <* optional (skipMany (oneOf " |") *> newline)

-- | Parses a table cell.
cell :: JiraParser Cell
cell = try $ do
  mkCell <- cellStart
  bs     <- many block
  return $ mkCell bs

-- | Parses the beginning of a table cell and returns a function which
-- constructs a cell of the appropriate type when given the cell's content.
cellStart :: JiraParser ([Block] -> Cell)
cellStart = try
  $  skipSpaces
  *> char '|'
  *> option BodyCell (HeaderCell <$ many1 (char '|'))
  <* skipSpaces
  <* notFollowedBy' newline

-- | Parses a code block into a @Code@ element.
code :: JiraParser Block
code = try $ do
  (langName, params) <- string "{code" *> parameters <* char '}' <* blankline
  let lang = maybe defaultLanguage Language langName
  content <- anyChar `manyTill` try (string "{code}" *> blankline)
  return $ Code lang params (pack content)
  where
    defaultLanguage = Language (pack "java")

-- | Parses a block quote into a @'Quote'@ element.
blockQuote :: JiraParser Block
blockQuote = try $ singleLineBq <|> multiLineBq
  where
    singleLineBq = BlockQuote . (:[]) . Para <$>
                   (string "bq." *> skipMany (char ' ') *>
                    inline `manyTill` (void newline <|> eof))
    multiLineBq = BlockQuote <$>
                  (string "{quote}"
                   *> optional blankline
                   *> skipMany (char ' ')
                   *> block `manyTill` try (string "{quote}"))

-- | Parses four consecutive hyphens as @'HorizontalRule'@.
horizontalRule :: JiraParser Block
horizontalRule = HorizontalRule <$
  try (string "----" *> blankline)

-- | Parses a preformatted text into a @NoFormat@ element.
noformat :: JiraParser Block
noformat = try $ do
  (_, params) <- string "{noformat" *> parameters <* char '}'
  optional newline
  content <- anyChar `manyTill` try (string "{noformat}" *> optional blankline)
  return $ NoFormat params (pack content)

-- | Parses a preformatted text into a @NoFormat@ element.
panel :: JiraParser Block
panel = try $ do
  (_, params) <- string "{panel" *> parameters <* char '}' <* newline
  content <- block `manyTill` try (string "{panel}" *> blankline)
  return $ Panel params content

-- | Parses colored text into a @'Color'@ element.
color :: JiraParser Block
color= try $ do
  name <- string "{color:" *> colorName <* char '}'
  content <- block `manyTill` try (string "{color}" *> blankline)
  return $ Color (ColorName $ pack name) content

-- | Skip whitespace till we reach the next block
skipWhitespace :: JiraParser ()
skipWhitespace = optional $ do
  isInList  <- stateInList  <$> getState
  isInTable <- stateInTable <$> getState
  case (isInList, isInTable) of
    (True, _) -> blankline
    (_, True) -> skipSpaces
    _         -> skipMany blankline
