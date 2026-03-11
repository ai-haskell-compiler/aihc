{-|
Module      : Text.Jira.Parser.Core
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Core components of the Jira wiki markup parser.
-}
module Text.Jira.Parser.Core
  (
  -- * Jira parser and state
    JiraParser
  , ParserState (..)
  , defaultState
  , parseJira
  , withStateFlag
  -- * String position tracking
  , updateLastStrPos
  , updateLastSpcPos
  , notAfterString
  , afterString
  , afterSpace
  -- * Parsing helpers
  , endOfPara
  , notFollowedBy'
  , many1Till
  , blankline
  , skipSpaces
  , blockNames
  , parameters
  ) where

import Control.Monad (join, void)
import Data.Text (Text, pack)
import Text.Jira.Markup
import Text.Parsec

-- | Jira Parsec parser
type JiraParser = Parsec Text ParserState

-- | Parser state used to keep track of various parameteres.
data ParserState = ParserState
  { stateInLink      :: Bool            -- ^ whether the parser is within a link
  , stateInList      :: Bool            -- ^ whether the parser is within a list
  , stateInMarkup    :: Bool            -- ^ whether the parser is within markup
  , stateInTable     :: Bool            -- ^ whether the parser is within a table
  , stateLastSpcPos  :: Maybe SourcePos -- ^ most recent space char position
  , stateLastStrPos  :: Maybe SourcePos -- ^ position at which the last string
                                        --   ended
  }

-- | Default parser state (i.e., start state)
defaultState :: ParserState
defaultState = ParserState
  { stateInLink      = False
  , stateInList      = False
  , stateInMarkup    = False
  , stateInTable     = False
  , stateLastSpcPos  = Nothing
  , stateLastStrPos  = Nothing
  }

-- | Set a flag in the parser to @True@ before running a parser, then
-- set the flag's value to @False@.
withStateFlag :: (Bool -> ParserState -> ParserState)
              -> JiraParser a
              -> JiraParser a
withStateFlag flagSetter parser = try $
  let setFlag = modifyState . flagSetter
  in setFlag True *> parser <* setFlag False

-- | Updates the state, marking the current input position as the end of a
-- string.
updateLastStrPos :: JiraParser ()
updateLastStrPos = do
  pos <- getPosition
  modifyState $ \st -> st { stateLastStrPos = Just pos }

-- | Updates the state, marking the current input position as the end of a
-- string.
updateLastSpcPos :: JiraParser ()
updateLastSpcPos = do
  pos <- getPosition
  modifyState $ \st -> st { stateLastSpcPos = Just pos }

-- | Returns @'True'@ if the current parser position is directly
-- after a word/string. Returns @'False'@ if the parser is
-- looking at the first character of the input.
afterString :: JiraParser Bool
afterString = do
  curPos <- getPosition
  prevPos <- stateLastStrPos <$> getState
  return (Just curPos == prevPos)

-- | Returns true when the current parser position is either at
-- the beginning of the document or if the preceding characters
-- did not belong to a string.
notAfterString :: JiraParser Bool
notAfterString = not <$> afterString

-- | Returns @'True'@ iff the character before the current parser
-- position was a space.
afterSpace :: JiraParser Bool
afterSpace = do
  curPos <- getPosition
  lastSpacePos <- stateLastSpcPos <$> getState
  return (Just curPos == lastSpacePos)

-- | Parses a string with the given Jira parser.
parseJira :: JiraParser a -> Text -> Either ParseError a
parseJira parser = runParser parser defaultState ""

-- | Skip zero or more space chars.
skipSpaces :: JiraParser ()
skipSpaces = skipMany (char ' ')

-- | Parses an empty line, i.e., a line with no chars or whitespace only.
blankline :: JiraParser ()
blankline = try $ skipSpaces *> void newline

-- | Parses a set of panel parameters
parameters :: JiraParser (Maybe Text, [Parameter])
parameters = option (Nothing, []) $ do
  _      <- char ':'
  lang   <- optionMaybe (try language)
  params <- try (Parameter <$> key <*> (char '=' *> value)) `sepBy` pipe
  return (lang, params)
  where
    pipe     = char '|'
    key      = pack <$> many1 (noneOf "\"'\t\n\r |{}=")
    value    = pack <$> many1 (noneOf "\"'\n\r|{}=")
    language = key <* (pipe <|> lookAhead (char '}'))

-- | Like @manyTill@, but reads at least one item.
many1Till :: (Show end)
          => JiraParser a
          -> JiraParser end
          -> JiraParser [a]
many1Till p end = do
  notFollowedBy' end
  first <- p
  rest <- manyTill p end
  return (first:rest)

-- | Succeeds if the parser is looking at the end of a paragraph.
endOfPara :: JiraParser ()
endOfPara = eof
  <|> lookAhead blankline
  <|> lookAhead headerStart
  <|> lookAhead quoteStart
  <|> lookAhead horizontalRule
  <|> lookAhead listItemStart
  <|> lookAhead tableStart
  <|> lookAhead panelStart
  where
    headerStart    = void $ char 'h' *> oneOf "123456" <* char '.'
    quoteStart     = void $ string "bq."
    listItemStart  = void $ skipSpaces *> many1 (oneOf "#*-") <* char ' '
    tableStart     = void $ skipSpaces *> many1 (char '|')
    panelStart     = void $ char '{' *> choice (map (try . string) blockNames)
    horizontalRule = void $ try (string "----") *> blankline

blockNames :: [String]
blockNames = ["code", "color", "noformat", "panel", "quote"]

-- | Variant of parsec's @notFollowedBy@ function which properly fails even if
-- the given parser does not consume any input (like @eof@ does).
notFollowedBy' :: Show a => JiraParser a -> JiraParser ()
notFollowedBy' p =
  let failIfSucceeds = unexpected . show <$> try p
      unitParser = return (return ())
  in try $ join (failIfSucceeds <|> unitParser)
