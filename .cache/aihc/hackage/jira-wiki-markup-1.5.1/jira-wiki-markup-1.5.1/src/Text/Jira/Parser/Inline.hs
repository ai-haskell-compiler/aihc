{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Text.Jira.Parser.Inline
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Parse Jira wiki inline markup.
-}
module Text.Jira.Parser.Inline
  ( inline
    -- * Inline component parsers
  , anchor
  , autolink
  , citation
  , colorInline
  , dash
  , emoji
  , entity
  , image
  , linebreak
  , link
  , monospaced
  , specialChar
  , str
  , styled
  , whitespace
    -- * Constants
  , specialChars
  ) where

import Control.Monad (guard, void)
import Data.Char (isAlphaNum, isAscii, isPunctuation, isSpace, ord)
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>), All (..))
#else
import Data.Monoid (All (..))
#endif
import Data.Text (append, pack)
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Jira.Parser.Shared
import Text.Parsec

-- | Parses any inline element.
inline :: JiraParser Inline
inline = notFollowedBy' blockEnd *> choice
  [ whitespace
  , emoji
  , dash
  , autolink
  , str
  , linebreak
  , link
  , image
  , styled
  , colorInline
  , monospaced
  , anchor
  , citation
  , entity
  , specialChar
  ] <?> "inline"
  where
    blockEnd = char '{' *> choice (map string blockNames) <* char '}'

-- | Characters which, depending on context, can have a special meaning.
specialChars :: String
specialChars = "_+-*^~|[]{}(?!&\\:;"

-- | Parses an in-paragraph newline as a @Linebreak@ element. Both newline
-- characters and double-backslash are recognized as line-breaks.
linebreak :: JiraParser Inline
linebreak = (<?> "linebreak") . try $ do
  guard . not . stateInMarkup =<< getState
  choice [ void $ newline <* notFollowedBy' endOfPara
         , void $ string "\\\\" <* notFollowedBy' (char '\\')
         ]
  updateLastSpcPos
  return Linebreak

-- | Parses whitespace and return a @Space@ element.
whitespace :: JiraParser Inline
whitespace = Space <$ skipMany1 (char ' ') <* updateLastSpcPos
  <?> "whitespace"

-- | Parses a simple, markup-less string into a @Str@ element.
str :: JiraParser Inline
str = Str . pack . mconcat
  <$> many1 (alphaNums <|> otherNonSpecialChars)
  <?> "string"
  where
    nonStrChars = " \n" ++ specialChars
    alphaNums = many1 alphaNum <* updateLastStrPos
    otherNonSpecialChars = many1 . satisfy $ \c ->
      not (isAlphaNum c || c `elem` nonStrChars)

-- | Parses an HTML entity into an @'Entity'@ element.
entity :: JiraParser Inline
entity = Entity . pack
  <$> try (char '&' *> (numerical <|> named) <* char ';')
  where
    numerical = (:) <$> char '#' <*> many1 digit
    named = many1 letter

-- | Parses textual representation of an icon into an @'Emoji'@ element.
emoji :: JiraParser Inline
emoji = try (Emoji <$> icon <?> "emoji")

-- | Parses ASCII representation of en-dash or em-dash.
dash :: JiraParser Inline
dash = try $ do
  guard =<< notAfterString
  _ <- string "--"
  choice [ Str "—" <$ char '-'   -- em dash
         , pure (Str "–")          -- en dash
         ] <* lookAhead (void (char ' ') <|> eof)

-- | Parses a special character symbol as a 'SpecialChar'.
specialChar :: JiraParser Inline
specialChar = SpecialChar <$> (backslash <|> escapedChar <|> plainSpecialChar)
  <?> "special char"
  where
    -- backslash before an icon-sequence that's not an emoji does *not*
    -- work as an escape character.
    backslash = try (char '\\' <* lookAhead (icon' *> alphaNum))

    escapedChar = try (char '\\' *> satisfy isPunctuation)

    plainSpecialChar = do
      inTablePred <- do
        b <- stateInTable <$> getState
        return $ if b then All . (/= '|') else mempty
      inLinkPred  <- do
        b <- stateInLink  <$> getState
        return $ if b then All . (`notElem` ("]^|\n" :: String)) else mempty
      oneOf $ filter (getAll . (inTablePred <> inLinkPred)) specialChars


--
-- Anchors, links and images
--

-- | Parses an anchor into an @Anchor@ element.
anchor :: JiraParser Inline
anchor = Anchor . pack . filter (/= ' ')
  <$> try (string "{anchor:" *> noneOf "\n" `manyTill` char '}')

-- | Parse image into an @Image@ element.
image :: JiraParser Inline
image = try $ do
  -- does not use @url@, as is may contain relative locations.
  src <- char '!'
         *> lookAhead (satisfy $ not . isSpace)
         *> (URL . pack <$> many1 (noneOf "\r\t\n|]!"))
  params <- option [] (char '|' *> (thumbnail <|> imgParams `sepBy` comma))
  _ <- char '!'
  return $ Image params src
  where
    thumbnail = [Parameter "thumbnail" ""] <$ try (string "thumbnail")
    imgParams = try (Parameter <$> key <*> (char '=' *> value))
    key       = pack <$> many1 (noneOf ",\"'\t\n\r |{}=!")
    value     = pack <$> (try quotedValue <|> unquotedValue)
    comma     = char ',' *> skipSpaces
    quotedValue = char '"' *> manyTill (noneOf "\n\r") (char '"')
    unquotedValue = many1 (noneOf ",\"'\n\r|{}=!")

-- | Parse link into a @Link@ element.
link :: JiraParser Inline
link = try $ do
  guard . not . stateInLink =<< getState
  withStateFlag (\b st -> st { stateInLink = b }) $ do
    _ <- char '['
    (alias, sep) <- option ([], '|') . try $ (,) <$> many inline <*> oneOf "^|"
    (linkType, linkURL) <-
      if sep == '|'
      then (Email,) <$> email <|>
           (External,) <$> anchorLink <|>
           (User,) <$> userLink <|>
           externalLink
      else (Attachment,) . URL . pack <$>
           many1 (noneOf "\t\r\f\n]|:;/\\")
    _ <- char ']'
    return $ Link linkType alias linkURL

-- | Parse a plain URL or mail address as @'AutoLink'@ element.
autolink :: JiraParser Inline
autolink = do
  guard . not . stateInLink =<< getState
  AutoLink <$> (email' <|> url True) <?> "email or other URL"
    where email' = (\(URL e) -> URL ("mailto:" <> e)) <$> email

-- | Parse a URL with scheme @file@, @ftp@, @http@, @https@, @irc@,
-- @nntp@, or @news@; ignores @file@ if @isAutoLink@ is false.
url :: Bool {-^ isAutoLink -} -> JiraParser URL
url isAutoLink = try $ do
  let urlChar' = if isAutoLink then urlPathChar else urlChar <|> char ' '
  urlScheme <- scheme
  sep <- pack <$> string "://"
  rest <- pack <$> many urlChar'
  return $ URL (urlScheme `append` sep `append` rest)
  where
    scheme = do
      first <- letter
      case first of
        'f' -> ("file" <$ (guard (not isAutoLink) *> string "ile")) <|>
               ("ftp" <$ string "tp")
        'h' -> string "ttp" *> option "http" ("https" <$ char 's')
        'i' -> "irc" <$ string "rc"
        'n' -> ("nntp" <$ string "ntp") <|> ("news" <$ string "ews")
        _   -> fail "not looking at a known scheme"

-- | Parses an email URI, returns the mail address without schema.
email :: JiraParser URL
email = URL . pack <$> try (string "mailto:" *> many1 urlChar)

-- | Parses the link to an anchor name.
anchorLink :: JiraParser URL
anchorLink = URL . pack <$> ((:) <$> char '#' <*> many1 urlChar)

-- | Parses a user-identifying resource name
userLink :: JiraParser URL
userLink = URL . pack <$> (char '~' *> many (noneOf "|]\n\r"))

-- | Parses an external link, i.e., either a plain link to an external
-- website, or a \"smart\" link or card.
externalLink :: JiraParser (LinkType, URL)
externalLink = do
  url' <- url False
  mSmartType <- optionMaybe (char '|' *> smartLinkType)
  return $ case mSmartType of
    Nothing -> (External, url')
    Just st -> (st, url')

-- | Finds the type of a "smart" link.
smartLinkType :: JiraParser LinkType
smartLinkType = string "smart-" *> choice
  [ SmartLink <$ string "link"
  , SmartCard <$ string "card"
  ]

-- | Parses a character which is allowed in URLs
urlChar :: JiraParser Char
urlChar = satisfy $ \case
  ']' -> False    -- "]"
  '|' -> False    -- "|"
  x   -> ord x > 32 && ord x <= 126 -- excludes space

-- | Parses a character in an URL path.
urlPathChar :: JiraParser Char
urlPathChar = satisfy $ \case
  '!' -> True
  '#' -> True
  '$' -> True
  '%' -> True
  '&' -> True
  '\''-> True
  '(' -> True
  ')' -> True
  '*' -> True
  '+' -> True
  ',' -> True
  '-' -> True
  '.' -> True
  '/' -> True
  ':' -> True
  ';' -> True
  '=' -> True
  '?' -> True
  '@' -> True
  '\\'-> True
  '_' -> True
  '~' -> True
  x   -> isAlphaNum x && isAscii x

--
-- Color
--

-- | Text in a different color.
colorInline :: JiraParser Inline
colorInline = try $ do
  name <- string "{color:" *> colorName <* char '}'
  content <- inline `manyTill` try (string "{color}")
  return $ ColorInline (ColorName $ pack name) content

--
-- Markup
--

-- | Parses styled text
styled :: JiraParser Inline
styled = (simpleStyled <|> forceStyled) <?> "styled text"
  where
    simpleStyled = try $ do
      styleChar <- lookAhead $ oneOf "-_+*~^"
      content   <- noNewlines $ styleChar `delimitingMany` inline
      let style = delimiterStyle styleChar
      return $ Styled style content

    forceStyled = try $ do
      styleChar <- char '{' *> oneOf "-_+*~^" <* char '}'
      let closing = try $ string ['{', styleChar, '}']
      let style   = delimiterStyle styleChar
      content   <- noNewlines $ manyTill inline closing
      return $ Styled style content

-- | Makes sure that the wrapped parser does not parse inline
-- linebreaks.
noNewlines :: JiraParser a -> JiraParser a
noNewlines = withStateFlag (\b st -> st { stateInMarkup = b })

-- | Returns the markup kind from the delimiting markup character.
delimiterStyle :: Char -> InlineStyle
delimiterStyle = \case
  '*' -> Strong
  '+' -> Insert
  '-' -> Strikeout
  '^' -> Superscript
  '_' -> Emphasis
  '~' -> Subscript
  c   -> error ("Unknown delimiter character: " ++ [c])

-- | Parses monospaced text into @Monospaced@.
monospaced :: JiraParser Inline
monospaced = Monospaced
  <$> enclosed (try $ string "{{") (try $ string "}}") inline
  <?> "monospaced"

citation :: JiraParser Inline
citation = Citation
  <$> enclosed (try $ string "??") (try $ string "??") inline
  <?> "citation"

--
-- Helpers
--

-- | Parse text delimited by a character.
delimitingMany :: Char -> JiraParser a -> JiraParser [a]
delimitingMany c = enclosed (char c) (char c)

enclosed :: Show closing
         => JiraParser opening -> JiraParser closing
         -> JiraParser a
         -> JiraParser [a]
enclosed opening closing parser = try $ do
  guard =<< notAfterString
  opening *> notFollowedBy space *> many1Till parser closing'
  where
    closing' = try $ do
      guard . not =<< afterSpace
      closing <* lookAhead wordBoundary
    wordBoundary = void (satisfy (not . isAlphaNum)) <|> eof
