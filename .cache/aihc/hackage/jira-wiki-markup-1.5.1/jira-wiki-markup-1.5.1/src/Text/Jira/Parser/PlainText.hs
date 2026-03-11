{-|
Module      : Text.Jira.Parser.PlainText
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Functions for parsing markup-less strings.
-}

module Text.Jira.Parser.PlainText
  ( plainText
  ) where

import Data.Text (Text, append, pack)
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline (specialChars)
import Text.Jira.Parser.Shared (icon)
import Text.Parsec

-- | Parses into an @'Inline'@ elements which represent plain text. The
-- result consists of any number of @'Str'@, @'SpecialChar'@, or
-- @'Space'@ elements.
--
-- This parser can be used to convert un-escaped strings into proper
-- Jira markup elements.
plainText :: Text -> Either ParseError [Inline]
plainText = parseJira (normalizeInlines <$> many plainInlineParser)
  where
    plainInlineParser :: JiraParser Inline
    plainInlineParser = choice
      [ Space <$ skipMany1 (char ' ')
      , escapeIcon
      , plainSpecialChar
      , Str . pack <$> many1 (noneOf (' ':specialChars))
      ] <?> "text-only inline"

-- | Escapes text which would otherwise render as an icon.
escapeIcon :: Parsec Text u Inline
escapeIcon = Str . ("\\" `append`) . iconText <$> try icon

plainSpecialChar :: Parsec Text u Inline
plainSpecialChar = SpecialChar <$> oneOf specialChars
