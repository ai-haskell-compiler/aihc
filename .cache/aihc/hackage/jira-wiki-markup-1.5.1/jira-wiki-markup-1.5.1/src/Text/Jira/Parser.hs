{-|
Module      : Text.Jira.Parser
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Parse Jira wiki markup.
-}
module Text.Jira.Parser
  ( parse
  , doc
  , module Text.Jira.Markup
  , module Text.Jira.Parser.Core
  , module Text.Jira.Parser.Inline
  , module Text.Jira.Parser.Block
  , module Text.Jira.Parser.PlainText
  ) where

import Data.Text (Text)
import Text.Jira.Markup
import Text.Jira.Parser.Block
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline
import Text.Jira.Parser.PlainText
import Text.Parsec hiding (parse)

-- | Parses a document into a Jira AST.
parse :: Text -> Either ParseError Doc
parse = parseJira doc

-- | Parses a list of jira blocks into a @'Doc'@ element.
doc :: JiraParser Doc
doc = Doc <$> (skipMany blankline *> many block) <?> "doc"
