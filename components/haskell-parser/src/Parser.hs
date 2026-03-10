{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parseModule,
    defaultConfig,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast (Expr, Module)
import Parser.Types

defaultConfig :: ParserConfig
defaultConfig =
  ParserConfig
    { allowLineComments = True
    }

parseExpr :: ParserConfig -> Text -> ParseResult Expr
parseExpr _cfg input = ParseErr (removedParserError input)

parseModule :: ParserConfig -> Text -> ParseResult Module
parseModule _cfg input = ParseErr (removedParserError input)

removedParserError :: Text -> ParseError
removedParserError input =
  ParseError
    { offset = 0,
      line = 1,
      col = 1,
      expected = ["parser removed"],
      found = toFound input
    }
  where
    toFound txt =
      let stripped = T.strip txt
       in if T.null stripped then Nothing else Just stripped
