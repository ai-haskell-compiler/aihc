{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.Glabrous.Internal where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import qualified Data.HashMap.Strict  as H
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T

import           Text.Glabrous.Types

toTextWithContext :: (T.Text -> T.Text) -> Context -> [Token] -> T.Text
toTextWithContext tagDefault Context{..} ts =
  T.concat (trans <$> ts)
  where
    trans (Tag k)     = fromMaybe (tagDefault k) (H.lookup k variables)
    trans (Literal c) = c

-- | Build a 'Template' from a 'T.Text'.
--
-- >λ>fromText "Glabrous templates use only the simplest Mustache tag: {{name}}."
-- >Right (Template {content = [Literal "Glabrous templates use only the simplest Mustache tag: ",Tag "name",Literal "."]})
--
fromText :: T.Text -> Either String Template
fromText t =
  case parseOnly tokens t of
    Right ts -> Right Template { content = ts }
    Left e   -> Left e

isLiteral :: Token -> Bool
isLiteral (Literal _) = True
isLiteral _           = False

isTag :: Token -> Bool
isTag (Tag _) = True
isTag _       = False

tokens :: Parser [Token]
tokens =
  many' token
  where
    token = literal <|> tag <|> leftover
    leftover = do
      c <- takeWhile1 (not . content)
      return (Literal c)
    literal = do
      c <- takeWhile1 content
      return (Literal c)
    tag = do
      _ <- string "{{"
      Literal t <- literal
      _ <- string "}}"
      return (Tag t)
    content '}' = False
    content '{' = False
    content _   = True

