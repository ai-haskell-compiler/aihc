{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.Identifiers
  ( genIdent,
    shrinkIdent,
    reservedWords,
    isValidGeneratedIdent,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Parser (isReservedIdentifier)
import Test.QuickCheck (Gen, chooseInt, elements, shrink, vectorOf)

genIdent :: Gen Text
genIdent = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 8)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isReservedIdentifier candidate
    then genIdent
    else pure candidate

shrinkIdent :: Text -> [Text]
shrinkIdent name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    not (T.null candidate),
    isValidGeneratedIdent candidate
  ]

isValidGeneratedIdent :: Text -> Bool
isValidGeneratedIdent ident =
  case T.uncons ident of
    Just (first, rest) ->
      (first `elem` (['a' .. 'z'] <> ['_']))
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")) rest
        && not (isReservedIdentifier ident)
    Nothing -> False

reservedWords :: [Text]
reservedWords =
  [ "_",
    "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "export",
    "foreign",
    "forall",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "qualified",
    "as",
    "hiding",
    "let",
    "module",
    "newtype",
    "of",
    "then",
    "type",
    "where"
  ]
