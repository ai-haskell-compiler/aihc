{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.Arb.Identifiers
  ( genIdent,
    shrinkIdent,
    isValidGeneratedIdent,
    extensionReservedIdentifiers,
  )
where

import Aihc.Parser.Lex (isReservedIdentifier)
import Data.Text (Text)
import Data.Text qualified as T
import Test.QuickCheck (Gen, chooseInt, elements, shrink, vectorOf)

genIdent :: Gen Text
genIdent = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 8)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isValidGeneratedIdent candidate
    then pure candidate
    else genIdent

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
      ident /= "_"
        && (first `elem` (['a' .. 'z'] <> ['_']))
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")) rest
        && ident `notElem` extensionReservedIdentifiers
        && not (isReservedIdentifier ident)
    Nothing -> False

extensionReservedIdentifiers :: [Text]
extensionReservedIdentifiers = ["mdo", "proc", "rec"]
