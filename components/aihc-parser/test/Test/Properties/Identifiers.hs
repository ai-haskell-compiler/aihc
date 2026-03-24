{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.Identifiers
  ( genIdent,
    genConIdent,
    genOperatorText,
    genName,
    genNameOp,
    genVarName,
    genDataName,
    genConName,
    genTvName,
    genTcClsName,
    genTypeVarName,
    genTypeConName,
    shrinkIdent,
    shrinkName,
    isValidGeneratedIdent,
  )
where

import Aihc.Lexer (isReservedIdentifier)
import Aihc.Parser.Ast (Name (..), NameSpace (..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Gen, chooseInt, elements, oneof, shrink, vectorOf)

-- | Generate a lowercase identifier text
genIdent :: Gen Text
genIdent = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 8)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isValidGeneratedIdent candidate
    then pure candidate
    else genIdent

-- | Generate an uppercase constructor identifier text
genConIdent :: Gen Text
genConIdent = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  pure (T.pack (first : rest))

-- | Generate an operator symbol text
genOperatorText :: Gen Text
genOperatorText = oneof [genSimpleOperator, genCustomOperator]
  where
    genSimpleOperator = elements ["+", "-", "*", "/", "++", ">>", ">>=", "<>", ".", "$"]
    genCustomOperator = do
      len <- chooseInt (1, 3)
      -- Excluding ':' since that's for constructor operators
      chars <- vectorOf len (elements "!#$%&*+./<=>?\\^|-~")
      let candidate = T.pack chars
      -- Avoid reserved operators and comment starters
      if candidate `elem` ["..", "::", "=", "\\", "|", "<-", "->", "~", "=>", "--"]
        then genCustomOperator
        else pure candidate

-- | Generate a Name (unqualified variable or symbol)
genName :: Gen Name
genName = oneof [genVarName, genOpName]

-- | Generate a VarName (variable namespace)
genVarName :: Gen Name
genVarName = Name Nothing VarName <$> genIdent

-- | Generate a DataName (data constructor namespace)
genDataName :: Gen Name
genDataName = Name Nothing DataName <$> genConIdent

-- | Generate a TvName (type variable namespace)
genTvName :: Gen Name
genTvName = Name Nothing TvName <$> genIdent

-- | Generate a TcClsName (type constructor/class namespace)
genTcClsName :: Gen Name
genTcClsName = Name Nothing TcClsName <$> genConIdent

-- | Generate a Name with operator (VarName namespace)
genOpName :: Gen Name
genOpName = Name Nothing VarName <$> genOperatorText

-- | Alias for genOpName (used in expression tests)
genNameOp :: Gen Name
genNameOp = genOpName

-- | Alias for genDataName (used in expression tests for constructor patterns)
genConName :: Gen Name
genConName = genDataName

-- | Generate a type variable Name (TvName namespace) - alias for type contexts
genTypeVarName :: Gen Name
genTypeVarName = genTvName

-- | Generate a type constructor Name (TcClsName namespace) - alias for type contexts
genTypeConName :: Gen Name
genTypeConName = genTcClsName

shrinkIdent :: Text -> [Text]
shrinkIdent name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    not (T.null candidate),
    isValidGeneratedIdent candidate
  ]

-- | Shrink a Name
shrinkName :: Name -> [Name]
shrinkName (Name mMod ns ident) =
  [ Name mMod ns ident'
  | ident' <- shrinkIdent ident,
    not (T.null ident')
  ]

isValidGeneratedIdent :: Text -> Bool
isValidGeneratedIdent ident =
  case T.uncons ident of
    Just (first, rest) ->
      ident /= "_"
        && (first `elem` (['a' .. 'z'] <> ['_']))
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")) rest
        && not (isReservedIdentifier ident)
    Nothing -> False
