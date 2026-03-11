{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Data.Aeson.JSONPath.Parser.Filter
  ( pFilter )
  where

import qualified Data.Text                      as T
import qualified Text.ParserCombinators.Parsec  as P

import Data.Functor                  (($>))
import Data.Maybe                    (isNothing)
import Data.Scientific               (Scientific)
import Text.ParserCombinators.Parsec ((<|>))

import Data.Aeson.JSONPath.Parser.Name
import Data.Aeson.JSONPath.Parser.Number
import Data.Aeson.JSONPath.Parser.Common

import Data.Aeson.JSONPath.Types

import Prelude

pFilter :: P.Parser a -> P.Parser (Selector a)
pFilter pQ = do
  P.char '?'
  pSpaces
  Filter <$> pLogicalOrExpr pQ


pLogicalOrExpr :: P.Parser a -> P.Parser (LogicalOrExpr a)
pLogicalOrExpr pQ = do
  expr <- pLogicalAndExpr pQ
  optionalExprs <- P.many $ pOrSepLogicalAndExprs pQ
  return $ LogicalOr (expr:optionalExprs)
    where
      pOrSepLogicalAndExprs :: P.Parser a -> P.Parser (LogicalAndExpr a)
      pOrSepLogicalAndExprs pQ' = P.try $ pSpaces *> P.string "||" *> pSpaces *> pLogicalAndExpr pQ'

pLogicalAndExpr :: P.Parser a -> P.Parser (LogicalAndExpr a)
pLogicalAndExpr pQ = do
  expr <- pBasicExpr pQ
  optionalExprs <- P.many $ pAndSepBasicExprs pQ
  return $ LogicalAnd (expr:optionalExprs)
    where
      pAndSepBasicExprs :: P.Parser a -> P.Parser (BasicExpr a)
      pAndSepBasicExprs pQ' = P.try $ pSpaces *> P.string "&&" *> pSpaces *> pBasicExpr pQ'

pBasicExpr :: P.Parser a -> P.Parser (BasicExpr a)
pBasicExpr pQ 
  = P.try (pParenExpr pQ)
 <|> P.try pComparisonExpr
 <|> P.try (pTestExpr pQ)

pParenExpr :: P.Parser a -> P.Parser (BasicExpr a)
pParenExpr pQ = do
  notOp <- P.optionMaybe (P.char '!' <* pSpaces)
  P.char '('
  pSpaces
  expr <- pLogicalOrExpr pQ
  pSpaces
  P.char ')'
  let parenExp = if isNothing notOp then Paren expr else NotParen expr
  return parenExp

pTestExpr :: P.Parser a -> P.Parser (BasicExpr a)
pTestExpr pQ = do
  notOp <- P.optionMaybe (P.char '!' <* pSpaces)
  q <- P.try (FilterQuery <$> pQ)
  let testExp = if isNothing notOp then Test q else NotTest q
  return testExp

pComparisonExpr :: P.Parser (BasicExpr a)
pComparisonExpr = do
  leftC <- pComparable
  pSpaces
  compOp <- pComparisonOp
  pSpaces
  Comparison . Comp leftC compOp <$> pComparable

pComparisonOp :: P.Parser ComparisonOp
pComparisonOp = P.try (P.string ">=" $> GreaterOrEqual)
             <|> P.try (P.string "<=" $> LessOrEqual)
             <|> P.try (P.char '>' $> Greater)
             <|> P.try (P.char '<' $> Less)
             <|> P.try (P.string "!=" $> NotEqual)
             <|> P.try (P.string "==" $> Equal)

pComparable :: P.Parser Comparable
pComparable = P.try pCompLit <|> P.try pCompSQ

pCompLit :: P.Parser Comparable
pCompLit = CompLit
            <$> (P.try pLitString
            <|> P.try pLitNum
            <|> P.try pLitBool
            <|> P.try pLitNull)

pLitString :: P.Parser Literal
pLitString = LitString . T.pack <$> (P.try pSingleQuotted <|> P.try pDoubleQuotted)

pLitNum :: P.Parser Literal
pLitNum = LitNum 
           <$> (P.try (P.string "-0" $> (0 :: Scientific)) -- edge case
           <|> P.try pDoubleScientific 
           <|> P.try pScientific)

pLitBool :: P.Parser Literal
pLitBool = LitBool <$> (P.try (P.string "true" $> True) <|> P.try (P.string "false" $> False))

pLitNull :: P.Parser Literal
pLitNull = P.string "null" $> LitNull

pCompSQ :: P.Parser Comparable
pCompSQ = CompSQ <$> (P.try pCurrentSingleQ <|> P.try pRootSingleQ)

pCurrentSingleQ :: P.Parser SingularQuery
pCurrentSingleQ = do
  P.char '@'
  segs <- P.many $ P.try (pSpaces *> pSingularQuerySegment)
  return $ SingularQuery { singularQueryType = CurrentSQ, singularQuerySegments = segs }

pRootSingleQ :: P.Parser SingularQuery
pRootSingleQ = do
  P.char '$'
  segs <- P.many $ P.try (pSpaces *> pSingularQuerySegment)
  return $ SingularQuery { singularQueryType = RootSQ, singularQuerySegments = segs }

pSingularQuerySegment :: P.Parser SingularQuerySegment
pSingularQuerySegment = P.try pSingularQNameSeg <|> P.try pSingularQIndexSeg

pSingularQNameSeg :: P.Parser SingularQuerySegment
pSingularQNameSeg = P.try pSingularQNameBracketed <|> P.try pSingularQNameDotted
  where
    pSingularQNameBracketed = do
      P.char '['
      name <- T.pack <$> (P.try pSingleQuotted <|> P.try pDoubleQuotted)
      P.char ']'
      return $ NameSQSeg name

    pSingularQNameDotted = do
      P.char '.'
      P.lookAhead (P.letter <|> P.oneOf "_" <|> pUnicodeChar)
      name <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "_" <|> pUnicodeChar)
      return $ NameSQSeg name

pSingularQIndexSeg :: P.Parser SingularQuerySegment
pSingularQIndexSeg = do
  P.char '['
  idx <- pSignedInt
  P.char ']'
  return $ IndexSQSeg idx
