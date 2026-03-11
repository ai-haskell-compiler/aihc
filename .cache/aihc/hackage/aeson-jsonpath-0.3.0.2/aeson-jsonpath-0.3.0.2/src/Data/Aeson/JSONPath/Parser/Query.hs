{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Data.Aeson.JSONPath.Parser.Query
  ( pRootQuery
  , pCurrentQuery )
  where

import qualified Data.Text                      as T
import qualified Text.ParserCombinators.Parsec  as P

import Data.Functor                  (($>))
import Data.Maybe                    (isNothing)
import Text.ParserCombinators.Parsec ((<|>))

import Data.Aeson.JSONPath.Parser.Filter (pFilter)
import Data.Aeson.JSONPath.Parser.Name
import Data.Aeson.JSONPath.Parser.Number
import Data.Aeson.JSONPath.Parser.Common
import Data.Aeson.JSONPath.Types

import Prelude

pRootQuery :: P.Parser Query
pRootQuery = do
  P.char '$'
  segs <- P.many $ P.try pSpacedOutSegments
  return $ Query { queryType = Root, querySegments = segs }
    where
      pQ = P.try pRootQuery <|> P.try pCurrentQuery
      pSpacedOutSegments = pSpaces *> pQuerySegment pQ

pCurrentQuery :: P.Parser Query
pCurrentQuery = do
  P.char '@'
  segs <- P.many $ P.try pSpacedOutSegments
  return $ Query { queryType = Current, querySegments = segs }
    where
      pQ = P.try pRootQuery <|> P.try pCurrentQuery
      pSpacedOutSegments = pSpaces *> pQuerySegment pQ


pQuerySegment :: P.Parser a -> P.Parser (QuerySegment a)
pQuerySegment pQ = do
  dotdot <- P.optionMaybe (P.try $ P.string "..")
  seg <- pSegment pQ $ isNothing dotdot
  let segType = if isNothing dotdot then Child else Descendant
  return $ QuerySegment { segmentType = segType, segment = seg }

pSegment :: P.Parser a -> Bool -> P.Parser (Segment a)
pSegment pQ isChild
        = P.try (pBracketed pQ)
       <|> P.try (pDotted isChild)
       <|> P.try (pWildcardSeg isChild)

pBracketed :: P.Parser a -> P.Parser (Segment a)
pBracketed pQ = do
  P.char '['
  pSpaces
  sel <- pSelector pQ
  optionalSels <- P.many $ pCommaSepSelectors pQ
  pSpaces
  P.char ']'
  return $ Bracketed (sel:optionalSels)
    where
      pCommaSepSelectors :: P.Parser a -> P.Parser (Selector a)
      pCommaSepSelectors p = P.try $ pSpaces *> P.char ',' *> pSpaces *> pSelector p


pDotted :: Bool -> P.Parser (Segment a)
pDotted isChild = do
  (if isChild then P.string "." else P.string "")
  P.lookAhead (P.letter <|> P.oneOf "_" <|> pUnicodeChar)
  key <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "_" <|> pUnicodeChar)
  return $ Dotted key


pWildcardSeg :: Bool -> P.Parser (Segment a)
pWildcardSeg isChild = (if isChild then P.string "." else P.string "") *> P.char '*' $> WildcardSegment
  
pSelector :: P.Parser a -> P.Parser (Selector a)
pSelector pQ = P.try pName
                   <|> P.try pSlice 
                   <|> P.try pIndex
                   <|> P.try pWildcardSel
                   <|> P.try (pFilter pQ)

pName :: P.Parser (Selector a)
pName = Name . T.pack <$> (P.try pSingleQuotted <|> P.try pDoubleQuotted)

pIndex :: P.Parser (Selector a)
pIndex = Index <$> pSignedInt

pSlice :: P.Parser (Selector a)
pSlice = do
  start <- P.optionMaybe (pSignedInt <* pSpaces)
  P.char ':'
  pSpaces
  end <- P.optionMaybe (pSignedInt <* pSpaces)
  step <- P.optionMaybe (P.char ':' *> P.optionMaybe (pSpaces *> pSignedInt))
  return $ ArraySlice (start, end, case step of
    Just (Just n) -> n
    _ -> 1)


pWildcardSel :: P.Parser (Selector a)
pWildcardSel = P.char '*' $> WildcardSelector
