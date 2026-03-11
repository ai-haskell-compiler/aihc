{-# LANGUAGE ViewPatterns #-}

-- |
--
-- Module:      Language.Egison.Pretty.Pattern.External
-- Description: Externally privided printers
-- Stability:   experimental
--
-- This module defines a set of combinators that access to externally provided printers

module Language.Egison.Pretty.Pattern.External
  ( name
  , varName
  , valueExpr
  )
where

import           Data.Text                      ( Text
                                                , unpack
                                                )

import           Language.Egison.Pretty.Pattern.Prim
                                                ( Doc
                                                , text
                                                , parens
                                                )
import           Language.Egison.Pretty.Pattern.Print
                                                ( Print
                                                , askMode
                                                )
import           Language.Egison.Pretty.Pattern.PrintMode
                                                ( PrintMode(..) )


-- | Check whether the string is surrounded by something.
--
-- >>> let parens = ('(', ')')
-- >>> surroundedBy parens "(Hello, World)"
-- True
--
-- >>> surroundedBy parens "Hello, World"
-- False
--
-- >>> surroundedBy parens "(Hello)(World)"
-- False
surroundedBy :: (Char, Char) -> String -> Bool
surroundedBy (begin, end) (h : (reverse -> (l : body)))
  | h == begin && l == end = foldr go 0 body == 0
 where
  go :: Char -> Int -> Int
  go c | c == begin = (+) 1
       | c == end   = (-) 1
       | otherwise  = id
surroundedBy _ _ = False

lexicalChunk :: Text -> Doc
lexicalChunk txt
  | containsDelimiter str && not (checkSurrounding str) = parens $ text txt
  | otherwise = text txt
 where
  containsDelimiter x = elem ' ' x || elem ',' x || elem ')' x || elem ']' x
  checkSurrounding x = surroundedBy ('(', ')') x || surroundedBy ('[', ']') x
  str = unpack txt

varName :: v -> Print n v e Doc
varName v = do
  PrintMode { varNamePrinter } <- askMode
  pure . lexicalChunk $ varNamePrinter v

name :: n -> Print n v e Doc
name n = do
  PrintMode { namePrinter } <- askMode
  pure . lexicalChunk $ namePrinter n

valueExpr :: e -> Print n v e Doc
valueExpr e = do
  PrintMode { valueExprPrinter } <- askMode
  pure . lexicalChunk $ valueExprPrinter e
