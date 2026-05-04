{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.ShorthandSubset
  ( prop_shorthandDeclSubsetOfShow,
    prop_shorthandExprSubsetOfShow,
    prop_shorthandModuleSubsetOfShow,
    prop_shorthandTypeSubsetOfShow,
  )
where

import Aihc.Parser.Shorthand (Shorthand (shorthand))
import Aihc.Parser.Syntax
import Data.Char (isAlphaNum, isSpace)
import Data.List (isSubsequenceOf)
import Test.Properties.Arb.Decl ()
import Test.Properties.Arb.Expr ()
import Test.Properties.Arb.Module ()
import Test.Properties.Arb.Type ()
import Test.QuickCheck

prop_shorthandModuleSubsetOfShow :: Module -> Property
prop_shorthandModuleSubsetOfShow = shorthandSubsetOfShow

prop_shorthandDeclSubsetOfShow :: Decl -> Property
prop_shorthandDeclSubsetOfShow = shorthandSubsetOfShow

prop_shorthandExprSubsetOfShow :: Expr -> Property
prop_shorthandExprSubsetOfShow = shorthandSubsetOfShow

prop_shorthandTypeSubsetOfShow :: Type -> Property
prop_shorthandTypeSubsetOfShow = shorthandSubsetOfShow

shorthandSubsetOfShow :: (Shorthand a, Show a) => a -> Property
shorthandSubsetOfShow value =
  counterexample
    ( "Show:\n"
        <> shown
        <> "\n\nShorthand:\n"
        <> short
        <> "\n\nShow tokens:\n"
        <> show shownTokens
        <> "\n\nShorthand tokens:\n"
        <> show shortTokens
    )
    (shortTokens `isSubsequenceOf` shownTokens)
  where
    shown = show value
    short = show (shorthand value)
    shownTokens = tokens shown
    shortTokens = tokens short

tokens :: String -> [String]
tokens [] = []
tokens input@(c : cs)
  | isSpace c = tokens cs
  | isIdentStart c =
      let (tok, rest) = span isIdentChar input
       in tok : tokens rest
  | c == '"' || c == '\'' =
      let (tok, rest) = stringToken c input
       in tok : tokens rest
  | otherwise = tokens cs

isIdentStart :: Char -> Bool
isIdentStart c = isAlphaNum c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '#'

stringToken :: Char -> String -> (String, String)
stringToken quote (_ : rest) = go False [quote] rest
  where
    go _ acc [] = (reverse acc, [])
    go True acc (c : cs) = go False (c : acc) cs
    go False acc (c : cs)
      | c == '\\' = go True (c : acc) cs
      | c == quote = (reverse (c : acc), cs)
      | otherwise = go False (c : acc) cs
stringToken _ [] = ([], [])
