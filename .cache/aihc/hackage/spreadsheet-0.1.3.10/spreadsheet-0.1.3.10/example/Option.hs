module Option where

import qualified Options.Applicative as OP
import Control.Applicative ((<|>))

import Data.Monoid ((<>))


delimiter :: OP.Parser Char
delimiter =
   OP.option (character "delimiter") $
      OP.short 'd' <>
      OP.long "delimiter" <>
      OP.metavar "CHAR" <>
      OP.value ',' <>
      OP.help "Field delimiter character"

quotation :: OP.Parser Char
quotation =
   OP.option (character "quotation") $
      OP.short 'q' <>
      OP.long "quotation" <>
      OP.metavar "CHAR" <>
      OP.value '"' <>
      OP.help "Quotation mark character"

character :: String -> OP.ReadM Char
character name =
   OP.eitherReader $ \str ->
   case str of
      "TAB" -> Right '\t'
      "\\t" -> Right '\t'
      [c] -> Right c
      _ -> Left $
         name ++ " must be one character, which " ++ show str ++ " is not"

template :: OP.Parser (IO String)
template =
   (fmap readFile $
    OP.strArgument $
      OP.metavar "TEMPLATE" <>
      OP.help "Template file")
   <|>
   (fmap return $
    OP.strOption $
      OP.long "template" <>
      OP.metavar "TEXT" <>
      OP.help "Template text")
