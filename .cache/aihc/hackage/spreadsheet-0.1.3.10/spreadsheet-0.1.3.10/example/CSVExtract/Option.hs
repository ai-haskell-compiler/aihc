module CSVExtract.Option where

import qualified Option
import qualified Options.Applicative as OP

import Control.Applicative (pure, (<*>))

import Data.Monoid ((<>))


info :: OP.Parser a -> OP.ParserInfo a
info p =
   OP.info
      (OP.helper <*> p)
      (OP.fullDesc <>
       (OP.progDesc $
         "Extract CSV table using a text template. " ++
         "The filled formular is read from standard input."))

parser :: OP.Parser Option
parser =
   pure Option
   <*> (OP.strOption $
         OP.long "columns" <>
         OP.metavar "COMMALIST" <>
         OP.help "Names for columns and placeholders")
   <*> (OP.strOption $
         OP.long "ignore" <>
         OP.metavar "COMMALIST" <>
         OP.value "" <>
         OP.help "Names for ignored placeholders")
   <*> (OP.switch $
         OP.long "header" <>
         OP.help "Emit column headers")
   <*> Option.delimiter
   <*> Option.quotation
   <*> Option.template


data Option =
   Option {
      columns, ignore :: String,
      header :: Bool,
      delimiter, quotation :: Char,
      readTemplate :: IO String
   }
