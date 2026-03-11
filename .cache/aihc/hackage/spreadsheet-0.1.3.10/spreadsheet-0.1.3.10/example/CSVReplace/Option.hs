module CSVReplace.Option where

import qualified Option
import qualified Options.Applicative as OP

import Control.Applicative (pure, (<*>), (<$>))

import Data.Monoid ((<>))


info :: OP.Parser a -> OP.ParserInfo a
info p =
   OP.info
      (OP.helper <*> p)
      (OP.fullDesc <>
       (OP.progDesc $
         "Replace placeholders in a text file according to a CSV table. " ++
         "The CSV file is read from standard input."))

parser :: OP.Parser Option
parser =
   pure Option
   <*> (OP.option (Just <$> OP.str) $
         OP.long "multifile" <>
         OP.metavar "FILEPATTERN" <>
         OP.value Nothing <>
         OP.help "Generate one file per CSV row")
   <*> Option.delimiter
   <*> Option.quotation
   <*> Option.template


data Option =
   Option {
      multiFile :: Maybe FilePath,
      delimiter, quotation :: Char,
      readTemplate :: IO String
   }
