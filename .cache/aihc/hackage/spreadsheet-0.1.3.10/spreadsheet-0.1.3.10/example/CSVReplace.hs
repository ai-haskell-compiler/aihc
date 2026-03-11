module Main where

import qualified CSVReplace.Option as Option
import qualified Options.Applicative as OP
import Shell.Utility.Exit (exitFailureMsg)

import qualified Data.Spreadsheet as Sheet
import qualified Data.List.HT as ListHT

import qualified Control.Monad.Exception.Asynchronous as AExc
import Control.Applicative ((<$>), )

import qualified Data.Foldable as Fold
import Data.Foldable (forM_, )



replaceRow :: String -> [String] -> [String] -> String
replaceRow template names row =
   ListHT.multiReplace (filter (not . null . fst) $ zip names row) template

replace :: String -> [String] -> Sheet.T -> String
replace template names = concatMap (replaceRow template names)


main :: IO ()
main = do
   opt <- OP.execParser $ Option.info Option.parser
   template <- Option.readTemplate opt
   sheet <-
      Sheet.fromString (Option.quotation opt) (Option.delimiter opt) <$>
      getContents
   case AExc.result sheet of
      [] -> exitFailureMsg "empty CSV input"
      names:rows ->
         case Option.multiFile opt of
            Nothing -> putStr $ replace template names rows
            Just filePattern ->
               forM_ rows $ \row ->
                  writeFile (replaceRow filePattern names row) $
                     replaceRow template names row
   Fold.mapM_ exitFailureMsg $ AExc.exception sheet
