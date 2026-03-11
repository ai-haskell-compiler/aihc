module Main where

import qualified CSVExtract.Option as Option
import qualified Options.Applicative as OP

import qualified Data.Spreadsheet as Sheet

import Control.Monad (msum, )
import Control.Applicative ((<$>), )
import Control.Functor.HT (outerProduct, )

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map, )
import Data.Maybe (fromMaybe, )
import Data.Tuple.HT (mapFst, )


divideTemplate :: [String] -> String -> (String, [(String, String)])
divideTemplate placeholders =
   let go [] = ([], [])
       go str@(s:ss) =
         fromMaybe
            (mapFst (s:) $ go ss)
            (msum $
             flip map placeholders $ \placeholder ->
               (\ ~(text,remain) -> ([], (placeholder,text) : remain)) . go <$>
               ListHT.maybePrefixOf placeholder str)
   in  go

searchFirst :: String -> String -> Maybe String
searchFirst text = msum . map (ListHT.maybePrefixOf text) . ListHT.tails

searchNext :: String -> String -> (String, Maybe String)
searchNext text =
   let go [] = ([], Nothing)
       go str@(s:ss) =
         case ListHT.maybePrefixOf text str of
            Nothing -> mapFst (s:) $ go ss
            Just remain -> ([], Just remain)
   in  go

extractRow ::
   (String, [(String, String)]) -> String -> Maybe (Map String String, String)
extractRow (firstPart, parts) str0 =
   flip fmap (searchFirst firstPart str0) $
      let go m [] str1 = (m,str1)
          go m ((placeholder,text):phs) str1 =
            case searchNext text str1 of
               (_, Nothing) -> (m,"")
               (content, Just remain) ->
                  go (Map.insert placeholder content m) phs remain
      in  go Map.empty parts

extract :: (String, [(String, String)]) -> String -> [Map String String]
extract template = List.unfoldr (extractRow template)


main :: IO ()
main = do
   opt <- OP.execParser $ Option.info Option.parser
   let placeholders = ListHT.chop (','==) $ Option.columns opt
   let ignore = ListHT.chop (','==) $ Option.ignore opt
   template <- Option.readTemplate opt
   interact $ \input ->
      Sheet.toString (Option.quotation opt) (Option.delimiter opt) $
      (if Option.header opt then (placeholders:) else id) $
      outerProduct
         (\row placeholder -> Map.findWithDefault "" placeholder row)
         (extract (divideTemplate (placeholders++ignore) template) input)
         placeholders
