module Main where

import Shell.Utility.ParseArgument (parseNumber)
import Shell.Utility.Exit (exitFailureMsg)

import qualified Options.Applicative as OP

import qualified Text.HTML.Tagchup.Format as TagFormat
import qualified Text.HTML.Tagchup.Parser as TagParser
import qualified Text.HTML.Tagchup.Process as TagProc
import qualified Text.HTML.Tagchup.Tag.Match as TagMatch
import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name.MixedCase as Name
import qualified Text.XML.Basic.Name as NameC

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy.UTF8 as B_UTF8
import qualified Data.ByteString.Lazy.Char8 as BC

import qualified Data.Spreadsheet as Spreadsheet
import qualified Data.NonEmpty.Mixed as NonEmptyM
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Char as Char
import Data.Function.HT (Id)
import Data.Tuple.HT (mapSnd)
import Data.Maybe.HT (toMaybe)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid ((<>))

import Control.Monad (join, guard)
import Control.Applicative (pure, (<*>), (<|>))

import Text.Printf (printf)



readByteContent :: FilePath -> IO BC.ByteString
readByteContent path =
   if List.isSuffixOf ".ods" path
      then do
         archive <- Zip.toArchive <$> BC.readFile path
         case filter (("content.xml"==) . Zip.eRelativePath) $
               Zip.zEntries archive of
            [] -> exitFailureMsg "content.xml missing in ODS file"
            -- FixMe: Zip.fromEntry is a partial function
            contentEntry : _ -> return $ Zip.fromEntry contentEntry
      else BC.readFile path


fixNameType :: Id [Tag.T Name.T String]
fixNameType = id

{-
In principle it would be fine to ignore encoding, that is,
read and write bytestrings and thus transfer encoding from XML to CSV.
However, we need to match table names.
-}
decode :: BC.ByteString -> String
decode bytes =
   case fmap (map Char.toLower) $ TagProc.getXMLEncoding $
         fixNameType $ TagParser.runSoup $ BC.take 10000 bytes of
      Just "utf-8" -> B_UTF8.toString bytes
      _ -> BC.unpack bytes


maybeTableName :: Tag.T Name.T String -> Maybe String
maybeTableName tag = do
   (foundName, attrs) <- Tag.maybeOpen tag
   guard $ NameC.match "table:table" foundName
   Attr.lookupLit "table:name" attrs


{-
<config:config-item-map-entry config:name="BlaBlaBla">
<table:table table:name="BlaBlaBla" table:style-name="ta1">
-}
extractTablesNames :: [Tag.T Name.T String] -> [String]
extractTablesNames = mapMaybe maybeTableName

listTables :: IO BC.ByteString -> IO ()
listTables readInput =
   mapM_ putStrLn . extractTablesNames . TagParser.runSoup . decode
      =<< readInput


{-
LibreOffice escapes double spaces as in @Foo  Bar@ like so:

> <text:p>Foo <text:s/>Baar</text:p>
-}
flattenText :: [Tag.T Name.T String] -> String
flattenText =
   concatMap
      (\tag ->
         fromMaybe
            (printf "*** unexpected tag %s in cell text ***"
               (TagFormat.xml [tag] "")) $
         Tag.maybeText tag
         <|>
         toMaybe (TagMatch.openNameLit "text:s" tag) " "
         <|>
         toMaybe (TagMatch.closeNameLit "text:s" tag) "")

flattenRepeatedCell :: Int -> [Tag.T Name.T String] -> [String]
flattenRepeatedCell n =
   replicate n
   .
   flattenText
   .
   takeWhile (not . TagMatch.closeNameLit "text:p")
   .
   drop 1
   .
   dropWhile (not . TagMatch.openNameLit "text:p")

maybeCell :: Tag.T Name.T String -> Maybe ([Tag.T Name.T String] -> [String])
maybeCell tag = do
   (foundName, attrs) <- Tag.maybeOpen tag
   toMaybe (NameC.match "table:covered-table-cell" foundName) (const [""])
      <|>
      toMaybe
         (NameC.match "table:table-cell" foundName)
         (flattenRepeatedCell $
            let repAttr = "table:number-columns-repeated" in
            case fmap reads $ Attr.lookupLit repAttr attrs of
               Just [(k,_)] -> k
               _ -> 1)

extractTablesContents :: [Tag.T Name.T String] -> [(String, [[String]])]
extractTablesContents =
   map (mapSnd (
         map
            (concatMap (uncurry ($))
             .
             snd
             .
             ListHT.segmentBeforeJust maybeCell
             .
             NonEmpty.tail)
         .
         snd
         .
         NonEmptyM.segmentBefore
            (TagMatch.openNameLit "table:table-row"))) .
   snd . ListHT.segmentBeforeJust maybeTableName

contentFromTables :: Char -> IO BC.ByteString -> IO ()
contentFromTables separator readInput =
   mapM_
      (\(tableName, content) -> do
         putStrLn ""
         putStrLn tableName
         putStrLn (Spreadsheet.toString '"' separator content)) .
   extractTablesContents .
   TagParser.runSoup . decode
      =<< readInput

contentFromTable :: Char -> Either String Int -> IO BC.ByteString -> IO ()
contentFromTable separator tableSelector readInput = do
   tables <- extractTablesContents . TagParser.runSoup . decode <$> readInput
   putStr . Spreadsheet.toString '"' separator =<<
      case filter (\(tableId, (tableName, _content)) ->
                     either (tableName==) (tableId==) tableSelector) $
           zip [1..] tables of
         (_,(_,found)):_ -> return found
         _ ->
            exitFailureMsg $
               "table with " ++
               either (printf "name %s") (printf "number %d") tableSelector ++
               " not found"


parser :: OP.Parser (IO ())
parser =
   ((OP.flag' listTables $
         OP.long "list-tables" <>
         OP.help "List all tables in an ODS document")
      <|>
      (pure contentFromTable
         <*>
         (OP.option
            (OP.eitherReader (\str ->
               case str of
                  "TAB" -> Right '\t'
                  [c] -> Right c
                  _ -> Left "separator must be one character")) $
            OP.long "separator" <>
            OP.metavar "CHAR" <>
            OP.value ',' <>
            OP.help "CSV separator, TAB for tabulator")
         <*>
         (
            (fmap Left $
             OP.strOption $
               OP.long "sheetname" <>
               OP.metavar "NAME" <>
               OP.help "Select table by name")
            <|>
            (fmap Right $
             OP.option (OP.eitherReader $ parseNumber "page" (0<) "positive") $
               OP.long "sheetnumber" <>
               OP.metavar "ONEBASED" <>
               OP.help "Select table by number")
         )))
   <*>
      OP.argument
         (OP.maybeReader $ Just . readByteContent)
         (OP.metavar "INPUT" <>
          OP.value BC.getContents <>
          OP.help "Input Document")

info :: OP.Parser a -> OP.ParserInfo a
info p =
   OP.info
      (OP.helper <*> p)
      (OP.fullDesc <>
       OP.progDesc "Convert Open Document Spreadsheet ODS to CSV.")

main :: IO ()
main = join $ OP.execParser $ info parser
