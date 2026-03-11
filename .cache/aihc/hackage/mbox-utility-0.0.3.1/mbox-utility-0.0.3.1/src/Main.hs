module Main where

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.Parsec.Rfc2822 as Parser
import Text.ParserCombinators.Parsec ((<|>), )

import qualified Data.Time.Format as Time

import qualified Data.Spreadsheet as Spreadsheet
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.NonEmpty.Mixed as NonEmptyM
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List as List
import qualified Data.Char as Char
import Control.Functor.HT (void, )
import Data.Tuple.HT (mapFst, mapSnd, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (mapMaybe, listToMaybe, )
import Data.List.HT (mapAdjacent, )
import Data.List (genericLength, )


selectFirst :: [String] -> String
selectFirst (x:_) = x
selectFirst _ = ""

-- compatible to old-time:calenderTimeToString
timeFormat :: String
timeFormat = "%a %b %d %H:%M:%S  %Y"

messageHeader :: [Parser.Field] -> [String]
messageHeader fields =
   (selectFirst $ do
      Parser.Date date <- fields
      return $ Time.formatTime Time.defaultTimeLocale timeFormat date) :
   (selectFirst $ do
      Parser.From addrs <- fields
      Parser.NameAddr name addr <- addrs
      return $ maybe addr id name) :
   (selectFirst $ do
      Parser.Subject text <- fields
      return text) :
   []

maybeContentType :: Parser.Field -> Maybe String
maybeContentType fld =
   case fld of
      Parser.OptionalField name value ->
         toMaybe (map Char.toLower name == "content-type") value
      _ -> Nothing

maybeMultipart :: String -> Maybe String
maybeMultipart contentType =
   (\p -> either (const Nothing) Just $ Parsec.parse p "" contentType) $ do
      Parsec.skipMany Parsec.space
      void $ Parsec.string "multipart/mixed;"
      Parsec.skipMany1 Parsec.space
      void $ Parsec.string "boundary="
      (Parsec.between (Parsec.char '"') (Parsec.char '"') $
          Parsec.many (Parsec.noneOf ['"']))
       <|>
       Parsec.many (Parsec.noneOf ['\n', '\r'])

partSummary :: NonEmpty.T [] BC.ByteString -> (Integer, [String])
partSummary rows =
   (genericLength $ NonEmpty.flatten rows,
    case Parsec.parse Parser.message "<part>" $
         concatMap ((++"\r\n") . BC.unpack) $
         NonEmpty.tail rows of
       Left err -> [show err]
       Right (Parser.Message fields _body) ->
          "" : "" :
          case mapMaybe maybeContentType fields of
             [] -> ["missing content-type"]
             ct:_ -> [ct])

nonEmptyLength :: NonEmpty.T [] a -> Integer
nonEmptyLength = NonEmpty.sum . fmap (const 1)

splitMessage :: NonEmpty.T [] BC.ByteString -> [(Integer, [String])]
splitMessage rows =
   case Parsec.parse Parser.message (BC.unpack $ NonEmpty.head rows) $
        concatMap ((++"\r\n") . BC.unpack) $ NonEmpty.tail rows of
      Left err -> [(nonEmptyLength rows, ["", "", show err])]
      Right (Parser.Message fields _body) ->
         maybe [(nonEmptyLength rows, messageHeader fields)] id $ do
            ct <- listToMaybe $ mapMaybe maybeContentType fields
            boundary <- maybeMultipart ct
            return $
               (\(hdr, ((initr, parts), trl)) ->
                  (genericLength hdr + genericLength initr, messageHeader fields) :
                  map partSummary parts ++
                  [(genericLength trl, ["", "", "trailer"])]) $
               mapSnd
                  (mapFst (NonEmptyM.segmentBefore (BC.pack ("--"++boundary) ==)) .
                   break (BC.pack ("--"++boundary++"--") ==)) $
               break BC.null $ NonEmpty.flatten rows

positions :: [[row]] -> [(Integer, Integer)]
positions =
   mapAdjacent (,) . scanl (+) 0 . map List.genericLength

{-
FIXME:
This does not work reliably,
since attachments may contain un-escaped From-lines.
Unfortunately codec-mbox does not solve this problem, too.
-}
splitMessages ::
   BC.ByteString -> ([BC.ByteString], [NonEmpty.T [] BC.ByteString])
splitMessages =
   NonEmptyM.segmentBefore (BC.isPrefixOf $ BC.pack "From ") .
   BC.lines

main :: IO ()
main =
   putStr . Spreadsheet.toString '"' ',' .
   (\(hd, body) ->
      case unzip $ concatMap splitMessage body of
         (lengths, descs) ->
            zipWith
               (\(from,to) desc -> show from : show to : desc)
               (mapAdjacent (,) $ scanl (+) (genericLength hd) lengths)
               descs) .
   splitMessages =<< BC.getContents
