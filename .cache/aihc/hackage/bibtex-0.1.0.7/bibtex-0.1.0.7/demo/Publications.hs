module Main where

import qualified Text.BibTeX.Parse as Parse
import qualified Text.BibTeX.Entry as Entry
import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Data.Char as Char
import System.IO (hPutStrLn, stderr, )


typeTable :: [((String, Maybe String), String)]
typeTable =
   (("article", Just "reviewed"), "reviewedjournal") :
   (("article", Just "popular"), "popular") :
   (("article", Nothing), "journal") :
   (("inproceedings", Just "reviewed"), "reviewedconference") :
   (("inproceedings", Nothing), "conference") :
   (("techreport", Nothing), "techreport") :
   (("inbook", Just "program"), "program") :
   (("misc", Just "program"), "program") :
   (("misc", Just "talk"), "talk") :
   (("mastersthesis", Nothing), "thesis") :
   (("phdthesis", Nothing), "thesis") :
   []


cite :: Entry.T -> String
cite entry =
   maybe
      "% \\nocite"
      ("\\nocite" ++ )
      (lookup
          (map Char.toLower (Entry.entryType entry),
           lookup "subtype" (Entry.fields (Entry.lowerCaseFieldNames entry)))
          typeTable) ++
   "{" ++ Entry.identifier entry ++ "}"


main :: IO ()
main =
   do bib <- getContents
      case Parsec.parse (Parsec.skipMany Parsec.space >> Parse.file) "stdin" bib of
         Left errMsg -> hPutStrLn stderr (show errMsg)
         Right entries ->
            mapM_ (putStrLn . cite) entries
