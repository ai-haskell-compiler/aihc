{- |
   module: Text.CSV 
   license: MIT 
   maintainer: Jaap Weel <weel at ugcs dot caltech dot edu> 
   stability: provisional 
   portability: ghc 

   This module parses and dumps documents that are formatted more or
   less according to RFC 4180, \"Common Format and MIME Type for
   Comma-Separated Values (CSV) Files\",
   <http://www.rfc-editor.org/rfc/rfc4180.txt>.

   There are some issues with this RFC. I will describe what these
   issues are and how I deal with them.

   First, the RFC prescribes CRLF standard network line breaks, but
   you are likely to run across CSV files with other line endings, so
   we accept any sequence of CRs and LFs as a line break. 

   Second, there is an optional header line, but the format for the
   header line is exactly like a regular record and you can only
   figure out whether it exists from the mime type, which may not be
   available. I ignore the issues of header lines and simply turn them
   into regular records.
   
   Third, there is an inconsistency, in that the formal grammar
   specifies that fields can contain only certain US ASCII characters,
   but the specification of the MIME type allows for other character
   sets. I will allow all characters in fields, except for commas, CRs
   and LFs in unquoted fields. This should make it possible to parse
   CSV files in any encoding, but it allows for characters such as
   tabs that the RFC may be interpreted to forbid even in non-US-ASCII
   character sets. 

   NOTE: Several people have asked me to implement extensions that are
   used in non-US versions Microsoft Excel. This library implements
   RFC-compliant CSV, not Microsoft Excel CSV. If you want to write a
   library that deals with the CSV-like formats used by non-US versions
   of Excel or any other software, you should write a separate library. I
   suggest you call it Text.SSV, for "Something Separated Values."
-}

{- Copyright (c) Jaap Weel 2007.  Permission is hereby granted, free
of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the
Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.  THE
SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. -}

module Text.CSV (CSV
                 , Record
                 , Field
                 , csv
                 , parseCSV
                 , parseCSVFromFile
                 , parseCSVTest
                 , printCSV
                 ) where

import Text.ParserCombinators.Parsec
import Data.List (intersperse)

-- | A CSV file is a series of records. According to the RFC, the
-- records all have to have the same length. As an extension, I
-- allow variable length records.
type CSV = [Record]

-- | A record is a series of fields
type Record = [Field]

-- | A field is a string
type Field = String

-- | A Parsec parser for parsing CSV files
csv :: Parser CSV
csv = do x <- record `sepEndBy` many1 (oneOf "\n\r")
         eof
         return x

record :: Parser Record
record = (quotedField <|> field) `sepBy` char ','

field :: Parser Field
field = many (noneOf ",\n\r\"")

quotedField :: Parser Field
quotedField = between (char '"') (char '"') $
              many (noneOf "\"" <|> try (string "\"\"" >> return '"'))

-- | Given a file name (used only for error messages) and a string to
-- parse, run the parser.
parseCSV :: FilePath -> String -> Either ParseError CSV
parseCSV = parse csv

-- | Given a file name, read from that file and run the parser
parseCSVFromFile :: FilePath -> IO (Either ParseError CSV)
parseCSVFromFile = parseFromFile csv

-- | Given a string, run the parser, and print the result on stdout.
parseCSVTest :: String -> IO ()
parseCSVTest = parseTest csv

-- | Given an object of type CSV, generate a CSV formatted
-- string. Always uses escaped fields.
printCSV :: CSV -> String
printCSV records = unlines (printRecord `map` records)
    where printRecord = concat . intersperse "," . map printField
          printField f = "\"" ++ concatMap escape f ++ "\""
          escape '"' = "\"\""
          escape x = [x]
          unlines = concat . intersperse "\n"

