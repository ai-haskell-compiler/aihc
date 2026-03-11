module Data.Spreadsheet (
   T,
   -- * parsing
   fromString,
   fromStringWithRemainder,
   fromStringSimple,
   Parser.UserMessage,
   -- * formatting
   toString,
   toStringSimple,
   ) where

import Data.List.HT  (chop, switchR, )
import Data.List     (intersperse, )
import Data.Maybe.HT (toMaybe, )

import qualified Data.Spreadsheet.Parser as Parser
import Control.Monad.Trans.State (runState, )
import Control.Monad (liftM, mplus, )

import qualified Control.Monad.Exception.Asynchronous as Async
import qualified Data.Spreadsheet.CharSource as CharSource

{- $setup
>>> import qualified Data.Spreadsheet as Spreadsheet
>>> import qualified Control.Monad.Exception.Asynchronous.Lazy as MEA
>>> import qualified Test.QuickCheck as QC
-}


{- |
A spreadsheet is a list of lines,
each line consists of cells,
and each cell is a string.
Ideally, spreadsheets read from a CSV file
have lines with the same number of cells per line.
However, we cannot assert this,
and thus we parse the lines as they come in.
-}
type T = [[String]]

parseChar :: CharSource.C source =>
   Char -> Parser.Fallible source Char
parseChar qm =
   Parser.eitherOr
      (Parser.satisfy (qm/=))
      (Parser.string [qm,qm] >> return qm)

parseQuoted :: CharSource.C source =>
   Char -> Parser.PartialFallible source String
parseQuoted qm =
   Parser.between "missing closing quote"
      (Parser.char qm) (Parser.char qm)
      (liftM Async.pure $ Parser.many (parseChar qm))

parseUnquoted :: CharSource.C source =>
   Char -> Char -> Parser.Straight source String
parseUnquoted qm sep =
   Parser.many
      (Parser.satisfy (not . flip elem [qm,sep,'\r','\n']))

parseCell :: CharSource.C source =>
   Char -> Char -> Parser.Partial source String
parseCell qm sep =
   Parser.deflt (liftM Async.pure $ parseUnquoted qm sep) (parseQuoted qm)

parseLine :: CharSource.C source =>
   Char -> Char -> Parser.Partial source [String]
parseLine qm sep =
   Parser.sepByIncomplete (Parser.char sep) (CharSource.fallible $ parseCell qm sep)

parseLineEnd :: CharSource.C source =>
   Parser.Fallible source ()
parseLineEnd =
   (Parser.char '\r' >> (Parser.char '\n' `Parser.eitherOr` return ()))
   `Parser.eitherOr`
   Parser.char '\n'

parseLineWithEnd :: CharSource.C source =>
   Char -> Char -> Parser.Partial source [String]
parseLineWithEnd qm sep =
   Parser.terminated "line end expected" parseLineEnd $
   parseLine qm sep


parseTable :: CharSource.C source =>
   Char -> Char -> Parser.Partial source [[String]]
parseTable qm sep =
   Parser.manyIncomplete $
{-
   CharSource.fallible $ parseLineWithEnd qm sep
-}
   CharSource.fallible CharSource.isEnd >>= \b ->
   if b then CharSource.stop else CharSource.fallible $ parseLineWithEnd qm sep

{- |
@fromString qm sep text@ parses @text@ into a spreadsheet,
using the quotation character @qm@ and the separator character @sep@.

>>> Spreadsheet.fromString '"' '\t' "\"hello\"\t\"world\"\n\"end\"\n"
Exceptional {exception = Nothing, result = [["hello","world"],["end"]]}
>>> Spreadsheet.fromString '"' ',' "\"hello,world\",\"really\"\n\"end\"\n"
Exceptional {exception = Nothing, result = [["hello,world","really"],["end"]]}
>>> Spreadsheet.fromString '"' ';' "\"hello \"\"world\"\"\"\n\"really\"\n"
Exceptional {exception = Nothing, result = [["hello \"world\""],["really"]]}
>>> Spreadsheet.fromString '"' ',' "\"hello\nworld\"\n"
Exceptional {exception = Nothing, result = [["hello\nworld"]]}
-}
fromString :: Char -> Char -> String -> Async.Exceptional Parser.UserMessage T
fromString qm sep str =
   let (Async.Exceptional e (table, rest)) =
          fromStringWithRemainder qm sep str
   in  Async.Exceptional
          (mplus e (toMaybe (not (null rest)) "junk after table")) table

{- |
@fromString qm sep text@ parses @text@ into a spreadsheet
and additionally returns text that follows after CSV formatted data.
-}
fromStringWithRemainder ::
   Char -> Char -> String -> Async.Exceptional Parser.UserMessage (T, String)
fromStringWithRemainder qm sep str =
   let (~(Async.Exceptional e table), rest) =
          runState (CharSource.runString (parseTable qm sep)) str
   in  Async.Exceptional e (table, rest)


{- |
>>> Spreadsheet.toString '"' '\t' [["hello","world"],["end"]]
"\"hello\"\t\"world\"\n\"end\"\n"
>>> Spreadsheet.toString '"' ',' [["hello,world","really"],["end"]]
"\"hello,world\",\"really\"\n\"end\"\n"
>>> Spreadsheet.toString '"' ';' [["hello \"world\""],["really"]]
"\"hello \"\"world\"\"\"\n\"really\"\n"
>>> Spreadsheet.toString '"' ',' [["hello\nworld"]]
"\"hello\nworld\"\n"
>>> take 50 $ Spreadsheet.toString '"' ',' $ repeat ["hello","world"]
"\"hello\",\"world\"\n\"hello\",\"world\"\n\"hello\",\"world\"\n\"h"
>>> take 50 $ Spreadsheet.toString '"' ',' [cycle ["hello","world"]]
"\"hello\",\"world\",\"hello\",\"world\",\"hello\",\"world\",\"h"

prop> :{
   QC.forAll (QC.elements ";,\t ") $ \sep tableNE ->
   let table = map QC.getNonEmpty tableNE in
   table ==
   MEA.result (Spreadsheet.fromString '"' sep
                  (Spreadsheet.toString '"' sep table))
:}
-}
toString :: Char -> Char -> T -> String
toString qm sep =
   unlines . map (concat . intersperse [sep] . map (quote qm))

quote :: Char -> String -> String
quote qm s = qm : foldr (\c cs -> c : if c==qm then qm:cs else cs) [qm] s
-- quote qm s = [qm] ++ replace [qm] [qm,qm] s ++ [qm]


{- |
This is a quick hack.
It does neither handle field nor line separators within quoted fields.
You must provide well-formed CSV content
without field and line separators within quotations.
Everything else yields an error.
-}
fromStringSimple :: Char -> Char -> String -> T
fromStringSimple qm sep =
   map (map (dequoteSimpleOptional qm) . chop (sep==)) . lines

toStringSimple :: Char -> Char -> T -> String
toStringSimple qm sep =
   unlines . map (concat . intersperse [sep] . map (\s -> [qm]++s++[qm]))

_dequoteSimple :: Eq a => a -> [a] -> [a]
_dequoteSimple _ [] = error "dequoteSimple: string is empty"
_dequoteSimple qm (x:xs) =
   if x /= qm
     then error "dequoteSimple: quotation mark missing at beginning"
     else
       switchR
         (error "dequoteSimple: string consists only of a single quotation mark")
         (\ys y ->
            ys ++
            if y == qm
              then []
              else error "dequoteSimple: string does not end with a quotation mark")
         xs

dequoteSimpleOptional :: Eq a => a -> [a] -> [a]
dequoteSimpleOptional _ [] = []
dequoteSimpleOptional qm xt@(x:xs) =
   if x /= qm
     then unescapeQuoteSimple qm xt
     else
       switchR
         (error "dequoteSimpleOptional: string consists only of a single quotation mark")
         (\ys y ->
            unescapeQuoteSimple qm ys ++
            if y == qm
              then []
              else error "dequoteSimpleOptional: string does not end with a quotation mark")
         xs

unescapeQuoteSimple :: Eq a => a -> [a] -> [a]
unescapeQuoteSimple qm =
   let recourse [] = []
       recourse (x:xs) =
          if x /= qm
            then x : recourse xs
            else case xs of
                    [] -> error "unescapeQuoteSimple: single quotation mark at end of string"
                    y:ys ->
                       if y/=qm
                         then error "unescapeQuoteSimple: unmatched quotation mark"
                         else qm : recourse ys
   in  recourse
