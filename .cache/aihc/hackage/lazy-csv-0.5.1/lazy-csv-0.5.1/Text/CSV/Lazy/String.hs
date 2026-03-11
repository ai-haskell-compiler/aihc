-- | The CSV (comma-separated value) format is defined by RFC 4180,
--   \"Common Format and MIME Type for Comma-Separated Values (CSV) Files\",
--   <http://www.rfc-editor.org/rfc/rfc4180.txt>
--
--   This lazy parser can report all CSV formatting errors, whilst also
--   returning all the valid data, so the user can choose whether to
--   continue, to show warnings, or to halt on error.
--
--   Valid fields retain information about their original location in the
--   input, so a secondary parser from textual fields to typed values
--   can give intelligent error messages.
--
--   In a valid CSV file, all rows must have the same number of columns.
--   This parser will flag a row with the wrong number of columns as a error.
--   (But the error type contains the actual data, so the user can recover
--   it if desired.)  Completely blank lines are also treated as errors,
--   and again the user is free either to filter these out or convert them
--   to a row of actual null fields.

module Text.CSV.Lazy.String
  ( -- * CSV types
    CSVTable
  , CSVRow
  , CSVField(..)
    -- * CSV parsing
  , CSVError(..)
  , CSVResult
  , csvErrors
  , csvTable
  , csvTableFull
  , csvTableHeader
  , parseCSV
  , parseDSV
    -- * Pretty-printing
  , ppCSVError
  , ppCSVField
  , ppCSVTable
  , ppDSVTable
    -- * Conversion between standard and simple representations
  , fromCSVTable
  , toCSVTable
    -- * Selection, validation, and algebra of CSV tables
  , selectFields
  , expectFields
  , mkEmptyColumn
  , joinCSV
  , mkCSVField
  ) where

import Data.List (groupBy, partition, elemIndex, intercalate, takeWhile
                 ,deleteFirstsBy, nub)
import Data.Function (on)
import Data.Maybe (fromJust)

-- | A CSV table is a sequence of rows.  All rows have the same number
--   of fields.
type CSVTable   = [CSVRow]

-- | A CSV row is just a sequence of fields.
type CSVRow     = [CSVField]

-- | A CSV field's content is stored with its logical row and column number,
--   as well as its textual extent.  This information is necessary if you
--   want to generate good error messages in a secondary parsing stage,
--   should you choose to convert the textual fields to typed data values.
data CSVField   = CSVField       { csvRowNum        :: !Int
                                 , csvColNum        :: !Int
                                 , csvTextStart     :: !(Int,Int)
                                 , csvTextEnd       :: !(Int,Int)
                                 , csvFieldContent  :: !String
                                 , csvFieldQuoted   :: !Bool }
                | CSVFieldError  { csvRowNum        :: !Int
                                 , csvColNum        :: !Int
                                 , csvTextStart     :: !(Int,Int)
                                 , csvTextEnd       :: !(Int,Int)
                                 , csvFieldError    :: !String }
                                                    deriving (Eq,Show)

-- | A structured error type for CSV formatting mistakes.
data CSVError   = IncorrectRow   { csvRow           :: !Int
                                 , csvColsExpected  :: !Int
                                 , csvColsActual    :: !Int
                                 , csvFields        :: [CSVField] }
                | BlankLine      { csvRow           :: !Int
                                 , csvColsExpected  :: !Int
                                 , csvColsActual    :: !Int
                                 , csvField         :: CSVField }
                | FieldError     { csvField         :: CSVField }
                | DuplicateHeader{ csvColsExpected  :: !Int
                                 , csvHeaderSerial  :: !Int
                                 , csvDuplicate     :: !String }
                | NoData
                                                    deriving (Eq,Show)

-- | The result of parsing a CSV input is a mixed collection of errors
--   and valid rows.  This way of representing things is crucial to the
--   ability to parse lazily whilst still catching format errors.
type CSVResult  = [Either [CSVError] CSVRow]

-- | Extract just the valid portions of a CSV parse.
csvTable    :: CSVResult -> CSVTable
csvTable  r  = [ v | Right v <- r ]
-- | Extract just the errors from a CSV parse.
csvErrors   :: CSVResult -> [CSVError]
csvErrors r  = concat [ v | Left  v <- r ]
-- | Extract the full table, including invalid rows, repaired with padding.
--   and de-duplicated headers.
csvTableFull:: CSVResult -> CSVTable
csvTableFull = map beCareful . deduplicate
    where beCareful (Right row) = row
          beCareful (Left (r@IncorrectRow{}:_)) =
              csvFields r ++
              replicate (csvColsExpected r - csvColsActual r)
                        (mkCSVField (csvRow r) 0 "")
          beCareful (Left (r@BlankLine{}:_)) =
              replicate (csvColsExpected r)
                        (mkCSVField (csvRow r) 0 "")
          beCareful (Left (r@DuplicateHeader{}:_)) = -- obsolete with deduping
              replicate (csvColsExpected r)
                        (mkCSVField 0 0 "")
          beCareful (Left (FieldError{}:r)) = beCareful (Left r)
          beCareful (Left (NoData:_))       = []
          beCareful (Left [])               = []

          deduplicate (Left (errs@(DuplicateHeader{}:_)):Right heads:rows) =
--               Right (reverse $ foldl replace [] heads)
                 Right (replaceInOrder errs (zip heads [0..]))
                 : rows
          deduplicate rows = rows
{-
          replace output header
              | headerName `elem` map csvFieldContent output
                          = header{ csvFieldContent=headerName++"_duplicate" }
                                  : output
              | otherwise = header: output
              where headerName = csvFieldContent header
-}
          replaceInOrder []       headers        = map fst headers
          replaceInOrder _        []             = []
          replaceInOrder (d:dups) ((h,n):headers)
              | csvHeaderSerial d == n = h{ csvFieldContent =
                                                (csvDuplicate d++"_"++show n) }
                                          : replaceInOrder dups     headers
              | otherwise              = h: replaceInOrder (d:dups) headers

-- | The header row of the CSV table, assuming it is non-empty.
csvTableHeader :: CSVResult -> [String]
csvTableHeader = map csvFieldContent . firstRow
    where firstRow (Left _: rest) = firstRow rest
          firstRow (Right x: _)   = x


-- | A first-stage parser for CSV (comma-separated values) data.
--   The individual fields remain as text, but errors in CSV formatting
--   are reported.  Errors (containing unrecognisable rows/fields) are
--   interspersed with the valid rows/fields.
parseCSV :: String -> CSVResult
parseCSV = parseDSV True ','

-- | Sometimes CSV is not comma-separated, but delimiter-separated
--   values (DSV).  The choice of delimiter is arbitrary, but semi-colon
--   is common in locales where comma is used as a decimal point, and tab
--   is also common.  The Boolean argument is
--   whether newlines should be accepted within quoted fields.  The CSV RFC
--   says newlines can occur in quotes, but other DSV formats might say
--   otherwise.  You can often get better error messages if newlines are
--   disallowed.
parseDSV :: Bool -> Char -> String -> CSVResult
parseDSV qn delim = validate
                    . groupBy ((==)`on`csvRowNum)
                    . lexCSV qn delim

validate          :: [CSVRow] -> CSVResult
validate []        = [Left [NoData]]
validate xs@(x:_)  = checkDuplicateHeaders x $ map (extractErrs (length x)) xs

extractErrs       :: Int -> CSVRow -> Either [CSVError] CSVRow
extractErrs size row
    | length row0 == size && null errs0    = Right row0
    | length row0 == 1    && empty field0  = Left [blankLine field0]
    | otherwise                            = Left (map convert errs0
                                                   ++ validateColumns row0)
  where
  (row0,errs0)   = partition isField row
  (field0:_)     = row0

  isField (CSVField{})      = True
  isField (CSVFieldError{}) = False

  empty   f@(CSVField{})    = null (csvFieldContent f)
  empty   _                 = False

  convert err = FieldError {csvField = err}

  validateColumns r  =
      if length r == size then []
      else [ IncorrectRow{ csvRow  = if null r then 0 else csvRowNum (head r)
                         , csvColsExpected  = size
                         , csvColsActual    = length r
                         , csvFields        = r } ]
  blankLine f = BlankLine{ csvRow           = csvRowNum f
                         , csvColsExpected  = size
                         , csvColsActual    = 1
                         , csvField         = f }

checkDuplicateHeaders :: CSVRow -> CSVResult -> CSVResult
checkDuplicateHeaders row result =
    let headers = [ f | f@(CSVField{}) <- row ]
        dups    = deleteFirstsBy ((==)`on`csvFieldContent)
                                 headers (nub headers)
        n       = length headers
    in if null dups then result
       else Left (map (\d-> DuplicateHeader
                              { csvColsExpected = n
                              , csvHeaderSerial = csvColNum d
                              , csvDuplicate    = csvFieldContent d })
                      dups)
            : result


-- Reading CSV data is essentially lexical, and can be implemented with a
-- simple finite state machine.  We keep track of logical row number,
-- logical column number (in tabular terms), and textual position (row,col)
-- to enable good error messages.
-- Positional data is retained even after successful lexing, in case a
-- second-stage field parser wants to complain.
--
-- A double-quoted CSV field may contain commas, newlines, and double quotes.

data CSVState  = CSVState  { tableRow, tableCol  :: !Int
                           , textRow,  textCol   :: !Int }

incTableRow, incTableCol, incTextRow, incTextCol :: CSVState -> CSVState
incTableRow  st = st { tableRow  = tableRow  st + 1 }
incTableCol  st = st { tableCol  = tableCol  st + 1 }
incTextRow   st = st { textRow   = textRow   st + 1 }
incTextCol   st = st { textCol   = textCol   st + 1 }

-- Lexer is a small finite state machine.
lexCSV :: Bool -> Char -> [Char] -> [CSVField]
lexCSV quotedNewline delim =
    simple CSVState{tableRow=1,tableCol=1,textRow=1,textCol=1} (1,1) []
  where
  -- 'simple' recognises an unquoted field, and delimiter char as separator
  simple :: CSVState -> (Int,Int) -> String -> String -> [CSVField]
  simple _ _     []         []    = []
  simple s begin acc        []    = mkField s begin acc False : []
  simple s begin acc     (c:cs)
            | not (interesting c) = simple (incTextCol $! s) begin (c:acc) cs
  simple s begin acc  (c:'"':cs)
                      | c==delim  = mkField s begin acc False :
                                    string s' (textRow s',textCol s') [] cs
                                    where s' = incTextCol . incTextCol .
                                               incTableCol $! s
  simple s begin acc  (c:cs)
                      | c==delim  = mkField s begin acc False :
                                    simple s' (textRow s',textCol s') [] cs
                                    where s' = incTableCol . incTextCol $! s
  simple s begin acc  ('\r':'\n':cs)
                                  = mkField s begin acc False :
                                    simple s' (textRow s',1) [] cs
                                    where s' = incTableRow . incTextRow $!
                                               s {tableCol=1, textCol=1}
  simple s begin acc  ('\n' :cs)  = mkField s begin acc False :
                                    simple s' (textRow s',1) [] cs
                                    where s' = incTableRow . incTextRow $!
                                               s {tableCol=1, textCol=1}
  simple s begin acc  ('\r' :cs)  = mkField s begin acc False :
                                    simple s' (textRow s',1) [] cs
                                    where s' = incTableRow . incTextRow $!
                                               s {tableCol=1, textCol=1}
  simple s begin []   ('"'  :cs)  = string (incTextCol $! s) begin [] cs
  simple s begin acc  ('"'  :cs)  = mkError s begin
                                            "Start-quote not next to comma":
                                    string (incTextCol $! s) begin acc cs

  -- 'string' recognises a double-quoted field containing commas and newlines
  string :: CSVState -> (Int,Int) -> String -> String -> [CSVField]
  string s begin []   []          = mkError s begin "Data ends at start-quote":
                                    []
  string s begin acc  []          = mkError s begin "Data ends in quoted field":
                                    []
  string s begin acc   (c:cs)
    | not (interestingInString c) = string (incTextCol $! s) begin (c:acc) cs
  string s begin acc ('"':'"':cs) = string s' begin ('"':acc) cs
                                    where s' = incTextCol . incTextCol $! s
  string s begin acc ('"':c:'"':cs)
                       | c==delim = mkField s begin acc True :
                                    string s' (textRow s',textCol s') [] cs
                                    where s' = incTextCol . incTextCol .
                                               incTextCol . incTableCol $! s
  string s begin acc ('"':c:cs)
                       | c==delim = mkField s begin acc True :
                                    simple s' (textRow s',textCol s') [] cs
                                    where s' = incTextCol . incTextCol .
                                               incTableCol $! s
  string s begin acc ('"':'\n':cs)= mkField s begin acc True :
                                    simple s' (textRow s',1) [] cs
                                    where s' = incTableRow . incTextRow $!
                                               s {tableCol=1, textCol=1}
  string s begin acc ('"':'\r':'\n':cs)
                                  = mkField s begin acc True :
                                    simple s' (textRow s',1) [] cs
                                    where s' = incTableRow . incTextRow $!
                                               s {tableCol=1, textCol=1}
  string s begin acc ('"':[])     = mkField s begin acc True : []
  string s begin acc ('"':cs)     = mkError s begin
                                            "End-quote not followed by comma":
                                    simple (incTextCol $! s) begin acc cs
  string s begin acc ('\r':'\n':cs)
                  | quotedNewline = string s' begin ('\n':acc) cs
                  | otherwise     = mkError s begin
                                            "Found newline within quoted field":
                                    simple s'' (textRow s'',textCol s'') [] cs
                                    where s'  = incTextRow $! s {textCol=1}
                                          s'' = incTableRow . incTextRow $!
                                                s {textCol=1, tableCol=1}
  string s begin acc ('\n' :cs)
                  | quotedNewline = string s' begin ('\n':acc) cs
                  | otherwise     = mkError s begin
                                            "Found newline within quoted field":
                                    simple s'' (textRow s'',textCol s'') [] cs
                                    where s'  = incTextRow $! s {textCol=1}
                                          s'' = incTableRow . incTextRow $!
                                                s {textCol=1, tableCol=1}

  interesting :: Char -> Bool
  interesting '\n' = True
  interesting '\r' = True
  interesting '"'  = True
  interesting c    = c==delim

  interestingInString :: Char -> Bool
  interestingInString '\n' = True
  interestingInString '\r' = True
  interestingInString '"'  = True
  interestingInString _    = False

  -- generate the lexical tokens representing either a field or an error
  mkField st begin f q =    CSVField { csvRowNum       = tableRow st
                                     , csvColNum       = tableCol st
                                     , csvTextStart    = begin
                                     , csvTextEnd      = (textRow st,textCol st)
                                     , csvFieldContent = reverse f
                                     , csvFieldQuoted  = q }
  mkError st begin e = CSVFieldError { csvRowNum       = tableRow st
                                     , csvColNum       = tableCol st
                                     , csvTextStart    = begin
                                     , csvTextEnd      = (textRow st,textCol st)
                                     , csvFieldError   = e }

-- | Some pretty-printing for structured CSV errors.
ppCSVError :: CSVError -> String
ppCSVError (err@IncorrectRow{}) =
        "\nRow "++show (csvRow err)++" has wrong number of fields."++
        "\n    Expected "++show (csvColsExpected err)++" but got "++
        show (csvColsActual err)++"."++
        "\n    The fields are:"++
        indent 8 (concatMap ppCSVField (csvFields err))
ppCSVError (err@BlankLine{})  =
        "\nRow "++show (csvRow err)++" is blank."++
        "\n    Expected "++show (csvColsExpected err)++" fields."
ppCSVError (err@FieldError{}) = ppCSVField (csvField err)
ppCSVError (err@DuplicateHeader{}) =
        "\nThere are two (or more) identical column headers: "++
        show (csvDuplicate err)++"."++
        "\n    Column number "++show (csvHeaderSerial err)
ppCSVError (err@NoData{})     =
        "\nNo usable data (after accounting for any other errors)."

-- | Pretty-printing for CSV fields, shows positional information in addition
--   to the textual content.
ppCSVField :: CSVField -> String
ppCSVField (f@CSVField{}) =
        "\n"++quoted (csvFieldQuoted f) (csvFieldContent f)++
        "\nin row "++show (csvRowNum f)++" at column "++show (csvColNum f)++
        " (textually from "++show (csvTextStart f)++" to "++
        show (csvTextEnd f)++")"
ppCSVField (f@CSVFieldError{}) =
        "\n"++csvFieldError f++
        "\nin row "++show (csvRowNum f)++" at column "++show (csvColNum f)++
        " (textually from "++show (csvTextStart f)++" to "++
        show (csvTextEnd f)++")"

-- | Turn a full CSV table back into text, as much like the original
--   input as possible,  e.g. preserving quoted/unquoted format of fields.
ppCSVTable :: CSVTable -> String
ppCSVTable = unlines . map (intercalate "," . map ppField)
  where ppField f = quoted (csvFieldQuoted f) (csvFieldContent f)

-- | Turn a full CSV table back into text, using the given delimiter
--   character.  Quoted/unquoted formatting of the original is preserved.
--   The Boolean argument is to repair fields containing newlines, by
--   replacing the nl with a space.
ppDSVTable :: Bool -> Char -> CSVTable -> String
ppDSVTable nl delim = unlines . map (intercalate [delim] . map ppField)
  where ppField f = quoted (csvFieldQuoted f) (doNL $ csvFieldContent f)
        doNL | nl        = replaceNL
             | otherwise = id


-- Some pp helpers - indent and quoted - should live elsewhere, in a
-- pretty-printing package.

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

quoted :: Bool -> String -> String
quoted False  s  = s
quoted True   s  = '"': escape s ++"\""
  where escape ('"':cs) = '"':'"': escape cs
        escape (c:cs)   = c: escape cs
        escape  []      = []

replaceNL :: String -> String
replaceNL ('\n':s) = ' ':replaceNL s
replaceNL (c:s)    = c: replaceNL s
replaceNL []       = []


-- | Convert a CSV table to a simpler representation, by dropping all
--   the original location information.
fromCSVTable :: CSVTable -> [[String]]
fromCSVTable = map (map csvFieldContent)

-- | Convert a simple list of lists into a CSVTable by the addition of
--   logical locations.  (Textual locations are not so useful.)
--   Rows of varying lengths generate errors.  Fields that need
--   quotation marks are automatically marked as such.
toCSVTable   :: [[String]] -> ([CSVError], CSVTable)
toCSVTable []         = ([NoData], [])
toCSVTable rows@(r:_) = (\ (a,b)-> (concat a, b)) $
                        unzip (zipWith walk [1..] rows)
  where
    n            = length r
    walk        :: Int -> [String] -> ([CSVError], CSVRow)
    walk rnum [] = ( [blank rnum]
                   , map (\c-> mkCSVField rnum c "") [1..n])
    walk rnum cs = ( if length cs /= n then [bad rnum cs] else []
                   , zipWith (mkCSVField rnum) [1..n] cs )

    blank rnum =  BlankLine{ csvRow          = rnum
                           , csvColsExpected = n
                           , csvColsActual   = 0
                           , csvField        = mkCSVField rnum 0 ""
                           }
    bad r cs = IncorrectRow{ csvRow          = r
                           , csvColsExpected = n
                           , csvColsActual   = length cs
                           , csvFields       = zipWith (mkCSVField r) [1..] cs
                           }

-- | Select and/or re-arrange columns from a CSV table, based on names in the
--   header row of the table.  The original header row is re-arranged too.
--   The result is either a list of column names that were not present, or
--   the (possibly re-arranged) sub-table.
selectFields :: [String] -> CSVTable -> Either [String] CSVTable
selectFields names table
    | null table          = Left names
    | not (null missing)  = Left missing
    | otherwise           = Right (map select table)
  where
    header         = map csvFieldContent (head table)
    missing        = filter (`notElem` header) names
    reordering     = map (fromJust . (\n-> elemIndex n header)) names
    select fields  = map (fields!!) reordering

-- | Validate that the named columns of a table have exactly the names and
--   ordering given in the argument.
expectFields :: [String] -> CSVTable -> Either [String] CSVTable
expectFields names table
    | null table          = Left ["CSV table is empty"]
    | not (null missing)  = Left (map ("CSV table is missing field: "++)
                                       missing)
    | header /= names     = Left ["CSV columns are in the wrong order"
                                 ,"Expected: "++intercalate ", " names
                                 ,"Found:    "++intercalate ", " header]
    | otherwise           = Right table
  where
    header         = map csvFieldContent (head table)
    missing        = filter (`notElem` header) names

-- | A join operator, adds the columns of two tables together.
--   Precondition: the tables have the same number of rows.
joinCSV :: CSVTable -> CSVTable -> CSVTable
joinCSV = zipWith (++)

-- | A generator for a new CSV column, of arbitrary length.
--   The result can be joined to an existing table if desired.
mkEmptyColumn :: String -> CSVTable
mkEmptyColumn header = [mkCSVField 1 0 header] :
                       map (\n-> [mkCSVField n 0 ""]) [2..]

-- | Generate a fresh field with the given textual content.
--   The quoting flag is set automatically based on the text.
--   Textual extents are not particularly useful, since there was no original
--   input to refer to.
mkCSVField :: Int -> Int -> String -> CSVField
mkCSVField n c text =
        CSVField { csvRowNum       = n
                 , csvColNum       = c
                 , csvTextStart    = (0,0)
                 , csvTextEnd      = (length (filter (=='\n') text)
                                     ,length . takeWhile (/='\n')
                                             . reverse $ text )
                 , csvFieldContent = text
                 , csvFieldQuoted  = any (`elem`"\",\n\r") text
                 }

