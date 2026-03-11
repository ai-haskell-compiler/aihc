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

module Text.CSV.Lazy.ByteString
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

--  , ppCSVTableAsTuples

import Data.List     (groupBy, partition, elemIndex, intercalate, takeWhile
                     ,deleteFirstsBy, nub)
import Data.Function (on)
import Data.Maybe    (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

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
                                 , csvFieldContent  :: !ByteString
                                 , csvFieldQuoted   :: !Bool }
                | CSVFieldError  { csvRowNum        :: !Int
                                 , csvColNum        :: !Int
                                 , csvTextStart     :: !(Int,Int)
                                 , csvTextEnd       :: !(Int,Int)
                                 , csvFieldError    :: !String }
                                                    deriving (Eq,Show)

-- | A structured error type for CSV formatting mistakes.
data CSVError   = IncorrectRow   { csvRow           :: Int
                                 , csvColsExpected  :: Int
                                 , csvColsActual    :: Int
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
type CSVResult  = [ Either [CSVError] [CSVField] ]

-- | Extract just the valid portions of a CSV parse.
csvTable    :: CSVResult -> CSVTable
csvTable  r  = [ row | Right row <- r ]
-- | Extract just the errors from a CSV parse.
csvErrors   :: CSVResult -> [CSVError]
csvErrors r  = concat [ err | Left err  <- r ]
-- | Extract the full table, including invalid rows, with padding, and
--   de-duplicated headers.
csvTableFull:: CSVResult -> CSVTable
csvTableFull = map beCareful . deduplicate
    where beCareful (Right row) = row
          beCareful (Left (r@IncorrectRow{}:_)) =
              csvFields r ++
              replicate (csvColsExpected r - csvColsActual r)
                        (mkCSVField (csvRow r) 0 BS.empty)
          beCareful (Left (r@BlankLine{}:_)) =
              replicate (csvColsExpected r)
                        (mkCSVField (csvRow r) 0 BS.empty)
          beCareful (Left (r@DuplicateHeader{}:_)) = -- obsolete with deduping
              replicate (csvColsExpected r)
                        (mkCSVField 0 0 BS.empty)
          beCareful (Left (FieldError{}:r))      = beCareful (Left r)
          beCareful (Left (NoData:_))            = []
          beCareful (Left [])                    = []

          deduplicate (Left (errs@(DuplicateHeader{}:_)):Right heads:rows) = 
--               Right (reverse $ foldl replace [] heads)
                 Right (replaceInOrder errs (zip heads [0..]))
                 : rows
          deduplicate rows = rows
{-
          replace output header
              | headerName `elem` map csvFieldContent output
                          = header{ csvFieldContent = headerName
                                            `BS.append` BS.pack "_duplicate" }
                                  : output
              | otherwise = header: output
              where headerName = csvFieldContent header
-}
          replaceInOrder []       headers        = map fst headers
          replaceInOrder _        []             = []
          replaceInOrder (d:dups) ((h,n):headers)
              | csvHeaderSerial d == n = h{ csvFieldContent = BS.pack
                                                (csvDuplicate d++"_"++show n) }
                                          : replaceInOrder dups     headers
              | otherwise              = h: replaceInOrder (d:dups) headers

-- | The header row of the CSV table, assuming it is non-empty.
csvTableHeader :: CSVResult -> [String]
csvTableHeader = map (BS.unpack . csvFieldContent) . firstRow
    where firstRow (Left _: rest) = firstRow rest
          firstRow (Right x: _)   = x


-- | A first-stage parser for CSV (comma-separated values) data.
--   The individual fields remain as text, but errors in CSV formatting
--   are reported.  Errors (containing unrecognisable rows/fields) are
--   interspersed with the valid rows/fields.
parseCSV :: ByteString -> CSVResult
parseCSV = parseDSV True ','

-- | Sometimes CSV is not comma-separated, but delimiter-separated
--   values (DSV).  The choice of delimiter is arbitrary, but semi-colon
--   is common in locales where comma is used as a decimal point, and tab
--   is also common.  The Boolean argument is
--   whether newlines should be accepted within quoted fields.  The CSV RFC
--   says newlines can occur in quotes, but other DSV formats might say
--   otherwise.  You can often get better error messages if newlines are
--   disallowed.
parseDSV :: Bool -> Char -> ByteString -> CSVResult
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
  (row0,errs0)  = partition isField row
  (field0:_)    = row0

  isField (CSVField{})       = True
  isField (CSVFieldError{})  = False

  empty   f@(CSVField{})    = BS.null (csvFieldContent f)
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
                              , csvDuplicate = BS.unpack (csvFieldContent d)})
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
    deriving Show

incTableRow, incTableCol, incTextRow :: CSVState -> CSVState
incTableRow  st = st { tableRow  = tableRow  st + 1 , tableCol = 1 }
incTableCol  st = st { tableCol  = tableCol  st + 1 }
incTextRow   st = st { textRow   = textRow   st + 1 , textCol = 1 }

incTextCol :: Int -> CSVState -> CSVState
incTextCol n st = st { textCol   = textCol   st + n }

here :: CSVState -> (Int,Int)
here st = (textRow st, textCol st)

-- Lexer is a small finite state machine.
lexCSV :: Bool -> Char -> ByteString -> [CSVField]
lexCSV qn delim =
    getFields qn delim
              (CSVState{tableRow=1,tableCol=1,textRow=1,textCol=1}) (1,1)

getFields :: Bool -> Char -> CSVState -> (Int, Int) -> ByteString -> [CSVField]
getFields q d state begin bs0
 = case BS.uncons bs0 of
   Nothing -> []
   Just ('"', bs1) -> doStringFieldContent q d (incTextCol 1 state) begin
                                           BS.empty bs1
   _ ->
       case BS.break interestingChar bs0 of
       (fieldBs, bs1) ->
           let field   = mkField end begin fieldBs False
               end     = incTextCol (len-1) $ state
               state'  = incTableCol $ incTextCol 2 end
               stateNL = incTableRow . incTextRow $ state
               len     = fromIntegral $ BS.length fieldBs
           in case BS.uncons bs1 of
              Just (c,bs2)
                   | c==d     -> field: getFields q d state' (here state') bs2
              Just ('\r',bs2) ->
                  case BS.uncons bs2 of
                  Just ('\n',bs3)
                              -> field: getFields q d stateNL (here stateNL) bs3
                                 -- XXX This could be an error instead:
                  _           -> field: getFields q d stateNL (here stateNL) bs2
              Just ('\n',bs2) -> field: getFields q d stateNL (here stateNL) bs2
              Just ('"', _)   -> field:
                                 mkError state' begin
                                         "unexpected quote, resync at EOL":
                                 getFields q d stateNL (here stateNL)
                                           (BS.dropWhile (/='\n') bs1)
              Just _          -> [mkError state' begin "XXX Can't happen"]
              Nothing         -> field: getFields q d stateNL (here stateNL) bs1
 where interestingChar '\r' = True
       interestingChar '\n' = True
       interestingChar '"'  = True
       interestingChar c    | c==d = True
       interestingChar _    = False

doStringFieldContent :: Bool -> Char -> CSVState -> (Int, Int) -> ByteString
                     -> ByteString -> [CSVField]
doStringFieldContent q d state begin acc bs1
 = case BS.break interestingCharInsideString bs1 of
   (newBs, bs2) ->
       let fieldBs = acc `BS.append` newBs
           field   = mkField end  begin fieldBs True
           end     = incTextCol (len-1) state
           state'  = incTableCol $ incTextCol 3 end
           stateNL = incTableRow . incTextRow $ state
           len     = fromIntegral $ BS.length newBs
       in case BS.uncons bs2 of
          Just ('\r',bs3) ->
              case BS.uncons bs3 of
              Just ('\n',bs4)  | q ->
                   doStringFieldContent q d (incTextRow end) begin
                                     (fieldBs `BS.append` BS.singleton '\n') bs4
              _ -> doStringFieldContent q d end begin
                                     (fieldBs `BS.append` BS.singleton '\r') bs3
          Just ('\n',bs3) | q ->
                   doStringFieldContent q d (incTextRow end) begin
                                     (fieldBs `BS.append` BS.singleton '\n') bs3
          Just ('\n',bs3) ->
                   field:
                   mkError end begin "Found newline within quoted field":
                   getFields q d stateNL (here stateNL) bs3
          Just ('"', bs3) ->
              case BS.uncons bs3 of
              Just (c,bs4)
                   | c==d     -> field: getFields q d state' (here state') bs4
              Just ('\r',bs4) ->
                  case BS.uncons bs4 of
                  Just ('\n',bs5) ->
                       field: getFields q d stateNL (here stateNL) bs5
                       -- XXX This could be an error instead:
                  _ -> field: getFields q d stateNL (here stateNL) bs4
              Just ('\n',bs4) -> field: getFields q d stateNL (here stateNL) bs4
              Just ('"',bs4)  ->
                  doStringFieldContent q d (incTextCol 3 end) begin
                                      (fieldBs `BS.append` BS.singleton '"') bs4
              Just _  -> field:
                         mkError state' begin "End-quote not followed by comma":
                         getFields q d state' (here state') bs3
              Nothing -> field: getFields q d stateNL (here stateNL) bs3
          Just _  -> [mkError state' begin "XXX Can't happen (string field)"]
          Nothing -> field:
                     mkError state' begin "CSV data ends within a quoted string"
                     :[]
 where interestingCharInsideString '\r' = True
       interestingCharInsideString '\n' = True
       interestingCharInsideString '"'  = True
       interestingCharInsideString _    = False

mkField :: CSVState -> (Int, Int) -> ByteString -> Bool -> CSVField
mkField st begin bs q =   CSVField { csvRowNum       = tableRow st
                                   , csvColNum       = tableCol st
                                   , csvTextStart    = begin
                                   , csvTextEnd      = (textRow st,textCol st)
                                   , csvFieldContent = bs
                                   , csvFieldQuoted  = q }

mkError :: CSVState -> (Int, Int) -> String -> CSVField
mkError st begin e = CSVFieldError { csvRowNum     = tableRow st
                                   , csvColNum     = tableCol st
                                   , csvTextStart  = begin
                                   , csvTextEnd    = (textRow st,textCol st)
                                   , csvFieldError = e }


-- Some pretty-printing for structured CSV errors.
ppCSVError :: CSVError -> String
ppCSVError (err@IncorrectRow{}) =
        "\nRow "++show (csvRow err)++" has wrong number of fields."++
        "\n    Expected "++show (csvColsExpected err)++" but got "++
        show (csvColsActual err)++"."++
        "\n    The fields are:"++
        indent 8 (concatMap ppCSVField (csvFields err))
ppCSVError (err@BlankLine{}) =
        "\nRow "++show (csvRow err)++" is blank."++
        "\n    Expected "++show (csvColsExpected err)++" fields."
ppCSVError (err@FieldError{}) = ppCSVField (csvField err)
ppCSVError (err@DuplicateHeader{}) =
        "\nThere are two (or more) identical column headers: "++
        show (csvDuplicate err)++"."++
        "\n    Column number "++show (csvHeaderSerial err)
ppCSVError (NoData{})         =
        "\nNo usable data (after accounting for any other errors)."

-- | Pretty-printing for CSV fields, shows positional information in addition
--   to the textual content.
ppCSVField :: CSVField -> String
ppCSVField (f@CSVField{}) =
        "\n"++BS.unpack (quoted (csvFieldQuoted f) (csvFieldContent f))++
        "\nin row "++show (csvRowNum f)++" at column "++show (csvColNum f)++
        " (textually from "++show (csvTextStart f)++" to "++
        show (csvTextEnd f)++")"
ppCSVField (f@CSVFieldError{}) =
        "\n"++csvFieldError f++
        "\nin row "++show (csvRowNum f)++" at column "++show (csvColNum f)++
        " (textually from "++show (csvTextStart f)++" to "++
        show (csvTextEnd f)


-- | Output a table back to a lazily-constructed string.  There are lots of
--   possible design decisions one could take, e.g. to re-arrange columns
--   back into something resembling their original order, but here we just
--   take the given table without looking at Row and Field numbers etc.
ppCSVTable :: CSVTable -> ByteString
ppCSVTable = BS.unlines . map (BS.intercalate (BS.pack ",") . map ppField)
  where ppField f = quoted (csvFieldQuoted f) (csvFieldContent f)

-- | Output a table back to a lazily-constructed bytestring, using the given
--   delimiter char.  The Boolean argument is to repair fields containing
--   newlines, by replacing the nl with a space.
ppDSVTable :: Bool -> Char -> CSVTable -> ByteString
ppDSVTable nl d = BS.unlines . map (BS.intercalate (BS.pack [d]) . map ppField)
  where ppField f = quoted (csvFieldQuoted f) (doNL $ csvFieldContent f)
        doNL | nl        = replaceNL
             | otherwise = id

{-
-- | Output a table back to a string, but using Haskell list-of-tuple notation
--   rather than CSV.
ppCSVTableAsTuples :: CSVTable -> String
ppCSVTableAsTuples = indent 4 . unlines . map ( (", ("++) . (++")")
                                              . intercalate ", " . map ppField )
  where ppField f = quoted (csvFieldQuoted f) (BS.unpack (csvFieldContent f))
-}

-- Some pp helpers - indent and quoted - should live elsewhere, in a
-- pretty-printing package.

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

quoted :: Bool -> ByteString -> ByteString
quoted False  s  = s
quoted True   s  = BS.concat [BS.pack "\"", escape s, BS.pack"\""]
  where escape s = let (good,next) = BS.span (/='"') s
                   in if BS.null next then good
                    else BS.concat [ good, BS.pack "\"\"", escape (BS.tail next) ]

replaceNL :: ByteString -> ByteString
replaceNL s = let (good,next) = BS.span (/='\n') s
              in if BS.null next then good
                 else if BS.null good then replaceNL (BS.tail next)
                 else BS.concat [ good, BS.pack " ", replaceNL next ]


-- | Convert a CSV table to a simpler representation, by dropping all
--   the original location information.
fromCSVTable :: CSVTable -> [[ByteString]]
fromCSVTable = map (map csvFieldContent)

-- | Convert a simple list of lists into a CSVTable by the addition of
--   logical locations.  (Textual locations are not so useful.)
--   Rows of varying lengths generate errors.  Fields that need
--   quotation marks are automatically marked as such.
toCSVTable   :: [[ByteString]] -> ([CSVError], CSVTable)
toCSVTable []         = ([NoData], [])
toCSVTable rows@(r:_) = (\ (a,b)-> (concat a, b)) $
                        unzip (zipWith walk [1..] rows)
  where
    n            = length r
    walk        :: Int -> [ByteString] -> ([CSVError], CSVRow)
    walk rnum [] = ( [blank rnum]
                   , map (\c-> mkCSVField rnum c (BS.empty)) [1..n])
    walk rnum cs = ( if length cs /= n then [bad rnum cs] else []
                   , zipWith (mkCSVField rnum) [1..n] cs )

    blank rnum =  BlankLine{ csvRow          = rnum
                           , csvColsExpected = n
                           , csvColsActual   = 0
                           , csvField        = mkCSVField rnum 0 BS.empty
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
    header         = map (BS.unpack . csvFieldContent) (head table)
    missing        = filter (`notElem` header) names
    reordering     = map (fromJust . (\n-> elemIndex n header)) names
    select fields  = map (fields!!) reordering

-- | Validate that the columns of a table have exactly the names and
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
    header         = map (BS.unpack . csvFieldContent) (head table)
    missing        = filter (`notElem` header) names

-- | A join operator, adds the columns of two tables together.
--   Precondition: the tables have the same number of rows.
joinCSV :: CSVTable -> CSVTable -> CSVTable
joinCSV = zipWith (++)

-- | A generator for a new CSV column, of arbitrary length.
--   The result can be joined to an existing table if desired.
mkEmptyColumn :: String -> CSVTable
mkEmptyColumn header = [headField] : map ((:[]).emptyField) [2..]
  where
    headField = (emptyField 1) { csvFieldContent = BS.pack header
                               , csvFieldQuoted  = True }
    emptyField n = CSVField { csvRowNum       = n
                            , csvColNum       = 0
                            , csvTextStart    = (0,0)
                            , csvTextEnd      = (0,0)
                            , csvFieldContent = BS.empty
                            , csvFieldQuoted  = False
                            }

-- | Generate a fresh field with the given textual content.
--   The quoting flag is set automatically based on the text.
--   Textual extents are not particularly useful, since there was no original
--   input to refer to.
mkCSVField :: Int -> Int -> ByteString -> CSVField
mkCSVField n c text =
        CSVField { csvRowNum       = n
                 , csvColNum       = c
                 , csvTextStart    = (0,0)
                 , csvTextEnd      = ( fromIntegral
                                             . BS.length 
                                             . BS.filter (=='\n')
                                             $ text
                                     , fromIntegral
                                             . BS.length
                                             . BS.takeWhile (/='\n')
                                             . BS.reverse $ text )
                 , csvFieldContent = text
                 , csvFieldQuoted  = any (`elem`"\",\n\r") (BS.unpack text)
                 }

