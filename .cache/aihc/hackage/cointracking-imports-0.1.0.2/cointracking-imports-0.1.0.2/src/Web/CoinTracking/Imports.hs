{-# LANGUAGE RecordWildCards #-}
{-| Generate CSV & XLSX files to use with CoinTracking's import feature.

-}
module Web.CoinTracking.Imports
    ( writeImportDataToFile
      -- * Types
    , module Web.CoinTracking.Imports.Types
      -- * CSV Import Files
    , coinTrackingCsvImport
    , headerRow
    , csvEncodingOptions
      -- * XLSX Import Files
    , coinTrackingXlsxImport
    , writeXlsxHeader
    , writeXlsxRow
    ) where

import           Codec.Xlsx                     ( CellValue(..)
                                                , DateBase(DateBase1900)
                                                , Worksheet
                                                , atSheet
                                                , cellValueAt
                                                , dateToNumber
                                                , def
                                                , fromXlsx
                                                )
import           Control.Lens                   ( (.~)
                                                , (?~)
                                                )
import           Data.Char                      ( toLower )
import           Data.Csv                       ( EncodeOptions(..)
                                                , defaultEncodeOptions
                                                , encodeWith
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.Function                  ( (&) )
import           Data.Scientific                ( toRealFloat )
import           Data.Time                      ( zonedTimeToUTC )
import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , getPOSIXTime
                                                )
import           System.FilePath                ( takeExtension )

import           Web.CoinTracking.Imports.Types

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LBC
import qualified Data.Text                     as T


-- | Write the given data to a file. If the file extension is @.xlsx@ or
-- @.xls@, we write a spreadsheet. Otherwise we write a CSV.
writeImportDataToFile :: FilePath -> [CTImportData] -> IO ()
writeImportDataToFile file xs = do
    currentTime <- getPOSIXTime
    let extension = takeExtension file
        output    = if map toLower extension `elem` [".xlsx", ".xls"]
            then coinTrackingXlsxImport currentTime xs
            else coinTrackingCsvImport xs
    LBC.writeFile file output


-- CSVs

-- | Generate the CoinTracking CSV Import for the data, prepended by
-- a header row.
--
-- Note: the resulting 'LBS.ByteString' has it's final newline removed, as
-- CoinTracking's Import creates a double entry with newlines at the end of
-- an import file.
coinTrackingCsvImport :: [CTImportData] -> LBS.ByteString
coinTrackingCsvImport =
    (headerRow <>) . LBC.init . encodeWith csvEncodingOptions

-- | The CSV header row to prepend to the generated output.
headerRow :: LBS.ByteString
headerRow = encodeWith
    csvEncodingOptions
    [ [ "Type" :: T.Text
      , "Buy"
      , "Cur."
      , "Sell"
      , "Cur."
      , "Fee"
      , "Cur."
      , "Exchange"
      , "Trade-Group"
      , "Comment"
      , "Date"
      , "Tx-ID"
      , "Buy Value in your Account Currency"
      , "Sell Value in your Account Currency"
      ]
    ]

-- | 'defaultEncodeOptions', but with newline-only line endings.
csvEncodingOptions :: EncodeOptions
csvEncodingOptions = defaultEncodeOptions { encUseCrLf = False }


-- XLSXs

-- | Generate an XLSX file containing the expected headers rows and the
-- import data.
coinTrackingXlsxImport
    :: POSIXTime
    -- ^ Creation time to embed in the spreadsheet.
    -> [CTImportData]
    -> LBS.ByteString
coinTrackingXlsxImport createdTime rows =
    let sheet = ixFoldl
            (\sheet_ rowNum row -> writeXlsxRow sheet_ (rowNum + 3) row)
            (writeXlsxHeader def)
            rows
        book = def & atSheet "Sheet1" ?~ sheet
    in  fromXlsx createdTime book
  where
    -- | Indexed fold from the left.
    ixFoldl :: (b -> RowIndex -> a -> b) -> b -> [a] -> b
    ixFoldl f initial =
        fst . foldl' (\(b, i) a -> (f b i a, i + 1)) (initial, 0)

-- | Write the standard CoinTracking header to the first two rows of the
-- worksheet.
writeXlsxHeader :: Worksheet -> Worksheet
writeXlsxHeader sheet =
    sheet
        &  cellValueAt (1, 1)
        ?~ CellText
               "CoinTracking Excel Import data (see docs: https://cointracking.info/import/import_xls/)"
        &  writeColumn 1  "Type"
        &  writeColumn 2  "Buy Amount"
        &  writeColumn 3  "Buy Cur."
        &  writeColumn 4  "Sell Amount"
        &  writeColumn 5  "Sell Cur."
        &  writeColumn 6  "Feel Amount"
        &  writeColumn 7  "Fee Cur."
        &  writeColumn 8  "Exchange"
        &  writeColumn 9  "Trade Group"
        &  writeColumn 10 "Comment"
        &  writeColumn 11 "Date"
  where
    writeColumn :: ColumnIndex -> T.Text -> Worksheet -> Worksheet
    writeColumn c t s = s & cellValueAt (2, c) ?~ CellText t


-- | Write a 'CTImportData' to the given row(1-indexed) of the worksheet.
writeXlsxRow :: Worksheet -> RowIndex -> CTImportData -> Worksheet
writeXlsxRow sheet row CTImportData {..} =
    sheet
        & (cellValueAt (row, 1) ?~ CellText (renderTransactionType ctidType))
        & (cellValueAt (row, 2) .~ renderAmount ctidBuy)
        & (cellValueAt (row, 3) .~ renderCurrency ctidBuy)
        & (cellValueAt (row, 4) .~ renderAmount ctidSell)
        & (cellValueAt (row, 5) .~ renderCurrency ctidSell)
        & (cellValueAt (row, 6) .~ renderAmount ctidFee)
        & (cellValueAt (row, 7) .~ renderCurrency ctidFee)
        & (cellValueAt (row, 8) ?~ CellText ctidExchange)
        & (cellValueAt (row, 9) ?~ CellText ctidGroup)
        & (cellValueAt (row, 10) ?~ CellText ctidComment)
        & (cellValueAt (row, 11) ?~ renderedDate)
  where
    renderAmount :: Maybe Amount -> Maybe CellValue
    renderAmount = fmap (CellDouble . toRealFloat . aAmount)
    renderCurrency :: Maybe Amount -> Maybe CellValue
    renderCurrency = fmap (CellText . cTicker . aCurrency)
    renderedDate :: CellValue
    renderedDate =
        CellDouble $ dateToNumber DateBase1900 $ zonedTimeToUTC ctidDate
