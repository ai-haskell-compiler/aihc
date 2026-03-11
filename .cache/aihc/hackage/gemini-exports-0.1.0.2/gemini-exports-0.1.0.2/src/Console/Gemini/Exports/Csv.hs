{-# LANGUAGE RecordWildCards #-}
{-| Types & functions for converting Gemini API responses into CSV exports.
-}
module Console.Gemini.Exports.Csv
    ( ExportData(..)
    , makeExportData
    , makeExportCsv
    , ExportLine(..)
    , getExportLineTimestamp
    ) where
import           Control.Applicative            ( (<|>) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Csv                       ( (.=)
                                                , DefaultOrdered(..)
                                                , ToNamedRecord(..)
                                                , defaultEncodeOptions
                                                , encUseCrLf
                                                , encodeDefaultOrderedByNameWith
                                                , header
                                                , namedRecord
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Scientific                ( FPFormat(Fixed)
                                                , Scientific
                                                , formatScientific
                                                )
import           Data.Text                      ( Text
                                                , empty
                                                , pack
                                                )
import           Data.Time                      ( TimeZone
                                                , defaultTimeLocale
                                                , formatTime
                                                , getTimeZone
                                                , utcToZonedTime
                                                )
import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , posixSecondsToUTCTime
                                                )

import           Web.Gemini

import qualified Data.ByteString.Lazy.Char8    as LBS


-- | The data required for rendering a single CSV row.
data ExportData = ExportData
    { edTZ   :: TimeZone
    , edLine :: ExportLine
    }
    deriving (Show, Read, Eq, Ord)

instance DefaultOrdered ExportData where
    headerOrder _ = header
        [ "time"
        , "base-asset"
        , "quote-asset"
        , "type"
        , "description"
        , "price"
        , "quantity"
        , "total"
        , "fee"
        , "fee-currency"
        , "trade-id"
        ]

instance ToNamedRecord ExportData where
    toNamedRecord (ExportData tz lineData) = namedRecord $ case lineData of
        TradeExport Trade {..} SymbolDetails {..} ->
            [ "time" .= formatTimestamp tTimestamp
            , "base-asset" .= sdBaseCurrency
            , "quote-asset" .= sdQuoteCurrency
            , "type" .= if tIsBuy then "Buy" else ("Sell" :: Text)
            , "description" .= empty
            , "price" .= formatDecimal tPrice
            , "quantity" .= formatDecimal tAmount
            , "total" .= formatDecimal (tPrice * tAmount)
            , "fee" .= formatDecimal tFeeAmount
            , "fee-currency" .= tFeeCurrency
            , "trade-id" .= tId
            ]
        TransferExport Transfer {..} ->
            [ "time" .= formatTimestamp trTimestamp
            , "base-asset" .= trCurrency
            , "quote-asset" .= empty
            , "type" .= trType
            , "description" .= toDescr (trMethod, trPurpose)
            , "price" .= empty
            , "quantity" .= formatDecimal trAmount
            , "total" .= formatDecimal trAmount
            , "fee" .= empty
            , "fee-currency" .= empty
            , "trade-id" .= trId
            ]
        EarnExport EarnTransaction {..} ->
            [ "time" .= formatTimestamp etTimestamp
            , "base-asset" .= etAmountCurrency
            , "quote-asset" .= fromMaybe empty etPriceCurrency
            , "type" .= ("Earn " <> etType)
            , "description" .= empty
            , "price" .= maybe empty formatDecimal etPrice
            , "quantity" .= formatDecimal etAmount
            , "total" .= maybe empty (formatDecimal . (* etAmount)) etPrice
            , "fee" .= empty
            , "fee-currency" .= empty
            , "trade-id" .= etId
            ]
      where
        -- Render a transfer description by combining the optional method
        -- & purpose fields.
        toDescr :: (Maybe Text, Maybe Text) -> Text
        toDescr = \case
            (Just m, Just p) -> m <> " " <> p
            (m     , p     ) -> fromMaybe "" $ m <|> p
        -- Convert a timestamp into a localtime with the line's timezone
        -- & render it in `YYYY-MM-DD HH:MM:SS.nnnnnnnnn` format.`
        formatTimestamp :: POSIXTime -> String
        formatTimestamp =
            formatTime defaultTimeLocale "%F %T%Q"
                . utcToZonedTime tz
                . posixSecondsToUTCTime
        -- Render a decimal number with the minimum precision neccesary..
        formatDecimal :: Scientific -> Text
        formatDecimal = pack . formatScientific Fixed Nothing

-- | Determine the 'TimeZone' for the 'ExportLine' & return both as an
-- 'ExportData'.
makeExportData :: MonadIO m => ExportLine -> m ExportData
makeExportData lineData = do
    tz <- liftIO . getTimeZone . posixSecondsToUTCTime $ getExportLineTimestamp
        lineData
    return $ ExportData tz lineData

-- | Render the export data as a CSV with a header row.
makeExportCsv :: [ExportData] -> LBS.ByteString
makeExportCsv =
    encodeDefaultOrderedByNameWith (defaultEncodeOptions { encUseCrLf = False })


-- | Split out the data required for different export line types.
data ExportLine
    = TradeExport Trade SymbolDetails
    | TransferExport Transfer
    | EarnExport EarnTransaction
    deriving (Show, Read, Eq, Ord)

-- | Get the timestamp field of an 'ExportLine'.
getExportLineTimestamp :: ExportLine -> POSIXTime
getExportLineTimestamp = \case
    TradeExport t _  -> tTimestamp t
    TransferExport t -> trTimestamp t
    EarnExport     t -> etTimestamp t
