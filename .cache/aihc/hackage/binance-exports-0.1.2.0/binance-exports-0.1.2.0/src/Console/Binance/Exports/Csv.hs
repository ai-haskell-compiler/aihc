{-# LANGUAGE RecordWildCards #-}
{- | Types & CSV serialization for the exports.
-}
module Console.Binance.Exports.Csv
    ( TradeExportData(..)
    , buildTradeExport
    ) where
import           Data.Csv                       ( (.=)
                                                , DefaultOrdered(..)
                                                , ToNamedRecord(..)
                                                , defaultEncodeOptions
                                                , encUseCrLf
                                                , encodeDefaultOrderedByNameWith
                                                , header
                                                , namedRecord
                                                )
import           Data.Scientific                ( FPFormat(..)
                                                , Scientific
                                                , formatScientific
                                                , isInteger
                                                )
import           Data.Time                      ( defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )

import qualified Data.ByteString.Lazy          as LBS
import           Web.Binance                    ( SymbolDetails(..)
                                                , Trade(..)
                                                )


-- | We need both the 'SymbolDetails' & the 'Trade' to generate an export
-- line.
data TradeExportData = TradeExportData
    { tedSymbol :: SymbolDetails
    , tedTrade  :: Trade
    }
    deriving (Show, Read, Eq, Ord)

-- | We match the format of the old @Trade History@ export as much as
-- possible, but use the asset precisions for the @price@ & @quantity@
-- fields & output the @trade-id@ as well.
instance ToNamedRecord TradeExportData where
    toNamedRecord (TradeExportData SymbolDetails {..} Trade {..}) = namedRecord
        [ "time" .= formatTime defaultTimeLocale
                               "%F %T"
                               (posixSecondsToUTCTime tTime)
        , "base-asset" .= sdBaseAsset
        , "quote-asset" .= sdQuoteAsset
        , "type" .= if tIsBuyer then "BUY" else ("SELL" :: String)
        , "price" .= formatScientific Fixed (Just sdQuoteAssetPrecision) tPrice
        , "quantity"
            .= formatScientific Fixed (Just sdBaseAssetPrecision) tQuantity
        , "total" .= renderScientific (tPrice * tQuantity)
        , "fee" .= renderScientific tCommission
        , "fee-currency" .= tCommissionAsset
        , "trade-id" .= tId
        ]
      where
        -- | Render as an integer if possible, otherwise render to the
        -- minimum decimal precision needed.
        renderScientific :: Scientific -> String
        renderScientific p = if isInteger p
            then formatScientific Fixed (Just 0) p
            else formatScientific Fixed Nothing p

instance DefaultOrdered TradeExportData where
    headerOrder _ = header
        [ "time"
        , "base-asset"
        , "quote-asset"
        , "type"
        , "price"
        , "quantity"
        , "total"
        , "fee"
        , "fee-currency"
        , "trade-id"
        ]

-- | Generate a CSV from the trade data.
buildTradeExport :: [TradeExportData] -> LBS.ByteString
buildTradeExport =
    encodeDefaultOrderedByNameWith (defaultEncodeOptions { encUseCrLf = False })
