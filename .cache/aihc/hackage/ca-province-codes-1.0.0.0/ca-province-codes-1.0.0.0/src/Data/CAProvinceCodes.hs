{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-| The "Data.CAProvinceCodes" module is used for enumerating, and
rendering the <https://en.wikipedia.org/wiki/ISO_3166-2:CA ISO 3166-2:CA>
codes for Canadian Provinces and Territories.

It is meant to be qualified when imported:

@
import qualified Data.CAProvinceCodes as CAProvinces
@

-}
module Data.CAProvinceCodes
    ( Code(..)
    , all
    , toName
    , fromName
    , isProvince
    , isTerritory
    )
where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( all )


{-| A Canadian Province/Territory Code from
<https://en.wikipedia.org/wiki/ISO_3166-2:CA ISO 3166-2:CA>.
-}
data Code
    = AB -- ^ Alberta
    | BC -- ^ British Columbia
    | MB -- ^ Manitoba
    | NB -- ^ New Brunswick
    | NL -- ^ Newfoundland And Labrador
    | NS -- ^ Nova Scotia
    | NT -- ^ Northwest Territories
    | NU -- ^ Nunavut
    | ON -- ^ Ontario
    | PE -- ^ Prince Edward Island
    | QC -- ^ Quebec
    | SK -- ^ Saskatchewan
    | YT -- ^ Yukon
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

instance ToJSON Code
instance FromJSON Code


{-| A list of every Province/Territory Code. -}
all :: [Code]
all = enumFrom minBound


{-| Render a `Code` to it's English name. -}
toName :: Code -> T.Text
toName c = case c of
    AB -> "Alberta"
    BC -> "British Columbia"
    MB -> "Manitoba"
    NB -> "New Brunswick"
    NL -> "Newfoundland And Labrador"
    NS -> "Nova Scotia"
    NT -> "Northwest Territories"
    NU -> "Nunavut"
    ON -> "Ontario"
    PE -> "Prince Edward Island"
    QC -> "Quebec"
    SK -> "Saskatchewan"
    YT -> "Yukon"


{-| Parse a `Code` from an English name. This is case-insensitive.
-}
fromName :: T.Text -> Maybe Code
fromName n = case T.toLower n of
    "alberta"                   -> Just AB
    "british columbia"          -> Just BC
    "manitoba"                  -> Just MB
    "new brunswick"             -> Just NB
    "newfoundland and labrador" -> Just NL
    "nova scotia"               -> Just NS
    "northwest territories"     -> Just NT
    "nunavut"                   -> Just NU
    "ontario"                   -> Just ON
    "prince edward island"      -> Just PE
    "quebec"                    -> Just QC
    "saskatchewan"              -> Just SK
    "yukon"                     -> Just YT
    _                           -> Nothing

{-| Does the `Code` denote a Province? -}
isProvince :: Code -> Bool
isProvince = not . isTerritory

{-| Does the `Code` denote a Territory? -}
isTerritory :: Code -> Bool
isTerritory = (`elem` [NT, NU, YT])
