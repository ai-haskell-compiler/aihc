{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-| The "Data.MXStateCodes" module is used for enumerating, and
rendering the <https://en.wikipedia.org/wiki/ISO_3166-2:MX ISO 3166-2:MX>
codes for Mexican States.

It is meant to be qualified when imported:

@
import qualified Data.MXStateCodes as MXStates
@

-}
module Data.MXStateCodes
    ( Code(..)
    , all
    , toName
    , fromName
    )
where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( all )


{-| A Mexican State Code from
<https://en.wikipedia.org/wiki/ISO_3166-2:MX ISO 3166-2:MX>.
-}
data Code
    = AGU       -- ^ Aguascalientes
    | BCN       -- ^ Baja California
    | BCS       -- ^ Baja California Sur
    | CAM       -- ^ Campeche
    | CHH       -- ^ Chihuahua
    | CHP       -- ^ Chiapas
    | CMX       -- ^ Ciudad de México
    | COA       -- ^ Coahuila de Zaragoza
    | COL       -- ^ Colima
    | DUR       -- ^ Durango
    | GRO       -- ^ Guerrero
    | GUA       -- ^ Guanajuato
    | HID       -- ^ Hidalgo
    | JAL       -- ^ Jalisco
    | MEX       -- ^ México
    | MIC       -- ^ Michoacán de Ocampo
    | MOR       -- ^ Morelos
    | NAY       -- ^ Nayarit
    | NLE       -- ^ Nuevo León
    | OAX       -- ^ Oaxaca
    | PUE       -- ^ Puebla
    | QUE       -- ^ Querétaro
    | ROO       -- ^ Quintana Roo
    | SIN       -- ^ Sinaloa
    | SLP       -- ^ San Luis Potosí
    | SON       -- ^ Sonora
    | TAB       -- ^ Tabasco
    | TAM       -- ^ Tamaulipas
    | TLA       -- ^ Tlaxcala
    | VER       -- ^ Veracruz de Ignacio de la Llave
    | YUC       -- ^ Yucatán
    | ZAC       -- ^ Zacatecas
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

instance ToJSON Code
instance FromJSON Code


{-| A list of every State Code. -}
all :: [Code]
all = enumFrom minBound


{-| Render a `Code` to it's English name. -}
toName :: Code -> T.Text
toName c = case c of
    AGU -> "Aguascalientes"
    BCN -> "Baja California"
    BCS -> "Baja California Sur"
    CAM -> "Campeche"
    CHH -> "Chihuahua"
    CHP -> "Chiapas"
    CMX -> "Ciudad de México"
    COA -> "Coahuila de Zaragoza"
    COL -> "Colima"
    DUR -> "Durango"
    GRO -> "Guerrero"
    GUA -> "Guanajuato"
    HID -> "Hidalgo"
    JAL -> "Jalisco"
    MEX -> "México"
    MIC -> "Michoacán de Ocampo"
    MOR -> "Morelos"
    NAY -> "Nayarit"
    NLE -> "Nuevo León"
    OAX -> "Oaxaca"
    PUE -> "Puebla"
    QUE -> "Querétaro"
    ROO -> "Quintana Roo"
    SIN -> "Sinaloa"
    SLP -> "San Luis Potosí"
    SON -> "Sonora"
    TAB -> "Tabasco"
    TAM -> "Tamaulipas"
    TLA -> "Tlaxcala"
    VER -> "Veracruz de Ignacio de la Llave"
    YUC -> "Yucatán"
    ZAC -> "Zacatecas"


{-| Parse a `Code` from an English name. This is case-insensitive.
-}
fromName :: T.Text -> Maybe Code
fromName n = case T.toLower n of
    "ciudad de méxico"     -> Just CMX
    "aguascalientes"       -> Just AGU
    "baja california"      -> Just BCN
    "baja california sur"  -> Just BCS
    "campeche"             -> Just CAM
    "coahuila de zaragoza" -> Just COA
    "colima"               -> Just COL
    "chiapas"              -> Just CHP
    "chihuahua"            -> Just CHH
    "durango"              -> Just DUR
    "guanajuato"           -> Just GUA
    "guerrero"             -> Just GRO
    "hidalgo"              -> Just HID
    "jalisco"              -> Just JAL
    "méxico"               -> Just MEX
    "michoacán de ocampo"  -> Just MIC
    "morelos"              -> Just MOR
    "nayarit"              -> Just NAY
    "nuevo león"           -> Just NLE
    "oaxaca"               -> Just OAX
    "puebla"               -> Just PUE
    "querétaro"            -> Just QUE
    "quintana roo"         -> Just ROO
    "san luis potosí"      -> Just SLP
    "sinaloa"              -> Just SIN
    "sonora"               -> Just SON
    "tabasco"              -> Just TAB
    "tamaulipas"           -> Just TAM
    "tlaxcala"             -> Just TLA
    "veracruz de ignacio de la llave" -> Just VER
    "yucatán"              -> Just YUC
    "zacatecas"            -> Just ZAC
    _                      -> Nothing
