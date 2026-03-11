{- This is a machine generated module.
   Do not edit.
   See cabal file for repository url.
-}
{- | Two letter Country Codes

     Defines the type CountryCode with constructors for each
     of the two-letter codes defined in
     <http://www.iso.org/iso/list-en1-semic-3.txt> and
     instances for 'Eq', 'Read', 'Show', 'Enum', 'Bounded'
     and 'Ord'.

     Also defines @'countryNameFromCode'@, which gives the
     official short country name all in uppercase and
     @'readableCountryName'@, which produces somewhat more user-friendly output

     Intended to be imported qualified as some country codes
     are the same as some standard Haskell constructors.

-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DeriveGeneric #-}
#endif
module Data.ISO3166_CountryCodes
    (CountryCode(..),
     countryNameFromCode,
     readableCountryName
    ) where
import qualified Prelude as P
import Prelude ((.),not,(==),otherwise,(&&),(==),(/=))
import Control.Monad
import Data.Char
import Data.List

#if __GLASGOW_HASKELL__ >= 701
import qualified GHC.Generics as G
#endif

{- | A human readable version of the official name of a country
     from its country code

     Uses some ad-hockery to rearrange the order of the words.

-}
readableCountryName :: CountryCode -> P.String

readableCountryName
    = concat . intersperse " " . rearrange . fmap up1 . words .
      fmap toUpper . countryNameFromCode
      where up1 [] = []
            up1 (c:rest) | not (isAlpha c) = c:up1 rest
            up1 "OF" = "of"
            up1 "THE" = "the"
            up1 "AND" = "and"
            up1 "U.S." = "US" -- gawd
            up1 ('M':'C':l) = "Mc"++up1 l -- Don't do MacEdonia!
                                          -- but there are no Mac_ countries yet
            up1 ('D':'\'':l) = "d'"++up1 l
            up1 (c:cs) = toUpper c: downup cs
            downup [] = [] -- needed for hyphenated names
            downup (c:cs) | c=='-' = c:up1 cs
                          | otherwise = toLower c:downup cs
            rearrange [] = []
            rearrange [c] = [c]
            rearrange ll@(n:l)
                | last l `elem` ["of", "the"] && last n == ','
                    = onhead up1 l++[[c|c<-n,c/=',']]
                | otherwise = ll
            onhead f [] = []
            onhead f (h:r) = f h:r
{- Copyright © 2010 Jón Fairbairn
-}
data CountryCode = 
   AF -- ^ Afghanistan
   |
   AX -- ^ Åland Islands
   |
   AL -- ^ Albania
   |
   DZ -- ^ Algeria
   |
   AS -- ^ American Samoa
   |
   AD -- ^ Andorra
   |
   AO -- ^ Angola
   |
   AI -- ^ Anguilla
   |
   AQ -- ^ Antarctica
   |
   AG -- ^ Antigua and Barbuda
   |
   AR -- ^ Argentina
   |
   AM -- ^ Armenia
   |
   AW -- ^ Aruba
   |
   AU -- ^ Australia
   |
   AT -- ^ Austria
   |
   AZ -- ^ Azerbaijan
   |
   BS -- ^ Bahamas
   |
   BH -- ^ Bahrain
   |
   BD -- ^ Bangladesh
   |
   BB -- ^ Barbados
   |
   BY -- ^ Belarus
   |
   BE -- ^ Belgium
   |
   BZ -- ^ Belize
   |
   BJ -- ^ Benin
   |
   BM -- ^ Bermuda
   |
   BT -- ^ Bhutan
   |
   BO -- ^ Bolivia, Plurinational State of
   |
   BQ -- ^ Bonaire, Sint Eustatius and Saba
   |
   BA -- ^ Bosnia and Herzegovina
   |
   BW -- ^ Botswana
   |
   BV -- ^ Bouvet Island
   |
   BR -- ^ Brazil
   |
   IO -- ^ British Indian Ocean Territory
   |
   BN -- ^ Brunei Darussalam
   |
   BG -- ^ Bulgaria
   |
   BF -- ^ Burkina Faso
   |
   BI -- ^ Burundi
   |
   KH -- ^ Cambodia
   |
   CM -- ^ Cameroon
   |
   CA -- ^ Canada
   |
   CV -- ^ Cape Verde
   |
   KY -- ^ Cayman Islands
   |
   CF -- ^ Central African Republic
   |
   TD -- ^ Chad
   |
   CL -- ^ Chile
   |
   CN -- ^ China
   |
   CX -- ^ Christmas Island
   |
   CC -- ^ Cocos (Keeling) Islands
   |
   CO -- ^ Colombia
   |
   KM -- ^ Comoros
   |
   CG -- ^ Congo
   |
   CD -- ^ Congo, the Democratic Republic of the
   |
   CK -- ^ Cook Islands
   |
   CR -- ^ Costa Rica
   |
   CI -- ^ Côte d'Ivoire
   |
   HR -- ^ Croatia
   |
   CU -- ^ Cuba
   |
   CW -- ^ Curaçao
   |
   CY -- ^ Cyprus
   |
   CZ -- ^ Czech Republic
   |
   DK -- ^ Denmark
   |
   DJ -- ^ Djibouti
   |
   DM -- ^ Dominica
   |
   DO -- ^ Dominican Republic
   |
   EC -- ^ Ecuador
   |
   EG -- ^ Egypt
   |
   SV -- ^ El Salvador
   |
   GQ -- ^ Equatorial Guinea
   |
   ER -- ^ Eritrea
   |
   EE -- ^ Estonia
   |
   ET -- ^ Ethiopia
   |
   FK -- ^ Falkland Islands (Malvinas)
   |
   FO -- ^ Faroe Islands
   |
   FJ -- ^ Fiji
   |
   FI -- ^ Finland
   |
   FR -- ^ France
   |
   GF -- ^ French Guiana
   |
   PF -- ^ French Polynesia
   |
   TF -- ^ French Southern Territories
   |
   GA -- ^ Gabon
   |
   GM -- ^ Gambia
   |
   GE -- ^ Georgia
   |
   DE -- ^ Germany
   |
   GH -- ^ Ghana
   |
   GI -- ^ Gibraltar
   |
   GR -- ^ Greece
   |
   GL -- ^ Greenland
   |
   GD -- ^ Grenada
   |
   GP -- ^ Guadeloupe
   |
   GU -- ^ Guam
   |
   GT -- ^ Guatemala
   |
   GG -- ^ Guernsey
   |
   GN -- ^ Guinea
   |
   GW -- ^ Guinea-Bissau
   |
   GY -- ^ Guyana
   |
   HT -- ^ Haiti
   |
   HM -- ^ Heard Island and McDonald Islands
   |
   VA -- ^ Holy See (Vatican City State)
   |
   HN -- ^ Honduras
   |
   HK -- ^ Hong Kong
   |
   HU -- ^ Hungary
   |
   IS -- ^ Iceland
   |
   IN -- ^ India
   |
   ID -- ^ Indonesia
   |
   IR -- ^ Iran, Islamic Republic of
   |
   IQ -- ^ Iraq
   |
   IE -- ^ Ireland
   |
   IM -- ^ Isle of Man
   |
   IL -- ^ Israel
   |
   IT -- ^ Italy
   |
   JM -- ^ Jamaica
   |
   JP -- ^ Japan
   |
   JE -- ^ Jersey
   |
   JO -- ^ Jordan
   |
   KZ -- ^ Kazakhstan
   |
   KE -- ^ Kenya
   |
   KI -- ^ Kiribati
   |
   KP -- ^ Korea, Democratic People's Republic of
   |
   KR -- ^ Korea, Republic of
   |
   KW -- ^ Kuwait
   |
   KG -- ^ Kyrgyzstan
   |
   LA -- ^ Lao People's Democratic Republic
   |
   LV -- ^ Latvia
   |
   LB -- ^ Lebanon
   |
   LS -- ^ Lesotho
   |
   LR -- ^ Liberia
   |
   LY -- ^ Libya
   |
   LI -- ^ Liechtenstein
   |
   LT -- ^ Lithuania
   |
   LU -- ^ Luxembourg
   |
   MO -- ^ Macao
   |
   MK -- ^ Macedonia, the Former Yugoslav Republic of
   |
   MG -- ^ Madagascar
   |
   MW -- ^ Malawi
   |
   MY -- ^ Malaysia
   |
   MV -- ^ Maldives
   |
   ML -- ^ Mali
   |
   MT -- ^ Malta
   |
   MH -- ^ Marshall Islands
   |
   MQ -- ^ Martinique
   |
   MR -- ^ Mauritania
   |
   MU -- ^ Mauritius
   |
   YT -- ^ Mayotte
   |
   MX -- ^ Mexico
   |
   FM -- ^ Micronesia, Federated States of
   |
   MD -- ^ Moldova, Republic of
   |
   MC -- ^ Monaco
   |
   MN -- ^ Mongolia
   |
   ME -- ^ Montenegro
   |
   MS -- ^ Montserrat
   |
   MA -- ^ Morocco
   |
   MZ -- ^ Mozambique
   |
   MM -- ^ Myanmar
   |
   NA -- ^ Namibia
   |
   NR -- ^ Nauru
   |
   NP -- ^ Nepal
   |
   NL -- ^ Netherlands
   |
   NC -- ^ New Caledonia
   |
   NZ -- ^ New Zealand
   |
   NI -- ^ Nicaragua
   |
   NE -- ^ Niger
   |
   NG -- ^ Nigeria
   |
   NU -- ^ Niue
   |
   NF -- ^ Norfolk Island
   |
   MP -- ^ Northern Mariana Islands
   |
   NO -- ^ Norway
   |
   OM -- ^ Oman
   |
   PK -- ^ Pakistan
   |
   PW -- ^ Palau
   |
   PS -- ^ Palestine, State of
   |
   PA -- ^ Panama
   |
   PG -- ^ Papua New Guinea
   |
   PY -- ^ Paraguay
   |
   PE -- ^ Peru
   |
   PH -- ^ Philippines
   |
   PN -- ^ Pitcairn
   |
   PL -- ^ Poland
   |
   PT -- ^ Portugal
   |
   PR -- ^ Puerto Rico
   |
   QA -- ^ Qatar
   |
   RE -- ^ Réunion
   |
   RO -- ^ Romania
   |
   RU -- ^ Russian Federation
   |
   RW -- ^ Rwanda
   |
   BL -- ^ Saint Barthélemy
   |
   SH -- ^ Saint Helena, Ascension and Tristan da Cunha
   |
   KN -- ^ Saint Kitts and Nevis
   |
   LC -- ^ Saint Lucia
   |
   MF -- ^ Saint Martin (French part)
   |
   PM -- ^ Saint Pierre and Miquelon
   |
   VC -- ^ Saint Vincent and the Grenadines
   |
   WS -- ^ Samoa
   |
   SM -- ^ San Marino
   |
   ST -- ^ Sao Tome and Principe
   |
   SA -- ^ Saudi Arabia
   |
   SN -- ^ Senegal
   |
   RS -- ^ Serbia
   |
   SC -- ^ Seychelles
   |
   SL -- ^ Sierra Leone
   |
   SG -- ^ Singapore
   |
   SX -- ^ Sint Maarten (Dutch part)
   |
   SK -- ^ Slovakia
   |
   SI -- ^ Slovenia
   |
   SB -- ^ Solomon Islands
   |
   SO -- ^ Somalia
   |
   ZA -- ^ South Africa
   |
   GS -- ^ South Georgia and the South Sandwich Islands
   |
   SS -- ^ South Sudan
   |
   ES -- ^ Spain
   |
   LK -- ^ Sri Lanka
   |
   SD -- ^ Sudan
   |
   SR -- ^ Suriname
   |
   SJ -- ^ Svalbard and Jan Mayen
   |
   SZ -- ^ Swaziland
   |
   SE -- ^ Sweden
   |
   CH -- ^ Switzerland
   |
   SY -- ^ Syrian Arab Republic
   |
   TW -- ^ Taiwan, Province of China
   |
   TJ -- ^ Tajikistan
   |
   TZ -- ^ Tanzania, United Republic of
   |
   TH -- ^ Thailand
   |
   TL -- ^ Timor-Leste
   |
   TG -- ^ Togo
   |
   TK -- ^ Tokelau
   |
   TO -- ^ Tonga
   |
   TT -- ^ Trinidad and Tobago
   |
   TN -- ^ Tunisia
   |
   TR -- ^ Turkey
   |
   TM -- ^ Turkmenistan
   |
   TC -- ^ Turks and Caicos Islands
   |
   TV -- ^ Tuvalu
   |
   UG -- ^ Uganda
   |
   UA -- ^ Ukraine
   |
   AE -- ^ United Arab Emirates
   |
   GB -- ^ United Kingdom
   |
   US -- ^ United States
   |
   UM -- ^ United States Minor Outlying Islands
   |
   UY -- ^ Uruguay
   |
   UZ -- ^ Uzbekistan
   |
   VU -- ^ Vanuatu
   |
   VE -- ^ Venezuela, Bolivarian Republic of
   |
   VN -- ^ Viet Nam
   |
   VG -- ^ Virgin Islands, British
   |
   VI -- ^ Virgin Islands, U.S.
   |
   WF -- ^ Wallis and Futuna
   |
   EH -- ^ Western Sahara
   |
   YE -- ^ Yemen
   |
   ZM -- ^ Zambia
   |
   ZW -- ^ Zimbabwe
#if __GLASGOW_HASKELL__ >= 701
   deriving (P.Eq,P.Read,P.Show,P.Enum,P.Bounded,P.Ord,G.Generic)
#else
   deriving (P.Eq,P.Read,P.Show,P.Enum,P.Bounded,P.Ord)
#endif

{-|
  convert a country code to the official (English) name of the country

   see @'readableCountryName'@ for something with a more pleasing word order and capitalisation
-}
countryNameFromCode:: CountryCode -> P.String
countryNameFromCode AF = "Afghanistan"
countryNameFromCode AX = "Åland Islands"
countryNameFromCode AL = "Albania"
countryNameFromCode DZ = "Algeria"
countryNameFromCode AS = "American Samoa"
countryNameFromCode AD = "Andorra"
countryNameFromCode AO = "Angola"
countryNameFromCode AI = "Anguilla"
countryNameFromCode AQ = "Antarctica"
countryNameFromCode AG = "Antigua and Barbuda"
countryNameFromCode AR = "Argentina"
countryNameFromCode AM = "Armenia"
countryNameFromCode AW = "Aruba"
countryNameFromCode AU = "Australia"
countryNameFromCode AT = "Austria"
countryNameFromCode AZ = "Azerbaijan"
countryNameFromCode BS = "Bahamas"
countryNameFromCode BH = "Bahrain"
countryNameFromCode BD = "Bangladesh"
countryNameFromCode BB = "Barbados"
countryNameFromCode BY = "Belarus"
countryNameFromCode BE = "Belgium"
countryNameFromCode BZ = "Belize"
countryNameFromCode BJ = "Benin"
countryNameFromCode BM = "Bermuda"
countryNameFromCode BT = "Bhutan"
countryNameFromCode BO = "Bolivia, Plurinational State of"
countryNameFromCode BQ = "Bonaire, Sint Eustatius and Saba"
countryNameFromCode BA = "Bosnia and Herzegovina"
countryNameFromCode BW = "Botswana"
countryNameFromCode BV = "Bouvet Island"
countryNameFromCode BR = "Brazil"
countryNameFromCode IO = "British Indian Ocean Territory"
countryNameFromCode BN = "Brunei Darussalam"
countryNameFromCode BG = "Bulgaria"
countryNameFromCode BF = "Burkina Faso"
countryNameFromCode BI = "Burundi"
countryNameFromCode KH = "Cambodia"
countryNameFromCode CM = "Cameroon"
countryNameFromCode CA = "Canada"
countryNameFromCode CV = "Cape Verde"
countryNameFromCode KY = "Cayman Islands"
countryNameFromCode CF = "Central African Republic"
countryNameFromCode TD = "Chad"
countryNameFromCode CL = "Chile"
countryNameFromCode CN = "China"
countryNameFromCode CX = "Christmas Island"
countryNameFromCode CC = "Cocos (Keeling) Islands"
countryNameFromCode CO = "Colombia"
countryNameFromCode KM = "Comoros"
countryNameFromCode CG = "Congo"
countryNameFromCode CD = "Congo, the Democratic Republic of the"
countryNameFromCode CK = "Cook Islands"
countryNameFromCode CR = "Costa Rica"
countryNameFromCode CI = "Côte d'Ivoire"
countryNameFromCode HR = "Croatia"
countryNameFromCode CU = "Cuba"
countryNameFromCode CW = "Curaçao"
countryNameFromCode CY = "Cyprus"
countryNameFromCode CZ = "Czech Republic"
countryNameFromCode DK = "Denmark"
countryNameFromCode DJ = "Djibouti"
countryNameFromCode DM = "Dominica"
countryNameFromCode DO = "Dominican Republic"
countryNameFromCode EC = "Ecuador"
countryNameFromCode EG = "Egypt"
countryNameFromCode SV = "El Salvador"
countryNameFromCode GQ = "Equatorial Guinea"
countryNameFromCode ER = "Eritrea"
countryNameFromCode EE = "Estonia"
countryNameFromCode ET = "Ethiopia"
countryNameFromCode FK = "Falkland Islands (Malvinas)"
countryNameFromCode FO = "Faroe Islands"
countryNameFromCode FJ = "Fiji"
countryNameFromCode FI = "Finland"
countryNameFromCode FR = "France"
countryNameFromCode GF = "French Guiana"
countryNameFromCode PF = "French Polynesia"
countryNameFromCode TF = "French Southern Territories"
countryNameFromCode GA = "Gabon"
countryNameFromCode GM = "Gambia"
countryNameFromCode GE = "Georgia"
countryNameFromCode DE = "Germany"
countryNameFromCode GH = "Ghana"
countryNameFromCode GI = "Gibraltar"
countryNameFromCode GR = "Greece"
countryNameFromCode GL = "Greenland"
countryNameFromCode GD = "Grenada"
countryNameFromCode GP = "Guadeloupe"
countryNameFromCode GU = "Guam"
countryNameFromCode GT = "Guatemala"
countryNameFromCode GG = "Guernsey"
countryNameFromCode GN = "Guinea"
countryNameFromCode GW = "Guinea-Bissau"
countryNameFromCode GY = "Guyana"
countryNameFromCode HT = "Haiti"
countryNameFromCode HM = "Heard Island and McDonald Islands"
countryNameFromCode VA = "Holy See (Vatican City State)"
countryNameFromCode HN = "Honduras"
countryNameFromCode HK = "Hong Kong"
countryNameFromCode HU = "Hungary"
countryNameFromCode IS = "Iceland"
countryNameFromCode IN = "India"
countryNameFromCode ID = "Indonesia"
countryNameFromCode IR = "Iran, Islamic Republic of"
countryNameFromCode IQ = "Iraq"
countryNameFromCode IE = "Ireland"
countryNameFromCode IM = "Isle of Man"
countryNameFromCode IL = "Israel"
countryNameFromCode IT = "Italy"
countryNameFromCode JM = "Jamaica"
countryNameFromCode JP = "Japan"
countryNameFromCode JE = "Jersey"
countryNameFromCode JO = "Jordan"
countryNameFromCode KZ = "Kazakhstan"
countryNameFromCode KE = "Kenya"
countryNameFromCode KI = "Kiribati"
countryNameFromCode KP = "Korea, Democratic People's Republic of"
countryNameFromCode KR = "Korea, Republic of"
countryNameFromCode KW = "Kuwait"
countryNameFromCode KG = "Kyrgyzstan"
countryNameFromCode LA = "Lao People's Democratic Republic"
countryNameFromCode LV = "Latvia"
countryNameFromCode LB = "Lebanon"
countryNameFromCode LS = "Lesotho"
countryNameFromCode LR = "Liberia"
countryNameFromCode LY = "Libya"
countryNameFromCode LI = "Liechtenstein"
countryNameFromCode LT = "Lithuania"
countryNameFromCode LU = "Luxembourg"
countryNameFromCode MO = "Macao"
countryNameFromCode MK = "Macedonia, the Former Yugoslav Republic of"
countryNameFromCode MG = "Madagascar"
countryNameFromCode MW = "Malawi"
countryNameFromCode MY = "Malaysia"
countryNameFromCode MV = "Maldives"
countryNameFromCode ML = "Mali"
countryNameFromCode MT = "Malta"
countryNameFromCode MH = "Marshall Islands"
countryNameFromCode MQ = "Martinique"
countryNameFromCode MR = "Mauritania"
countryNameFromCode MU = "Mauritius"
countryNameFromCode YT = "Mayotte"
countryNameFromCode MX = "Mexico"
countryNameFromCode FM = "Micronesia, Federated States of"
countryNameFromCode MD = "Moldova, Republic of"
countryNameFromCode MC = "Monaco"
countryNameFromCode MN = "Mongolia"
countryNameFromCode ME = "Montenegro"
countryNameFromCode MS = "Montserrat"
countryNameFromCode MA = "Morocco"
countryNameFromCode MZ = "Mozambique"
countryNameFromCode MM = "Myanmar"
countryNameFromCode NA = "Namibia"
countryNameFromCode NR = "Nauru"
countryNameFromCode NP = "Nepal"
countryNameFromCode NL = "Netherlands"
countryNameFromCode NC = "New Caledonia"
countryNameFromCode NZ = "New Zealand"
countryNameFromCode NI = "Nicaragua"
countryNameFromCode NE = "Niger"
countryNameFromCode NG = "Nigeria"
countryNameFromCode NU = "Niue"
countryNameFromCode NF = "Norfolk Island"
countryNameFromCode MP = "Northern Mariana Islands"
countryNameFromCode NO = "Norway"
countryNameFromCode OM = "Oman"
countryNameFromCode PK = "Pakistan"
countryNameFromCode PW = "Palau"
countryNameFromCode PS = "Palestine, State of"
countryNameFromCode PA = "Panama"
countryNameFromCode PG = "Papua New Guinea"
countryNameFromCode PY = "Paraguay"
countryNameFromCode PE = "Peru"
countryNameFromCode PH = "Philippines"
countryNameFromCode PN = "Pitcairn"
countryNameFromCode PL = "Poland"
countryNameFromCode PT = "Portugal"
countryNameFromCode PR = "Puerto Rico"
countryNameFromCode QA = "Qatar"
countryNameFromCode RE = "Réunion"
countryNameFromCode RO = "Romania"
countryNameFromCode RU = "Russian Federation"
countryNameFromCode RW = "Rwanda"
countryNameFromCode BL = "Saint Barthélemy"
countryNameFromCode SH = "Saint Helena, Ascension and Tristan da Cunha"
countryNameFromCode KN = "Saint Kitts and Nevis"
countryNameFromCode LC = "Saint Lucia"
countryNameFromCode MF = "Saint Martin (French part)"
countryNameFromCode PM = "Saint Pierre and Miquelon"
countryNameFromCode VC = "Saint Vincent and the Grenadines"
countryNameFromCode WS = "Samoa"
countryNameFromCode SM = "San Marino"
countryNameFromCode ST = "Sao Tome and Principe"
countryNameFromCode SA = "Saudi Arabia"
countryNameFromCode SN = "Senegal"
countryNameFromCode RS = "Serbia"
countryNameFromCode SC = "Seychelles"
countryNameFromCode SL = "Sierra Leone"
countryNameFromCode SG = "Singapore"
countryNameFromCode SX = "Sint Maarten (Dutch part)"
countryNameFromCode SK = "Slovakia"
countryNameFromCode SI = "Slovenia"
countryNameFromCode SB = "Solomon Islands"
countryNameFromCode SO = "Somalia"
countryNameFromCode ZA = "South Africa"
countryNameFromCode GS = "South Georgia and the South Sandwich Islands"
countryNameFromCode SS = "South Sudan"
countryNameFromCode ES = "Spain"
countryNameFromCode LK = "Sri Lanka"
countryNameFromCode SD = "Sudan"
countryNameFromCode SR = "Suriname"
countryNameFromCode SJ = "Svalbard and Jan Mayen"
countryNameFromCode SZ = "Swaziland"
countryNameFromCode SE = "Sweden"
countryNameFromCode CH = "Switzerland"
countryNameFromCode SY = "Syrian Arab Republic"
countryNameFromCode TW = "Taiwan, Province of China"
countryNameFromCode TJ = "Tajikistan"
countryNameFromCode TZ = "Tanzania, United Republic of"
countryNameFromCode TH = "Thailand"
countryNameFromCode TL = "Timor-Leste"
countryNameFromCode TG = "Togo"
countryNameFromCode TK = "Tokelau"
countryNameFromCode TO = "Tonga"
countryNameFromCode TT = "Trinidad and Tobago"
countryNameFromCode TN = "Tunisia"
countryNameFromCode TR = "Turkey"
countryNameFromCode TM = "Turkmenistan"
countryNameFromCode TC = "Turks and Caicos Islands"
countryNameFromCode TV = "Tuvalu"
countryNameFromCode UG = "Uganda"
countryNameFromCode UA = "Ukraine"
countryNameFromCode AE = "United Arab Emirates"
countryNameFromCode GB = "United Kingdom"
countryNameFromCode US = "United States"
countryNameFromCode UM = "United States Minor Outlying Islands"
countryNameFromCode UY = "Uruguay"
countryNameFromCode UZ = "Uzbekistan"
countryNameFromCode VU = "Vanuatu"
countryNameFromCode VE = "Venezuela, Bolivarian Republic of"
countryNameFromCode VN = "Viet Nam"
countryNameFromCode VG = "Virgin Islands, British"
countryNameFromCode VI = "Virgin Islands, U.S."
countryNameFromCode WF = "Wallis and Futuna"
countryNameFromCode EH = "Western Sahara"
countryNameFromCode YE = "Yemen"
countryNameFromCode ZM = "Zambia"
countryNameFromCode ZW = "Zimbabwe"
