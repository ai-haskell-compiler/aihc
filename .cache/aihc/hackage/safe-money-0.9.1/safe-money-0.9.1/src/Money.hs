{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Import this module qualified as follows:
--
-- @
-- import qualified Money
-- @
--
-- Note: This module exports support for many well-known currencies
-- out-of-the-box, but you are not limited to the currencies mentioned here. You
-- can simply create a new 'I.UnitScale' instance, and /voilà/. If you want to add a
-- new currency to the out-of-the-box offer, please request so in
-- https://github.com/k0001/safe-money/issues and the authors will see to it.
--
-- This module offers plenty of documentation, but for a deep explanation of
-- how all of the pieces fit together, please read
-- <https://ren.zone/articles/safe-money>. Notice, however, that this library
-- has changed a bit since that article was written. You can always see the
-- [change log](https://github.com/k0001/safe-money/blob/master/changelog.md) to
-- understand what has changed.
--
-- Also, keep in mind that useful instances for the many types defined by
-- @safe-money@ can be found in these other libraries:
--
-- * [safe-money-aeson](https://hackage.haskell.org/package/safe-money-aeson):
--   `FromJSON` and `ToJSON` instances (from the
--   [aeson](https://hackage.haskell.org/package/aeson) library).
--
-- * [safe-money-cereal](https://hackage.haskell.org/package/safe-money-cereal):
--   `Serialize` instances (from the
--   [cereal](https://hackage.haskell.org/package/cereal) library).
--
-- * [safe-money-serialise](https://hackage.haskell.org/package/safe-money-serialise):
--   `Serialise` instances (from the
--   [serialise](https://hackage.haskell.org/package/serialise) library).
--
-- * [safe-money-store](https://hackage.haskell.org/package/safe-money-store):
--   `Store` instances (from the
--   [store](https://hackage.haskell.org/package/store) library).
--
-- * [safe-money-xmlbf](https://hackage.haskell.org/package/safe-money-xmlbf):
--   `FromXml` and `ToXml` instances (from the
--   [xmlbf](https://hackage.haskell.org/package/xmlbf) library).
module Money
 ( -- * Dense monetary values
   I.Dense
 , I.denseCurrency
 , I.dense
 , I.dense'
 , I.denseFromDiscrete
 , I.denseFromDecimal
 , I.denseToDecimal
   -- * Discrete monetary values
 , I.Discrete
 , I.Discrete'
 , I.discrete
 , I.discreteCurrency
 , I.discreteFromDense
 , I.discreteFromDecimal
 , I.discreteToDecimal
   -- * Currency scales
 , I.Scale
 , I.scaleFromRational
 , I.scaleToRational
 , I.scale
 , I.UnitScale
 , I.CurrencyScale
 , I.GoodScale
 , I.ErrScaleNonCanonical
   -- * Currency exchange
 , I.ExchangeRate
 , I.exchangeRate
 , I.exchangeRate'
 , I.exchange
 , I.exchangeRateRecip
 , I.exchangeRateFromDecimal
 , I.exchangeRateToDecimal
 , I.exchangeRateToRational
   -- * Serializable representations
 , I.SomeDense
 , I.toSomeDense
 , I.mkSomeDense
 , I.fromSomeDense
 , I.withSomeDense
 , I.someDenseToDecimal
 , I.someDenseCurrency
 , I.someDenseAmount
 , I.SomeDiscrete
 , I.toSomeDiscrete
 , I.mkSomeDiscrete
 , I.fromSomeDiscrete
 , I.withSomeDiscrete
 , I.someDiscreteToDecimal
 , I.someDiscreteCurrency
 , I.someDiscreteScale
 , I.someDiscreteAmount
 , I.SomeExchangeRate
 , I.toSomeExchangeRate
 , I.mkSomeExchangeRate
 , I.fromSomeExchangeRate
 , I.withSomeExchangeRate
 , I.someExchangeRateToDecimal
 , I.someExchangeRateSrcCurrency
 , I.someExchangeRateDstCurrency
 , I.someExchangeRateRate
 -- * Miscellaneous
 , I.Approximation(..)
 -- ** Decimal config
 , I.DecimalConf(..)
 , I.defaultDecimalConf
 -- *** Separators
 , I.Separators
 , I.mkSeparators
 , I.separatorsComma
 , I.separatorsCommaDot
 , I.separatorsCommaNarrownbsp
 , I.separatorsCommaNbsp
 , I.separatorsCommaThinsp
 , I.separatorsCommaSpace
 , I.separatorsDot
 , I.separatorsDotComma
 , I.separatorsDotNarrownbsp
 , I.separatorsDotThinsp
 , I.separatorsDotNbsp
 , I.separatorsDotSpace
 ) where

import qualified Money.Internal as I

--------------------------------------------------------------------------------
-- Currency scales

-- | United Arab Emirates dirham
type instance I.CurrencyScale "AED" = I.UnitScale "AED" "fils"
type instance I.UnitScale "AED" "dirham" = '(1, 1)
type instance I.UnitScale "AED" "fils" = '(100, 1)
-- | Afghan afghani
type instance I.CurrencyScale "AFN" = I.UnitScale "AFN" "pul"
type instance I.UnitScale "AFN" "afghani" = '(1, 1)
type instance I.UnitScale "AFN" "pul" = '(100, 1)
-- | Albanian lek
type instance I.CurrencyScale "ALL" = I.UnitScale "ALL" "lek"
type instance I.UnitScale "ALL" "lek" = '(1, 1)
type instance I.UnitScale "ALL" "qindarke" = '(100, 1)
-- | Armenian dram
type instance I.CurrencyScale "AMD" = I.UnitScale "AMD" "luma"
type instance I.UnitScale "AMD" "dram" = '(1, 1)
type instance I.UnitScale "AMD" "luma" = '(100, 1)
-- | Netherlands Antillean guilder
type instance I.CurrencyScale "ANG" = I.UnitScale "AMD" "cent"
type instance I.UnitScale "ANG" "guilder" = '(1, 1)
type instance I.UnitScale "ANG" "cent" = '(100, 1)
-- | Angolan kwanza
type instance I.CurrencyScale "AOA" = I.UnitScale "AOA" "centimo"
type instance I.UnitScale "AOA" "kwanza" = '(1, 1)
type instance I.UnitScale "AOA" "centimo" = '(100, 1)
-- | Argentine peso
type instance I.CurrencyScale "ARS" = I.UnitScale "ARS" "centavo"
type instance I.UnitScale "ARS" "peso" = '(1, 1)
type instance I.UnitScale "ARS" "centavo" = '(100, 1)
-- | Australian dollar
type instance I.CurrencyScale "AUD" = I.UnitScale "AUD" "cent"
type instance I.UnitScale "AUD" "dollar" = '(1, 1)
type instance I.UnitScale "AUD" "cent" = '(100, 1)
-- | Aruban florin
type instance I.CurrencyScale "AWG" = I.UnitScale "AWG" "cent"
type instance I.UnitScale "AWG" "florin" = '(1, 1)
type instance I.UnitScale "AWG" "cent" = '(100, 1)
-- | Azerbaijani manat
type instance I.CurrencyScale "AZN" = I.UnitScale "AZN" "qapik"
type instance I.UnitScale "AZN" "manat" = '(1, 1)
type instance I.UnitScale "AZN" "qapik" = '(100, 1)
-- | Bosnia and Herzegovina convertible mark
type instance I.CurrencyScale "BAM" = I.UnitScale "BAM" "fenig"
type instance I.UnitScale "BAM" "mark" = '(1, 1)
type instance I.UnitScale "BAM" "fening" = '(100, 1)
-- | Barbadian dollar
type instance I.CurrencyScale "BBD" = I.UnitScale "BBD" "cent"
type instance I.UnitScale "BBD" "dollar" = '(1, 1)
type instance I.UnitScale "BBD" "cent" = '(100, 1)
-- | Bangladeshi taka
type instance I.CurrencyScale "BDT" = I.UnitScale "BDT" "paisa"
type instance I.UnitScale "BDT" "taka" = '(1, 1)
type instance I.UnitScale "BDT" "paisa" = '(100, 1)
-- | Bulgarian lev
type instance I.CurrencyScale "BGN" = I.UnitScale "BGN" "stotinka"
type instance I.UnitScale "BGN" "lev" = '(1, 1)
type instance I.UnitScale "BGN" "stotinka" = '(100, 1)
-- | Bahraini dinar
type instance I.CurrencyScale "BHD" = I.UnitScale "BHD" "fils"
type instance I.UnitScale "BHD" "dinar" = '(1, 1)
type instance I.UnitScale "BHD" "fils" = '(1000, 1)
-- | Burundi franc
type instance I.CurrencyScale "BIF" = I.UnitScale "BIF" "centime"
type instance I.UnitScale "BIF" "franc" = '(1, 1)
type instance I.UnitScale "BIF" "centime" = '(100, 1)
-- | Bermudian dollar
type instance I.CurrencyScale "BMD" = I.UnitScale "BMD" "cent"
type instance I.UnitScale "BMD" "dollar" = '(1, 1)
type instance I.UnitScale "BMD" "cent" = '(100, 1)
-- | Brunei dollar
type instance I.CurrencyScale "BND" = I.UnitScale "BND" "sen"
type instance I.UnitScale "BND" "dollar" = '(1, 1)
type instance I.UnitScale "BND" "sen" = '(100, 1)
-- | Bolivian boliviano
type instance I.CurrencyScale "BOB" = I.UnitScale "BOB" "centavo"
type instance I.UnitScale "BOB" "boliviano" = '(1, 1)
type instance I.UnitScale "BOB" "centavo" = '(100, 1)
-- | Bolivian Mvdol
type instance I.CurrencyScale "BOV" = '(100, 1)
-- | Brazilian real
type instance I.CurrencyScale "BRL" = I.UnitScale "BRL" "centavo"
type instance I.UnitScale "BRL" "real" = '(1, 1)
type instance I.UnitScale "BRL" "centavo" = '(100, 1)
-- | Bahamian dollar
type instance I.CurrencyScale "BSD" = I.UnitScale "BSD" "cent"
type instance I.UnitScale "BSD" "dollar" = '(1, 1)
type instance I.UnitScale "BSD" "cent" = '(100, 1)
-- | Bhutanese ngultrum
type instance I.CurrencyScale "BTN" = I.UnitScale "BTN" "chetrum"
type instance I.UnitScale "BTN" "ngultrum" = '(1, 1)
type instance I.UnitScale "BTN" "chetrum" = '(100, 1)
-- | Botswana pula
type instance I.CurrencyScale "BWP" = I.UnitScale "BWP" "thebe"
type instance I.UnitScale "BWP" "pula" = '(1, 1)
type instance I.UnitScale "BWP" "thebe" = '(100, 1)
-- | Belarusian ruble
type instance I.CurrencyScale "BYN" = I.UnitScale "BYN" "kapiejka"
type instance I.UnitScale "BYN" "ruble" = '(1, 1)
type instance I.UnitScale "BYN" "kapiejka" = '(100, 1)
-- | Belarusian ruble
type instance I.CurrencyScale "BYR" = I.UnitScale "BYR" "kapiejka"
type instance I.UnitScale "BYR" "ruble" = '(1, 1)
type instance I.UnitScale "BYR" "kapiejka" = '(100, 1)
-- | Belize dollar
type instance I.CurrencyScale "BZD" = I.UnitScale "BZD" "cent"
type instance I.UnitScale "BZD" "dollar" = '(1, 1)
type instance I.UnitScale "BZD" "cent" = '(100, 1)
-- | Canadian dollar
type instance I.CurrencyScale "CAD" = I.UnitScale "CAD" "cent"
type instance I.UnitScale "CAD" "dollar" = '(1, 1)
type instance I.UnitScale "CAD" "cent" = '(100, 1)
-- | Congolese franc
type instance I.CurrencyScale "CDF" = I.UnitScale "CDF" "centime"
type instance I.UnitScale "CDF" "franc" = '(1, 1)
type instance I.UnitScale "CDF" "centime" = '(100, 1)
-- | WIR euro
type instance I.CurrencyScale "CHE" = '(100, 1)
-- | Swiss franc
type instance I.CurrencyScale "CHF" = I.UnitScale "CHF" "rappen"
type instance I.UnitScale "CHF" "franc" = '(1, 1)
type instance I.UnitScale "CHF" "rappen" = '(100, 1)
-- | WIR franc
type instance I.CurrencyScale "CHW" = '(100, 1)
-- | Chilean unidad de fomento
type instance I.CurrencyScale "CLF" = '(10000, 1)
-- | Chilean peso
type instance I.CurrencyScale "CLP" = I.UnitScale "CLP" "centavo"
type instance I.UnitScale "CLP" "peso" = '(1, 1)
type instance I.UnitScale "CLP" "centavo" = '(100, 1)
-- | Chinese Renminbi
type instance I.CurrencyScale "CNY" = I.UnitScale "CNY" "fen"
type instance I.UnitScale "CNY" "yuan" = '(1, 1)
type instance I.UnitScale "CNY" "fen" = '(100, 1)
-- | Colombian peso
type instance I.CurrencyScale "COP" = I.UnitScale "COP" "centavo"
type instance I.UnitScale "COP" "peso" = '(1, 1)
type instance I.UnitScale "COP" "centavo" = '(100, 1)
-- | Colombian unidad de valor real
type instance I.CurrencyScale "COU" = '(100, 1)
-- | Costa Rican colon
type instance I.CurrencyScale "CRC" = I.UnitScale "CRC" "centimo"
type instance I.UnitScale "CRC" "colon" = '(1, 1)
type instance I.UnitScale "CRC" "centimo" = '(100, 1)
-- | Cuban peso convertible
type instance I.CurrencyScale "CUC" = I.UnitScale "CUC" "centavo"
type instance I.UnitScale "CUC" "peso" = '(1, 1)
type instance I.UnitScale "CUC" "centavo" = '(100, 1)
-- | Cuban peso
type instance I.CurrencyScale "CUP" = I.UnitScale "CUP" "centavo"
type instance I.UnitScale "CUP" "peso" = '(1, 1)
type instance I.UnitScale "CUP" "centavo" = '(100, 1)
-- | Cape Verdean escudo
type instance I.CurrencyScale "CVE" = I.UnitScale "CVE" "centavo"
type instance I.UnitScale "CVE" "escudo" = '(1, 1)
type instance I.UnitScale "CVE" "centavo" = '(100, 1)
-- | Czech koruna
type instance I.CurrencyScale "CZK" = I.UnitScale "CZK" "haler"
type instance I.UnitScale "CZK" "koruna" = '(1, 1)
type instance I.UnitScale "CZK" "haler" = '(100, 1)
-- | Djiboutian franc
type instance I.CurrencyScale "DJF" = I.UnitScale "DJF" "centime"
type instance I.UnitScale "DJF" "franc" = '(1, 1)
type instance I.UnitScale "DJF" "centime" = '(100, 1)
-- | Danish krone
type instance I.CurrencyScale "DKK" = I.UnitScale "DKK" "ore"
type instance I.UnitScale "DKK" "krone" = '(1, 1)
type instance I.UnitScale "DKK" "ore" = '(100, 1)
-- | Dominican peso
type instance I.CurrencyScale "DOP" = I.UnitScale "DOP" "centavo"
type instance I.UnitScale "DOP" "peso" = '(1, 1)
type instance I.UnitScale "DOP" "centavo" = '(100, 1)
-- | Algerian dinar
type instance I.CurrencyScale "DZD" = I.UnitScale "DZD" "santeem"
type instance I.UnitScale "DZD" "dinar" = '(1, 1)
type instance I.UnitScale "DZD" "santeem" = '(100, 1)
-- | Egyptian pound
type instance I.CurrencyScale "EGP" = I.UnitScale "EGP" "piastre"
type instance I.UnitScale "EGP" "pound" = '(1, 1)
type instance I.UnitScale "EGP" "piastre" = '(100, 1)
-- | Eritrean nakfa
type instance I.CurrencyScale "ERN" = I.UnitScale "ERN" "cent"
type instance I.UnitScale "ERN" "nafka" = '(1, 1)
type instance I.UnitScale "ERN" "cent" = '(100, 1)
-- | Ethiopian birr
type instance I.CurrencyScale "ETB" = I.UnitScale "ETB" "santim"
type instance I.UnitScale "ETB" "birr" = '(1, 1)
type instance I.UnitScale "ETB" "santim" = '(100, 1)
-- | European euro
type instance I.CurrencyScale "EUR" = I.UnitScale "EUR" "cent"
type instance I.UnitScale "EUR" "euro" = '(1, 1)
type instance I.UnitScale "EUR" "cent" = '(100, 1)
-- | Fijian dollar
type instance I.CurrencyScale "FJD" = I.UnitScale "FJD" "cent"
type instance I.UnitScale "FJD" "dollar" = '(1, 1)
type instance I.UnitScale "FJD" "cent" = '(100, 1)
-- | Falkland Islands pound
type instance I.CurrencyScale "FKP" = I.UnitScale "FKP" "penny"
type instance I.UnitScale "FKP" "pound" = '(1, 1)
type instance I.UnitScale "FKP" "penny" = '(100, 1)
-- | Pound sterling
type instance I.CurrencyScale "GBP" = I.UnitScale "GBP" "penny"
type instance I.UnitScale "GBP" "pound" = '(1, 1)
type instance I.UnitScale "GBP" "penny" = '(100, 1)
-- | Georgian lari
type instance I.CurrencyScale "GEL" = I.UnitScale "GEL" "tetri"
type instance I.UnitScale "GEL" "lari" = '(1, 1)
type instance I.UnitScale "GEL" "tetri" = '(100, 1)
-- | Ghanaian cedi
type instance I.CurrencyScale "GHS" = I.UnitScale "GHS" "pesewa"
type instance I.UnitScale "GHS" "cedi" = '(1, 1)
type instance I.UnitScale "GHS" "pesewa" = '(100, 1)
-- | Gibraltar pound
type instance I.CurrencyScale "GIP" = I.UnitScale "GIP" "penny"
type instance I.UnitScale "GIP" "pound" = '(1, 1)
type instance I.UnitScale "GIP" "penny" = '(100, 1)
-- | Gambian dalasi
type instance I.CurrencyScale "GMD" = I.UnitScale "GMD" "butut"
type instance I.UnitScale "GMD" "dalasi" = '(1, 1)
type instance I.UnitScale "GMD" "butut" = '(100, 1)
-- | Guinean franc
type instance I.CurrencyScale "GNF" = I.UnitScale "GNF" "centime"
type instance I.UnitScale "GNF" "franc" = '(1, 1)
type instance I.UnitScale "GNF" "centime" = '(100, 1)
-- | Guatemalan quetzal
type instance I.CurrencyScale "GTQ" = I.UnitScale "GTQ" "centavo"
type instance I.UnitScale "GTQ" "quetzal" = '(1, 1)
type instance I.UnitScale "GTQ" "centavo" = '(100, 1)
-- | Guyanese dollar
type instance I.CurrencyScale "GYD" = I.UnitScale "GYD" "cent"
type instance I.UnitScale "GYD" "dollar" = '(1, 1)
type instance I.UnitScale "GYD" "cent" = '(100, 1)
-- | Hong Kong dollar
type instance I.CurrencyScale "HKD" = I.UnitScale "HKD" "cent"
type instance I.UnitScale "HKD" "dollar" = '(1, 1)
type instance I.UnitScale "HKD" "cent" = '(100, 1)
-- | Honduran lempira
type instance I.CurrencyScale "HNL" = I.UnitScale "HNL" "centavo"
type instance I.UnitScale "HNL" "lempira" = '(1, 1)
type instance I.UnitScale "HNL" "centavo" = '(100, 1)
-- | Croatian kuna
type instance I.CurrencyScale "HRK" = I.UnitScale "HRK" "lipa"
type instance I.UnitScale "HRK" "kuna" = '(1, 1)
type instance I.UnitScale "HRK" "lipa" = '(100, 1)
-- | Haitian gourde
type instance I.CurrencyScale "HTG" = I.UnitScale "HTG" "centime"
type instance I.UnitScale "HTG" "gourde" = '(1, 1)
type instance I.UnitScale "HTG" "centime" = '(100, 1)
-- | Hungarian forint
type instance I.CurrencyScale "HUF" = I.UnitScale "HUF" "filler"
type instance I.UnitScale "HUF" "forint" = '(1, 1)
type instance I.UnitScale "HUF" "filler" = '(100, 1)
-- | Indonesian rupiah
type instance I.CurrencyScale "IDR" = I.UnitScale "IDR" "sen"
type instance I.UnitScale "IDR" "rupiah" = '(1, 1)
type instance I.UnitScale "IDR" "sen" = '(100, 1)
-- | Israeli new shekel
type instance I.CurrencyScale "ILS" = I.UnitScale "ILS" "agora"
type instance I.UnitScale "ILS" "shekel" = '(1, 1)
type instance I.UnitScale "ILS" "agora" = '(100, 1)
-- | Indian rupee
type instance I.CurrencyScale "INR" = I.UnitScale "INR" "paisa"
type instance I.UnitScale "INR" "rupee" = '(1, 1)
type instance I.UnitScale "INR" "paisa" = '(100, 1)
-- | Iraqi dinar
type instance I.CurrencyScale "IQD" = I.UnitScale "IQD" "fils"
type instance I.UnitScale "IQD" "dinar" = '(1, 1)
type instance I.UnitScale "IQD" "fils" = '(1000, 1)
-- | Iranian rial
type instance I.CurrencyScale "IRR" = I.UnitScale "IRR" "dinar"
type instance I.UnitScale "IRR" "rial" = '(1, 1)
type instance I.UnitScale "IRR" "dinar" = '(100, 1)
-- | Icelandic króna
type instance I.CurrencyScale "ISK" = I.UnitScale "ISK" "eyrir"
type instance I.UnitScale "ISK" "krona" = '(1, 1)
type instance I.UnitScale "ISK" "eyrir" = '(100, 1)
-- | Jamaican dollar
type instance I.CurrencyScale "JMD" = I.UnitScale "JMD" "cent"
type instance I.UnitScale "JMD" "dollar" = '(1, 1)
type instance I.UnitScale "JMD" "cent" = '(100, 1)
-- | Jordanian dinar
type instance I.CurrencyScale "JOD" = I.UnitScale "JOD" "piastre"
type instance I.UnitScale "JOD" "dinar" = '(1, 1)
type instance I.UnitScale "JOD" "piastre" = '(100, 1)
-- | Japanese yen
type instance I.CurrencyScale "JPY" = I.UnitScale "JPY" "sen"
type instance I.UnitScale "JPY" "yen" = '(1, 1)
type instance I.UnitScale "JPY" "sen" = '(100, 1)
-- | Kenyan shilling
type instance I.CurrencyScale "KES" = I.UnitScale "KES" "cent"
type instance I.UnitScale "KES" "shilling" = '(1, 1)
type instance I.UnitScale "KES" "cent" = '(100, 1)
-- | Kyrgyzstani som
type instance I.CurrencyScale "KGS" = I.UnitScale "KGS" "tyiyn"
type instance I.UnitScale "KGS" "som" = '(1, 1)
type instance I.UnitScale "KGS" "tyiyn" = '(100, 1)
-- | Cambodian riel
type instance I.CurrencyScale "KHR" = I.UnitScale "KHR" "sen"
type instance I.UnitScale "KHR" "riel" = '(1, 1)
type instance I.UnitScale "KHR" "sen" = '(100, 1)
-- | Comorian franc
type instance I.CurrencyScale "KMF" = I.UnitScale "KMF" "centime"
type instance I.UnitScale "KMF" "franc" = '(1, 1)
type instance I.UnitScale "KMF" "centime" = '(100, 1)
-- | North Korean won
type instance I.CurrencyScale "KPW" = I.UnitScale "KPW" "chon"
type instance I.UnitScale "KPW" "won" = '(1, 1)
type instance I.UnitScale "KPW" "chon" = '(100, 1)
-- | South Korean won
type instance I.CurrencyScale "KRW" = I.UnitScale "KRW" "jeon"
type instance I.UnitScale "KRW" "won" = '(1, 1)
type instance I.UnitScale "KRW" "jeon" = '(100, 1)
-- | Kuwaiti dinar
type instance I.CurrencyScale "KWD" = I.UnitScale "KWD" "fils"
type instance I.UnitScale "KWD" "dinar" = '(1, 1)
type instance I.UnitScale "KWD" "fils" = '(1000, 1)
-- | Cayman Islands dollar
type instance I.CurrencyScale "KYD" = I.UnitScale "KYD" "cent"
type instance I.UnitScale "KYD" "dollar" = '(1, 1)
type instance I.UnitScale "KYD" "cent" = '(100, 1)
-- | Kazakhstani tenge
type instance I.CurrencyScale "KZT" = I.UnitScale "KZT" "tiyin"
type instance I.UnitScale "KZT" "tenge" = '(1, 1)
type instance I.UnitScale "KZT" "tiyin" = '(100, 1)
-- | Lao kip
type instance I.CurrencyScale "LAK" = I.UnitScale "LAK" "att"
type instance I.UnitScale "LAK" "kip" = '(1, 1)
type instance I.UnitScale "LAK" "att" = '(100, 1)
-- | Lebanese pound
type instance I.CurrencyScale "LBP" = I.UnitScale "LBP" "piastre"
type instance I.UnitScale "LBP" "pound" = '(1, 1)
type instance I.UnitScale "LBP" "piastre" = '(100, 1)
-- | Sri Lankan rupee
type instance I.CurrencyScale "LKR" = I.UnitScale "LKR" "cent"
type instance I.UnitScale "LKR" "rupee" = '(1, 1)
type instance I.UnitScale "LKR" "cent" = '(100, 1)
-- | Liberian dollar
type instance I.CurrencyScale "LRD" = I.UnitScale "LRD" "cent"
type instance I.UnitScale "LRD" "dollar" = '(1, 1)
type instance I.UnitScale "LRD" "cent" = '(100, 1)
-- | Lesotho loti
type instance I.CurrencyScale "LSL" = I.UnitScale "LSL" "sente"
type instance I.UnitScale "LSL" "loti" = '(1, 1)
type instance I.UnitScale "LSL" "sente" = '(100, 1)
-- | Libyan dinar
type instance I.CurrencyScale "LYD" = I.UnitScale "LYD" "dirham"
type instance I.UnitScale "LYD" "dinar" = '(1, 1)
type instance I.UnitScale "LYD" "dirham" = '(1000, 1)
-- | Moroccan dirham
type instance I.CurrencyScale "MAD" = I.UnitScale "MAD" "centime"
type instance I.UnitScale "MAD" "dirham" = '(1, 1)
type instance I.UnitScale "MAD" "centime" = '(100, 1)
-- | Moldovan leu
type instance I.CurrencyScale "MDL" = I.UnitScale "MDL" "ban"
type instance I.UnitScale "MDL" "leu" = '(1, 1)
type instance I.UnitScale "MDL" "ban" = '(100, 1)
-- | Malagasy ariary
type instance I.CurrencyScale "MGA" = I.UnitScale "MGA" "iraimbilanja"
type instance I.UnitScale "MGA" "ariary" = '(1, 1)
type instance I.UnitScale "MGA" "iraimbilanja" = '(5, 1)
-- | Macedonian denar
type instance I.CurrencyScale "MKD" = I.UnitScale "MKD" "deni"
type instance I.UnitScale "MKD" "denar" = '(1, 1)
type instance I.UnitScale "MKD" "deni" = '(100, 1)
-- | Myanmar kyat
type instance I.CurrencyScale "MMK" = I.UnitScale "MMK" "pya"
type instance I.UnitScale "MMK" "kyat" = '(1, 1)
type instance I.UnitScale "MMK" "pya" = '(100, 1)
-- | Mongolian tugrik
type instance I.CurrencyScale "MNT" = I.UnitScale "MNT" "mongo"
type instance I.UnitScale "MNT" "tugrik" = '(1, 1)
type instance I.UnitScale "MNT" "mongo" = '(100, 1)
-- | Macanese pataca
type instance I.CurrencyScale "MOP" = I.UnitScale "MOP" "avo"
type instance I.UnitScale "MOP" "pataca" = '(1, 1)
type instance I.UnitScale "MOP" "avo" = '(100, 1)
-- | Mauritanian ouguiya
type instance I.CurrencyScale "MRO" = I.UnitScale "MRO" "khoums"
type instance I.UnitScale "MRO" "ouguiya" = '(1, 1)
type instance I.UnitScale "MRO" "khoums" = '(5, 1)
-- | Mauritian rupee
type instance I.CurrencyScale "MUR" = I.UnitScale "MUR" "cent"
type instance I.UnitScale "MUR" "rupee" = '(1, 1)
type instance I.UnitScale "MUR" "cent" = '(100, 1)
-- | Maldivian rufiyaa
type instance I.CurrencyScale "MVR" = I.UnitScale "MVR" "laari"
type instance I.UnitScale "MVR" "rufiyaa" = '(1, 1)
type instance I.UnitScale "MVR" "laari" = '(100, 1)
-- | Malawian kwacha
type instance I.CurrencyScale "MWK" = I.UnitScale "MWK" "tambala"
type instance I.UnitScale "MWK" "kwacha" = '(1, 1)
type instance I.UnitScale "MWK" "tambala" = '(100, 1)
-- | Mexican peso
type instance I.CurrencyScale "MXN" = I.UnitScale "MXN" "centavo"
type instance I.UnitScale "MXN" "peso" = '(1, 1)
type instance I.UnitScale "MXN" "centavo" = '(100, 1)
-- | Mexican unidad de inversion
type instance I.CurrencyScale "MXV" = '(100, 1)
-- | Malaysian ringgit
type instance I.CurrencyScale "MYR" = I.UnitScale "MYR" "sen"
type instance I.UnitScale "MYR" "ringgit" = '(1, 1)
type instance I.UnitScale "MYR" "sen" = '(100, 1)
-- | Mozambican metical
type instance I.CurrencyScale "MZN" = I.UnitScale "MZN" "centavo"
type instance I.UnitScale "MZN" "metical" = '(1, 1)
type instance I.UnitScale "MZN" "centavo" = '(100, 1)
-- | Namibian dollar
type instance I.CurrencyScale "NAD" = I.UnitScale "NAD" "cent"
type instance I.UnitScale "NAD" "dollar" = '(1, 1)
type instance I.UnitScale "NAD" "cent" = '(100, 1)
-- | Nigerian naira
type instance I.CurrencyScale "NGN" = I.UnitScale "NGN" "kobo"
type instance I.UnitScale "NGN" "naira" = '(1, 1)
type instance I.UnitScale "NGN" "kobo" = '(100, 1)
-- | Nicaraguan cordoba
type instance I.CurrencyScale "NIO" = I.UnitScale "NIO" "centavo"
type instance I.UnitScale "NIO" "cordoba" = '(1, 1)
type instance I.UnitScale "NIO" "centavo" = '(100, 1)
-- | Norwegian krone
type instance I.CurrencyScale "NOK" = I.UnitScale "NOK" "ore"
type instance I.UnitScale "NOK" "krone" = '(1, 1)
type instance I.UnitScale "NOK" "ore" = '(100, 1)
-- | Nepalese rupee
type instance I.CurrencyScale "NPR" = I.UnitScale "NPR" "paisa"
type instance I.UnitScale "NPR" "rupee" = '(1, 1)
type instance I.UnitScale "NPR" "paisa" = '(100, 1)
-- | New Zealand dollar
type instance I.CurrencyScale "NZD" = I.UnitScale "NZD" "cent"
type instance I.UnitScale "NZD" "dollar" = '(1, 1)
type instance I.UnitScale "NZD" "cent" = '(100, 1)
-- | Omani rial
type instance I.CurrencyScale "OMR" = I.UnitScale "OMR" "baisa"
type instance I.UnitScale "OMR" "rial" = '(1, 1)
type instance I.UnitScale "OMR" "baisa" = '(1000, 1)
-- | Panamenian balboa
type instance I.CurrencyScale "PAB" = I.UnitScale "PAB" "centesimo"
type instance I.UnitScale "PAB" "balboa" = '(1, 1)
type instance I.UnitScale "PAB" "centesimo" = '(100, 1)
-- | Peruvian sol
type instance I.CurrencyScale "PEN" = I.UnitScale "PEN" "centimo"
type instance I.UnitScale "PEN" "sol" = '(1, 1)
type instance I.UnitScale "PEN" "centimo" = '(100, 1)
-- | Papua New Guinean kina
type instance I.CurrencyScale "PGK" = I.UnitScale "PGK" "toea"
type instance I.UnitScale "PGK" "kina" = '(1, 1)
type instance I.UnitScale "PGK" "toea" = '(100, 1)
-- | Philippine peso
type instance I.CurrencyScale "PHP" = I.UnitScale "PHP" "centavo"
type instance I.UnitScale "PHP" "peso" = '(1, 1)
type instance I.UnitScale "PHP" "centavo" = '(100, 1)
-- | Pakistani rupee
type instance I.CurrencyScale "PKR" = I.UnitScale "PKR" "paisa"
type instance I.UnitScale "PKR" "rupee" = '(1, 1)
type instance I.UnitScale "PKR" "paisa" = '(100, 1)
-- | Polish zloty
type instance I.CurrencyScale "PLN" = I.UnitScale "PLN" "grosz"
type instance I.UnitScale "PLN" "zloty" = '(1, 1)
type instance I.UnitScale "PLN" "grosz" = '(100, 1)
-- | Paraguayan guarani
type instance I.CurrencyScale "PYG" = I.UnitScale "PYG" "centimo"
type instance I.UnitScale "PYG" "guarani" = '(1, 1)
type instance I.UnitScale "PYG" "centimo" = '(100, 1)
-- | Qatari riyal
type instance I.CurrencyScale "QAR" = I.UnitScale "QAR" "dirham"
type instance I.UnitScale "QAR" "riyal" = '(1, 1)
type instance I.UnitScale "QAR" "dirham" = '(100, 1)
-- | Romanian leu
type instance I.CurrencyScale "RON" = I.UnitScale "RON" "ban"
type instance I.UnitScale "RON" "leu" = '(1, 1)
type instance I.UnitScale "RON" "ban" = '(100, 1)
-- | Serbian dinar
type instance I.CurrencyScale "RSD" = I.UnitScale "RSD" "para"
type instance I.UnitScale "RSD" "dinar" = '(1, 1)
type instance I.UnitScale "RSD" "para" = '(100, 1)
-- | Russian ruble
type instance I.CurrencyScale "RUB" = I.UnitScale "RUB" "kopek"
type instance I.UnitScale "RUB" "ruble" = '(1, 1)
type instance I.UnitScale "RUB" "kopek" = '(100, 1)
-- | Rwandan franc
type instance I.CurrencyScale "RWF" = I.UnitScale "RWF" "centime"
type instance I.UnitScale "RWF" "franc" = '(1, 1)
type instance I.UnitScale "RWF" "centime" = '(100, 1)
-- | Saudi Arabian riyal
type instance I.CurrencyScale "SAR" = I.UnitScale "SAR" "halala"
type instance I.UnitScale "SAR" "riyal" = '(1, 1)
type instance I.UnitScale "SAR" "halala" = '(100, 1)
-- | Solomon Islands dollar
type instance I.CurrencyScale "SBD" = I.UnitScale "SBD" "cent"
type instance I.UnitScale "SBD" "dollar" = '(1, 1)
type instance I.UnitScale "SBD" "cent" = '(100, 1)
-- | Seychellois rupee
type instance I.CurrencyScale "SCR" = I.UnitScale "SCR" "cent"
type instance I.UnitScale "SCR" "rupee" = '(1, 1)
type instance I.UnitScale "SCR" "cent" = '(100, 1)
-- | Sudanese pound
type instance I.CurrencyScale "SDG" = I.UnitScale "SDG" "piastre"
type instance I.UnitScale "SDG" "pound" = '(1, 1)
type instance I.UnitScale "SDG" "piastre" = '(100, 1)
-- | Swedish krona
type instance I.CurrencyScale "SEK" = I.UnitScale "SEK" "ore"
type instance I.UnitScale "SEK" "krona" = '(1, 1)
type instance I.UnitScale "SEK" "ore" = '(100, 1)
-- | Singapore dollar
type instance I.CurrencyScale "SGD" = I.UnitScale "SGD" "cent"
type instance I.UnitScale "SGD" "dollar" = '(1, 1)
type instance I.UnitScale "SGD" "cent" = '(100, 1)
-- | Saint Helena pound
type instance I.CurrencyScale "SHP" = I.UnitScale "SHP" "penny"
type instance I.UnitScale "SHP" "pound" = '(1, 1)
type instance I.UnitScale "SHP" "penny" = '(100, 1)
-- | Sierra Leonean leone
type instance I.CurrencyScale "SLL" = I.UnitScale "SLL" "cent"
type instance I.UnitScale "SLL" "leone" = '(1, 1)
type instance I.UnitScale "SLL" "cent" = '(100, 1)
-- | Somali shilling
type instance I.CurrencyScale "SOS" = I.UnitScale "SOS" "cent"
type instance I.UnitScale "SOS" "shilling" = '(1, 1)
type instance I.UnitScale "SOS" "cent" = '(100, 1)
-- | Surinamese dollar
type instance I.CurrencyScale "SRD" = I.UnitScale "SRD" "cent"
type instance I.UnitScale "SRD" "dollar" = '(1, 1)
type instance I.UnitScale "SRD" "cent" = '(100, 1)
-- | South Sudanese pound
type instance I.CurrencyScale "SSP" = I.UnitScale "SSP" "piastre"
type instance I.UnitScale "SSP" "pound" = '(1, 1)
type instance I.UnitScale "SSP" "piastre" = '(100, 1)
-- | Sao Tome and Principe dobra
type instance I.CurrencyScale "STD" = I.UnitScale "STD" "centimo"
type instance I.UnitScale "STD" "dobra" = '(1, 1)
type instance I.UnitScale "STD" "centimo" = '(100, 1)
-- | Salvadoran colon
type instance I.CurrencyScale "SVC" = I.UnitScale "SVC" "centavo"
type instance I.UnitScale "SVC" "colon" = '(1, 1)
type instance I.UnitScale "SVC" "centavo" = '(100, 1)
-- | Syrian pound
type instance I.CurrencyScale "SYP" = I.UnitScale "SYP" "piastre"
type instance I.UnitScale "SYP" "pound" = '(1, 1)
type instance I.UnitScale "SYP" "piastre" = '(100, 1)
-- | Swazi lilangeni
type instance I.CurrencyScale "SZL" = I.UnitScale "SZL" "cent"
type instance I.UnitScale "SZL" "lilangeni" = '(1, 1)
type instance I.UnitScale "SZL" "cent" = '(100, 1)
-- | Thai baht
type instance I.CurrencyScale "THB" = I.UnitScale "THB" "satang"
type instance I.UnitScale "THB" "baht" = '(1, 1)
type instance I.UnitScale "THB" "satang" = '(100, 1)
-- | Tajikistani somoni
type instance I.CurrencyScale "TJS" = I.UnitScale "TJS" "diram"
type instance I.UnitScale "TJS" "somoni" = '(1, 1)
type instance I.UnitScale "TJS" "diram" = '(100, 1)
-- | Turkmen manat
type instance I.CurrencyScale "TMT" = I.UnitScale "TMT" "tennesi"
type instance I.UnitScale "TMT" "manat" = '(1, 1)
type instance I.UnitScale "TMT" "tennesi" = '(100, 1)
-- | Tunisian dinar
type instance I.CurrencyScale "TND" = I.UnitScale "TND" "millime"
type instance I.UnitScale "TND" "dinar" = '(1, 1)
type instance I.UnitScale "TND" "millime" = '(1000, 1)
-- | Tongan pa’anga
type instance I.CurrencyScale "TOP" = I.UnitScale "TOP" "seniti"
type instance I.UnitScale "TOP" "pa'anga" = '(1, 1)
type instance I.UnitScale "TOP" "seniti" = '(100, 1)
-- | Turkish lira
type instance I.CurrencyScale "TRY" = I.UnitScale "TRY" "kurus"
type instance I.UnitScale "TRY" "lira" = '(1, 1)
type instance I.UnitScale "TRY" "kurus" = '(100, 1)
-- | Tobago Trinidad and Tobago dollar
type instance I.CurrencyScale "TTD" = I.UnitScale "TTD" "cent"
type instance I.UnitScale "TTD" "dollar" = '(1, 1)
type instance I.UnitScale "TTD" "cent" = '(100, 1)
-- | New Taiwan dollar
type instance I.CurrencyScale "TWD" = I.UnitScale "TWD" "cent"
type instance I.UnitScale "TWD" "dollar" = '(1, 1)
type instance I.UnitScale "TWD" "cent" = '(100, 1)
-- | Tanzanian shilling
type instance I.CurrencyScale "TZS" = I.UnitScale "TZS" "cent"
type instance I.UnitScale "TZS" "shilling" = '(1, 1)
type instance I.UnitScale "TZS" "cent" = '(100, 1)
-- | Ukrainian hryvnia
type instance I.CurrencyScale "UAH" = I.UnitScale "UAH" "kopiyka"
type instance I.UnitScale "UAH" "hryvnia" = '(1, 1)
type instance I.UnitScale "UAH" "kopiyka" = '(100, 1)
-- | Ugandan shilling
type instance I.CurrencyScale "UGX" = I.UnitScale "UGX" "cent"
type instance I.UnitScale "UGX" "shilling" = '(1, 1)
type instance I.UnitScale "UGX" "cent" = '(100, 1)
-- | United States dollar
type instance I.CurrencyScale "USD" = I.UnitScale "USD" "cent"
type instance I.UnitScale "USD" "dollar" = '(1, 1)
type instance I.UnitScale "USD" "cent" = '(100, 1)
-- | United States dollar (next day)
type instance I.CurrencyScale "USN" = I.UnitScale "USN" "cent"
type instance I.UnitScale "USN" "dollar" = '(1, 1)
type instance I.UnitScale "USN" "cent" = '(100, 1)
-- | Uruguayan peso en unidades indexadas
type instance I.CurrencyScale "UYI" = '(1, 1)
-- | Uruguayan peso
type instance I.CurrencyScale "UYU" = I.UnitScale "UYU" "centesimo"
type instance I.UnitScale "UYU" "peso" = '(1, 1)
type instance I.UnitScale "UYU" "centesimo" = '(100, 1)
-- | Uruguayan unidad previsional
type instance I.CurrencyScale "UYW" = '(10000, 1)
-- | Uzbekistani som
type instance I.CurrencyScale "UZS" = I.UnitScale "UZS" "tiyin"
type instance I.UnitScale "UZS" "som" = '(1, 1)
type instance I.UnitScale "UZS" "tiyin" = '(100, 1)
-- | Venezuelan bolivar fuerte
type instance I.CurrencyScale "VEF" = I.UnitScale "VEF" "centimo"
type instance I.UnitScale "VEF" "bolivar" = '(1, 1)
type instance I.UnitScale "VEF" "centimo" = '(100, 1)
-- | Venezuelan bolivar soberano
type instance I.CurrencyScale "VES" = I.UnitScale "VES" "centimo"
type instance I.UnitScale "VES" "bolivar" = '(1, 1)
type instance I.UnitScale "VES" "centimo" = '(100, 1)
-- | Vietnamese dong
type instance I.CurrencyScale "VND" = I.UnitScale "VND" "hao"
type instance I.UnitScale "VND" "dong" = '(1, 1)
type instance I.UnitScale "VND" "hao" = '(10, 1)
-- | Vanuatu vatu
type instance I.CurrencyScale "VUV" = I.UnitScale "VUV" "vatu"
type instance I.UnitScale "VUV" "vatu" = '(1, 1)
-- | Samoan tālā
type instance I.CurrencyScale "WST" = I.UnitScale "WST" "sene"
type instance I.UnitScale "WST" "tala" = '(1, 1)
type instance I.UnitScale "WST" "sene" = '(100, 1)
-- | Central African CFA franc
type instance I.CurrencyScale "XAF" = I.UnitScale "XAF" "centime"
type instance I.UnitScale "XAF" "franc" = '(1, 1)
type instance I.UnitScale "XAF" "centime" = '(100, 1)
-- | East Caribbean dollar
type instance I.CurrencyScale "XCD" = I.UnitScale "XCD" "cent"
type instance I.UnitScale "XCD" "dollar" = '(1, 1)
type instance I.UnitScale "XCD" "cent" = '(100, 1)
-- | International Monetary Fund Special Drawing Right
type instance I.CurrencyScale "XDR" = '(1, 1)
-- | West African CFA franc
type instance I.CurrencyScale "XOF" = I.UnitScale "XOF" "centime"
type instance I.UnitScale "XOF" "franc" = '(1, 1)
type instance I.UnitScale "XOF" "centime" = '(100, 1)
-- | CFP franc
type instance I.CurrencyScale "XPF" = I.UnitScale "XPF" "centime"
type instance I.UnitScale "XPF" "franc" = '(1, 1)
type instance I.UnitScale "XPF" "centime" = '(100, 1)
-- | Sucre
type instance I.CurrencyScale "XSU" = '(1, 1)
-- | African Development Bank unit of account
type instance I.CurrencyScale "XUA" = '(1, 1)
-- | Yemeni rial
type instance I.CurrencyScale "YER" = I.UnitScale "YER" "fils"
type instance I.UnitScale "YER" "rial" = '(1, 1)
type instance I.UnitScale "YER" "fils" = '(100, 1)
-- | South African rand
type instance I.CurrencyScale "ZAR" = I.UnitScale "ZAR" "cent"
type instance I.UnitScale "ZAR" "rand" = '(1, 1)
type instance I.UnitScale "ZAR" "cent" = '(100, 1)
-- | Zambian kwacha
type instance I.CurrencyScale "ZMW" = I.UnitScale "ZMW" "ngwee"
type instance I.UnitScale "ZMW" "kwacha" = '(1, 1)
type instance I.UnitScale "ZMW" "ngwee" = '(100, 1)
-- | Zimbawe dollar
type instance I.CurrencyScale "ZWL" = I.UnitScale "ZWL" "cent"
type instance I.UnitScale "ZWL" "dollar" = '(1, 1)
type instance I.UnitScale "ZWL" "cent" = '(100, 1)
-- | Gold. No canonical smallest unit. Unusable instance.
type instance I.CurrencyScale "XAU" = I.ErrScaleNonCanonical "XAU"
type instance I.UnitScale "XAU" "troy-ounce" = '(1, 1)
type instance I.UnitScale "XAU" "grain" = '(480, 1)
type instance I.UnitScale "XAU" "milligrain" = '(480000, 1)
type instance I.UnitScale "XAU" "micrograin" = '(480000000, 1)
type instance I.UnitScale "XAU" "kilogram" = '(31103477, 1000000000)
type instance I.UnitScale "XAU" "gram" = '(31103477, 1000000)
type instance I.UnitScale "XAU" "milligram" = '(31103477, 1000)
type instance I.UnitScale "XAU" "microgram" = '(31103477, 1)
-- | Silver. No canonical smallest unit. Unusable instance.
type instance I.CurrencyScale "XAG" = I.ErrScaleNonCanonical "XAU"
type instance I.UnitScale "XAG" "troy-ounce" = '(1, 1)
type instance I.UnitScale "XAG" "grain" = '(480, 1)
type instance I.UnitScale "XAG" "milligrain" = '(480000, 1)
type instance I.UnitScale "XAG" "micrograin" = '(480000000, 1)
type instance I.UnitScale "XAG" "kilogram" = '(31103477, 1000000000)
type instance I.UnitScale "XAG" "gram" = '(31103477, 1000000)
type instance I.UnitScale "XAG" "milligram" = '(31103477, 1000)
type instance I.UnitScale "XAG" "microgram" = '(31103477, 1)
-- | Palladium. No canonical smallest unit. Unusable instance.
type instance I.CurrencyScale "XPD" = I.ErrScaleNonCanonical "XPD"
type instance I.UnitScale "XPD" "troy-ounce" = '(1, 1)
type instance I.UnitScale "XPD" "grain" = '(480, 1)
type instance I.UnitScale "XPD" "milligrain" = '(480000, 1)
type instance I.UnitScale "XPD" "micrograin" = '(480000000, 1)
type instance I.UnitScale "XPD" "kilogram" = '(31103477, 1000000000)
type instance I.UnitScale "XPD" "gram" = '(31103477, 1000000)
type instance I.UnitScale "XPD" "milligram" = '(31103477, 1000)
type instance I.UnitScale "XPD" "microgram" = '(31103477, 1)
-- | Platinum. No canonical smallest unit. Unusable instance.
type instance I.CurrencyScale "XPT" = I.ErrScaleNonCanonical "XPT"
type instance I.UnitScale "XPT" "troy-ounce" = '(1, 1)
type instance I.UnitScale "XPT" "grain" = '(480, 1)
type instance I.UnitScale "XPT" "milligrain" = '(480000, 1)
type instance I.UnitScale "XPT" "micrograin" = '(480000000, 1)
type instance I.UnitScale "XPT" "kilogram" = '(31103477, 1000000000)
type instance I.UnitScale "XPT" "gram" = '(31103477, 1000000)
type instance I.UnitScale "XPT" "milligram" = '(31103477, 1000)
type instance I.UnitScale "XPT" "microgram" = '(31103477, 1)
-- | Bitcoin
type instance I.CurrencyScale "BTC" = I.UnitScale "BTC" "satoshi"
type instance I.UnitScale "BTC" "bitcoin" = '(1, 1)
type instance I.UnitScale "BTC" "millibitcoin" = '(1000, 1)
type instance I.UnitScale "BTC" "satoshi" = '(100000000, 1)
-- | Bitcoin
type instance I.CurrencyScale "XBT" = I.CurrencyScale "BTC"
type instance I.UnitScale "XBT" "bitcoin" = I.UnitScale "BTC" "bitcoin"
type instance I.UnitScale "XBT" "millibitcoin" = I.UnitScale "BTC" "millibitcoin"
type instance I.UnitScale "XBT" "satoshi" = I.UnitScale "BTC" "satoshi"
-- | Ether
type instance I.CurrencyScale "ETH" = I.UnitScale "ETH" "wei"
type instance I.UnitScale "ETH" "ether"      = '(1, 1)
type instance I.UnitScale "ETH" "kwei"       = '(1000, 1)
type instance I.UnitScale "ETH" "babbage"    = '(1000, 1)
type instance I.UnitScale "ETH" "mwei"       = '(1000000, 1)
type instance I.UnitScale "ETH" "lovelace"   = '(1000000, 1)
type instance I.UnitScale "ETH" "gwei"       = '(1000000000, 1)
type instance I.UnitScale "ETH" "shannon"    = '(1000000000, 1)
type instance I.UnitScale "ETH" "microether" = '(1000000000000, 1)
type instance I.UnitScale "ETH" "szabo"      = '(1000000000000, 1)
type instance I.UnitScale "ETH" "finney"     = '(1000000000000000, 1)
type instance I.UnitScale "ETH" "milliether" = '(1000000000000000, 1)
type instance I.UnitScale "ETH" "wei"        = '(1000000000000000000, 1)
-- | Cardano
type instance I.CurrencyScale "ADA" = I.UnitScale "ADA" "lovelace"
type instance I.UnitScale "ADA" "ada" = '(1, 1)
type instance I.UnitScale "ADA" "lovelace" = '(1000000, 1)
-- | Litecoin
type instance I.CurrencyScale "LTC" = I.UnitScale "LTC" "photon"
type instance I.UnitScale "LTC" "litecoin" = '(1, 1)
type instance I.UnitScale "LTC" "lite" = '(1000, 1)
type instance I.UnitScale "LTC" "photon" = '(100000000, 1)
-- | Ripple
type instance I.CurrencyScale "XRP" = I.UnitScale "XRP" "drop"
type instance I.UnitScale "XRP" "ripple" = '(1, 1)
type instance I.UnitScale "XRP" "drop" = '(1000000, 1)
-- | Monero
type instance I.CurrencyScale "XMR" = I.UnitScale "XMR" "piconero"
type instance I.UnitScale "XMR" "monero" = '(1, 1)
type instance I.UnitScale "XMR" "decinero" = '(10, 1)
type instance I.UnitScale "XMR" "centinero" = '(100, 1)
type instance I.UnitScale "XMR" "millinero" = '(1000, 1)
type instance I.UnitScale "XMR" "micronero" = '(1000000, 1)
type instance I.UnitScale "XMR" "nanonero" = '(1000000000, 1)
type instance I.UnitScale "XMR" "piconero" = '(1000000000000, 1)

