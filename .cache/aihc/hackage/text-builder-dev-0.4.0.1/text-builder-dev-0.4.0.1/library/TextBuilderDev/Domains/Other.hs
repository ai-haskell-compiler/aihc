module TextBuilderDev.Domains.Other where

import TextBuilder
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Prelude hiding (intercalate)

-- | Data size in decimal notation over amount of bytes.
--
-- >>> approximateDataSize 999
-- "999B"
--
-- >>> approximateDataSize 9999
-- "9.9kB"
--
-- >>> approximateDataSize (-9999)
-- "-9.9kB"
--
-- >>> approximateDataSize 1234567890
-- "1.2GB"
--
-- >>> approximateDataSize 10000000000000000000000000000000023
-- "10,000,000,000YB"
{-# INLINEABLE approximateDataSize #-}
approximateDataSize :: (Integral a) => a -> TextBuilder
approximateDataSize = signed \a ->
  if a < 1000
    then decimal a <> "B"
    else
      if a < 1000000
        then dividedDecimal 100 a <> "kB"
        else
          if a < 1000000000
            then dividedDecimal 100000 a <> "MB"
            else
              if a < 1000000000000
                then dividedDecimal 100000000 a <> "GB"
                else
                  if a < 1000000000000000
                    then dividedDecimal 100000000000 a <> "TB"
                    else
                      if a < 1000000000000000000
                        then dividedDecimal 100000000000000 a <> "PB"
                        else
                          if a < 1000000000000000000000
                            then dividedDecimal 100000000000000000 a <> "EB"
                            else
                              if a < 1000000000000000000000000
                                then dividedDecimal 100000000000000000000 a <> "ZB"
                                else dividedDecimal 100000000000000000000000 a <> "YB"
  where
    dividedDecimal divisor n =
      let byDivisor = div n divisor
          byExtraTen = div byDivisor 10
          remainder = byDivisor - byExtraTen * 10
          separatorChar = ','
       in if remainder == 0 || byExtraTen >= 10
            then thousandSeparatedDecimal separatorChar byExtraTen
            else thousandSeparatedDecimal separatorChar byExtraTen <> "." <> decimalDigit remainder

-- | Double with a fixed number of decimal places.
--
-- >>> doubleFixedPoint 4 0.123456
-- "0.1235"
--
-- >>> doubleFixedPoint 2 2.1
-- "2.10"
--
-- >>> doubleFixedPoint (-2) 2.1
-- "2"
--
-- >>> doubleFixedPoint 2 (-2.1)
-- "-2.10"
--
-- >>> doubleFixedPoint 2 0
-- "0.00"
{-# INLINE doubleFixedPoint #-}
doubleFixedPoint ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
doubleFixedPoint (max 0 -> decimalPlaces) =
  fromString . printf ("%." ++ show decimalPlaces ++ "f")

-- | Double multiplied by 100 with a fixed number of decimal places applied and followed by a percent-sign.
--
-- >>> doubleFixedPointPercent 3 0.123456
-- "12.346%"
--
-- >>> doubleFixedPointPercent 0 2
-- "200%"
--
-- >>> doubleFixedPointPercent 0 (-2)
-- "-200%"
{-# INLINE doubleFixedPointPercent #-}
doubleFixedPointPercent ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
doubleFixedPointPercent decimalPlaces x = doubleFixedPoint decimalPlaces (x * 100) <> "%"
