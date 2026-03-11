{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Text.Format.Numbers
    ( PrettyCfg(..)
    , prettyF
    , prettyI
    )
where

import Data.Monoid
import qualified Data.Text as T

data PrettyCfg
    = PrettyCfg
    { pc_decimals :: !Int
      -- ^ Decimals to display
    , pc_thousandsSep :: !(Maybe Char)
      -- ^ Optional thousands separator
    , pc_decimalSep :: !Char
      -- ^ Decimal separator
    }

-- | Pretty print any number given a configuration
prettyF :: RealFrac i => PrettyCfg -> i -> T.Text
prettyF PrettyCfg{..} n =
  let tpow = 10 ^ pc_decimals
      lshift = n * fromIntegral tpow
      lshiftr = round lshift
      lshifti' = abs lshiftr
      intPart = lshifti' `div` tpow
      decPart = lshifti' - intPart * tpow
      preDecimal =
          if lshiftr < 0
          then prettyI pc_thousandsSep (intPart * (-1))
          else prettyI pc_thousandsSep intPart
      postDecimal =
          if pc_decimals > 0
          then T.cons pc_decimalSep (T.justifyRight pc_decimals '0' $ T.pack $ show decPart)
          else ""
  in preDecimal <> postDecimal

-- | Pretty print an 'Int' given an optional thousands separator
prettyI :: Maybe Char -> Int -> T.Text
prettyI tsep n =
  let ni = T.pack $ show $ abs n
      nis =
          case tsep of
            Just s ->
                T.intercalate (T.singleton s) $
                reverse $ map T.reverse $ T.chunksOf 3 $ T.reverse ni
            Nothing ->
                ni
  in if n < 0 then "-" <> nis else nis
