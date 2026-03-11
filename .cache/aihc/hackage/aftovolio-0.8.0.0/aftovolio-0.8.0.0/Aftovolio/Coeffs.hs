{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.Coeffs
Copyright   :  (c) OleksandrZhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

The coefficients functionality common for both phonetic-languages-simplified-examples-array and phonetic-languages-simplified-generalized-examples-array lines of AFTOVolio.
-}
module Aftovolio.Coeffs (
    -- * Newtype to work with
    CoeffTwo (..),
    Coeffs2,
    isEmpty,
    isPair,
    fstCF,
    sndCF,
    readCF,
) where

import Data.Maybe (fromJust, fromMaybe, isNothing)
import GHC.Base
import GHC.List
import Text.Read (readMaybe)

data CoeffTwo a = CF0 | CF2 (Maybe a) (Maybe a) deriving (Eq)

isEmpty :: CoeffTwo a -> Bool
isEmpty CF0 = True
isEmpty _ = False

isPair :: CoeffTwo a -> Bool
isPair CF0 = False
isPair _ = True

fstCF :: CoeffTwo a -> Maybe a
fstCF (CF2 x _) = x
fstCF _ = Nothing

sndCF :: CoeffTwo a -> Maybe a
sndCF (CF2 _ y) = y
sndCF _ = Nothing

readCF :: String -> Coeffs2
readCF xs
    | any (== '_') xs =
        let (!ys, !zs) =
                (\(ks, ts) -> (readMaybe ks :: Maybe Double, readMaybe (drop 1 ts) :: Maybe Double))
                    . break (== '_') $
                    xs
         in if (isNothing ys && isNothing zs) then CF0 else CF2 ys zs
    | otherwise = CF0

-- | A data type that is used to represent the coefficients of the rhythmicity functions as a one argument value.
type Coeffs2 = CoeffTwo Double
