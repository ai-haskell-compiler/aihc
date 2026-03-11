{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Aftovolio.PermutationsRepresent
Copyright   :  (c) OleksandrZhabenko 2022-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Permutations data type to mark the needed permutations type from the other modules.
-}
module Aftovolio.PermutationsRepresent (
    PermutationsType (..),
    bTransform2Perms,
    permChoose
) where

import GHC.Base
import Text.Show
import Aftovolio.PermutationsArr
import Aftovolio.PermutationsArrMini1
import Aftovolio.PermutationsArrMini2
import Aftovolio.PermutationsArrMini
import GHC.Arr 

data PermutationsType = P Int deriving (Eq, Ord)

instance Show PermutationsType where
    show (P x) = "+P " `mappend` show x

bTransform2Perms :: [String] -> PermutationsType
bTransform2Perms ys
    | ys == ["1"] = P 1
    | ys == ["2"] = P 2
    | ys == ["3"] = P 3
    | otherwise = P 0

permChoose :: PermutationsType -> Int -> [Array Int Int]
permChoose permType
    | permType == P 0 = genPermutationsL
    | permType == P 1 = genElementaryPermutationsLN1
    | permType == P 2 = genPairwisePermutationsLN
    | permType == P 3 = genDoublePermutationsLN2

