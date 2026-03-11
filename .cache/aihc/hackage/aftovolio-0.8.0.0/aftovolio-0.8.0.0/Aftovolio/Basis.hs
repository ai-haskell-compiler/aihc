{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.Basis
Copyright   :  (c) OleksandrZhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Simplified version of the @phonetic-languages-common@ and @phonetic-languages-general@ packages.
Uses less dependencies.
-}
module Aftovolio.Basis where

import GHC.Base

data Result t a b c = R {line :: !(t a), propertiesF :: !b, transPropertiesF :: !c}
    deriving (Eq)

instance (Ord (t a), Ord b, Ord c) => Ord (Result t a b c) where
    compare x y =
        case compare (transPropertiesF x) (transPropertiesF y) of
            !EQ -> case compare (propertiesF x) (propertiesF y) of
                !EQ -> compare (line x) (line y)
                !z -> z
            !z0 -> z0
    {-# INLINE compare #-}

data FuncRep2 a b c = D {getAB :: (a -> b), getBC :: (b -> c)}

getAC :: FuncRep2 a b c -> (a -> c)
getAC (D f g) = g . f
{-# INLINE getAC #-}

data Result2 a b c = R2 {line2 :: !a, propertiesF2 :: !b, transPropertiesF2 :: !c}
    deriving (Eq)

instance (Ord a, Ord b, Ord c) => Ord (Result2 a b c) where
    compare x y =
        case compare (transPropertiesF2 x) (transPropertiesF2 y) of
            !EQ -> case compare (propertiesF2 x) (propertiesF2 y) of
                !EQ -> compare (line2 x) (line2 y)
                !z -> z
            !z0 -> z0
    {-# INLINE compare #-}
