{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.StrictVG
Copyright   :  (c) Oleksandr Zhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Simplified version of the @phonetic-languages-common@ package.
Uses less dependencies.
-}
module Aftovolio.StrictVG (
    -- * Working with lists
    uniquenessVariants2GNBL,
    uniquenessVariants2GNPBL,
) where

import Aftovolio.PermutationsArr
import qualified Data.Foldable as F
import Data.InsertLeft (InsertLeft (..))
import GHC.Arr
import GHC.Base
import GHC.Num ((-))

uniquenessVariants2GNBL ::
    (Eq a, F.Foldable t, InsertLeft t a, Monoid (t a), Monoid (t (t a))) =>
    -- | The first most common element in the \"whitespace symbols\" structure
    a ->
    -- | The function that is used internally to convert to the @[a]@ so that the function can process further the permutations
    (t a -> [a]) ->
    -- | The function that is used internally to convert to the @[[a]]@ so that the function can process further
    ((t (t a)) -> [[a]]) ->
    -- | The function that is used internally to convert to the needed representation so that the function can process further
    ([a] -> t a) ->
    -- | The permutations of 'Int' indices starting from 0 and up to n (n is probably less than 8).
    [Array Int Int] ->
    -- | Must be obtained as 'subG' @whspss xs@ or in equivalent way
    t (t a) ->
    [t a]
uniquenessVariants2GNBL !hd f1 f2 f3 perms !subs = uniquenessVariants2GNPBL mempty mempty hd f1 f2 f3 perms subs
{-# INLINE uniquenessVariants2GNBL #-}
{-# SPECIALIZE uniquenessVariants2GNBL ::
    Char ->
    (String -> String) ->
    ([String] -> [String]) ->
    (String -> String) ->
    [Array Int Int] ->
    [String] ->
    [String]
    #-}

uniquenessVariants2GNPBL ::
    (Eq a, F.Foldable t, InsertLeft t a, Monoid (t a), Monoid (t (t a))) =>
    t a ->
    t a ->
    -- | The first most common element in the whitespace symbols structure
    a ->
    -- | The function that is used internally to convert to the @[a]@ so that the function can process further the permutations
    (t a -> [a]) ->
    -- | The function that is used internally to convert to the @[[a]]@ so that the function can process further
    ((t (t a)) -> [[a]]) ->
    -- | The function that is used internally to convert to the needed representation that the function can process further
    ([a] -> t a) ->
    -- | The permutations of 'Int' indices starting from 0 and up to n (n is probably less than 8).
    [Array Int Int] ->
    -- | Must be obtained as @subG whspss xs@ or in equivalent way
    t (t a) ->
    [t a]
uniquenessVariants2GNPBL !ts !us !hd f1 f2 f3 perms !subs
    | F.null subs = mempty
    | otherwise = map f3 ns
  where
    !uss = (hd %@ us) %^ mempty
    !base0 = map (hd %@) . f2 $ subs
    !l = F.length base0
    !baseArr = listArray (0, l - 1) base0
    !ns = universalSetGL ts uss f1 f2 perms baseArr -- in map f3 ns
{-# INLINE uniquenessVariants2GNPBL #-}
{-# SPECIALIZE uniquenessVariants2GNPBL ::
    String ->
    String ->
    Char ->
    (String -> String) ->
    ([String] -> [String]) ->
    (String -> String) ->
    [Array Int Int] ->
    [String] ->
    [String]
    #-}
