{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.DataG
Copyright   :  (c) Oleksandr Zhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Simplified version of the @phonetic-languages-common@ and @phonetic-languages-general@ packages.
Uses less dependencies.
-}
module Aftovolio.DataG where

import Aftovolio.Basis
import qualified Data.Foldable as F
import Data.InsertLeft (InsertLeft (..), mapG, partitionG)
import Data.Maybe (fromJust)
import Data.MinMax1
import GHC.Base
import GHC.Num ((-))
import GHC.Real

maximumEl ::
    (F.Foldable t2, Ord c) =>
    FuncRep2 (t a) b c ->
    t2 (t a) ->
    Result t a b c
maximumEl !frep2 data0 =
    let !l = F.maximumBy (\x y -> compare (getAC frep2 x) (getAC frep2 y)) data0
        !m = getAB frep2 l
        !tm = getBC frep2 m
     in R{line = l, propertiesF = m, transPropertiesF = tm}
{-# INLINE maximumEl #-}
{-# SPECIALIZE maximumEl ::
    (Ord c) => FuncRep2 [a] Double c -> [[a]] -> Result [] a Double c
    #-}

-- | Is intended to be used for the structures with at least two elements, though it is not checked.
minMaximumEls ::
    (InsertLeft t2 (t a), Monoid (t2 (t a)), Ord (t a), Ord c) =>
    FuncRep2 (t a) b c ->
    t2 (t a) ->
    (Result t a b c, Result t a b c)
minMaximumEls !frep2 data0 =
    let (!ln, !lx) =
            fromJust . minMax11By (\x y -> compare (getAC frep2 x) (getAC frep2 y)) $ data0
        !mn = getAB frep2 ln
        !mx = getAB frep2 lx
        !tmn = getBC frep2 mn
        !tmx = getBC frep2 mx
     in ( R{line = ln, propertiesF = mn, transPropertiesF = tmn}
        , R{line = lx, propertiesF = mx, transPropertiesF = tmx}
        )
{-# INLINE minMaximumEls #-}
{-# SPECIALIZE minMaximumEls ::
    (Ord a, Ord c) =>
    FuncRep2 [a] Double c ->
    [[a]] ->
    (Result [] a Double c, Result [] a Double c)
    #-}

maximumElR ::
    (F.Foldable t2, Ord c) =>
    t2 (Result t a b c) ->
    Result t a b c
maximumElR = F.maximumBy (\x y -> compare (transPropertiesF x) (transPropertiesF y))
{-# INLINE maximumElR #-}
{-# SPECIALIZE maximumElR ::
    (Ord c) => [Result [] a Double c] -> Result [] a Double c
    #-}

-- | Is intended to be used for the structures with at least two elements, though it is not checked.
minMaximumElRs ::
    ( InsertLeft t2 (Result t a b c)
    , Monoid (t2 (Result t a b c))
    , Ord (t a)
    , Ord b
    , Ord c
    ) =>
    t2 (Result t a b c) ->
    (Result t a b c, Result t a b c)
minMaximumElRs =
    fromJust
        . minMax11By (\x y -> compare (transPropertiesF x) (transPropertiesF y))
{-# INLINE minMaximumElRs #-}
{-# SPECIALIZE minMaximumElRs ::
    (Ord a, Ord c) =>
    [Result [] a Double c] ->
    (Result [] a Double c, Result [] a Double c)
    #-}

-----------------------------------------------------------------------------------

-- | The second argument must be not empty for the function to work correctly.
innerPartitioning ::
    (InsertLeft t2 (t a), Monoid (t2 (t a)), InsertLeft t2 c, Monoid (t2 c), Ord c) =>
    FuncRep2 (t a) b c ->
    t2 (t a) ->
    (t2 (t a), t2 (t a))
innerPartitioning !frep2 data0 =
    let !l = F.maximum . mapG (toTransPropertiesF' frep2) $ data0
     in partitionG ((== l) . getAC frep2) data0
{-# INLINE innerPartitioning #-}
{-# SPECIALIZE innerPartitioning ::
    (Eq a, Ord c) => FuncRep2 [a] Double c -> [[a]] -> ([[a]], [[a]])
    #-}

-- | The first argument must be not empty for the function to work correctly.
innerPartitioningR ::
    ( InsertLeft t2 (Result t a b c)
    , Monoid (t2 (Result t a b c))
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Ord c
    ) =>
    t2 (Result t a b c) ->
    (t2 (Result t a b c), t2 (Result t a b c))
innerPartitioningR dataR =
    let !l = F.maximum . mapG transPropertiesF $ dataR
     in partitionG ((== l) . transPropertiesF) dataR
{-# INLINE innerPartitioningR #-}
{-# SPECIALIZE innerPartitioningR ::
    (Eq a, Ord c) =>
    [Result [] a Double c] ->
    ([Result [] a Double c], [Result [] a Double c])
    #-}

maximumGroupsClassification ::
    ( InsertLeft t2 (t a)
    , Monoid (t2 (t a))
    , Ord c
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Integral d
    ) =>
    d ->
    FuncRep2 (t a) b c ->
    (t2 (t a), t2 (t a)) ->
    (t2 (t a), t2 (t a))
maximumGroupsClassification !nGroups !frep2 (dataT, dataF)
    | F.null dataF = (dataT, mempty)
    | nGroups <= 0 = (dataT, dataF)
    | otherwise =
        maximumGroupsClassification (nGroups - 1) frep2 (dataT `mappend` partT, partF)
  where
    (!partT, !partF) = innerPartitioning frep2 dataF
{-# NOINLINE maximumGroupsClassification #-}

maximumGroupsClassification1 ::
    ( InsertLeft t2 (t a)
    , Monoid (t2 (t a))
    , Ord c
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Integral d
    ) =>
    d ->
    FuncRep2 (t a) b c ->
    t2 (t a) ->
    (t2 (t a), t2 (t a))
maximumGroupsClassification1 !nGroups !frep2 data0
    | F.null data0 = (mempty, mempty)
    | nGroups <= 0 = innerPartitioning frep2 data0
    | otherwise =
        maximumGroupsClassification (nGroups - 1) frep2 . innerPartitioning frep2 $
            data0
{-# NOINLINE maximumGroupsClassification1 #-}

maximumGroupsClassificationR2 ::
    ( Eq a
    , Eq b
    , Eq (t a)
    , InsertLeft t2 (Result t a b c)
    , Monoid (t2 (Result t a b c))
    , Ord c
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Integral d
    ) =>
    d ->
    (t2 (Result t a b c), t2 (Result t a b c)) ->
    (t2 (Result t a b c), t2 (Result t a b c))
maximumGroupsClassificationR2 !nGroups (dataT, dataF)
    | F.null dataF = (dataT, mempty)
    | nGroups <= 0 = (dataT, dataF)
    | otherwise =
        maximumGroupsClassificationR2 (nGroups - 1) (dataT `mappend` partT, partF)
  where
    (!partT, !partF) = innerPartitioningR dataF
{-# NOINLINE maximumGroupsClassificationR2 #-}

maximumGroupsClassificationR ::
    ( Eq a
    , Eq b
    , Eq (t a)
    , InsertLeft t2 (Result t a b c)
    , Monoid (t2 (Result t a b c))
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Ord c
    , Integral d
    ) =>
    d ->
    t2 (Result t a b c) ->
    (t2 (Result t a b c), t2 (Result t a b c))
maximumGroupsClassificationR !nGroups dataR
    | F.null dataR = (mempty, mempty)
    | nGroups <= 0 = innerPartitioningR dataR
    | otherwise =
        maximumGroupsClassificationR2 (nGroups - 1) . innerPartitioningR $ dataR
{-# NOINLINE maximumGroupsClassificationR #-}

toResultR ::
    FuncRep2 (t a) b c ->
    t a ->
    Result t a b c
toResultR !frep2 !ys = R{line = ys, propertiesF = m, transPropertiesF = tm}
  where
    !m = getAB frep2 ys
    !tm = getBC frep2 m
{-# INLINE toResultR #-}

toPropertiesF' ::
    FuncRep2 (t a) b c ->
    t a ->
    b
toPropertiesF' !frep2 !ys = getAB frep2 ys
{-# INLINE toPropertiesF' #-}

toTransPropertiesF' ::
    FuncRep2 (t a) b c ->
    t a ->
    c
toTransPropertiesF' !frep2 !ys = getAC frep2 ys
{-# INLINE toTransPropertiesF' #-}

-- | The second argument must be not empty for the function to work correctly.
partiR ::
    (InsertLeft t2 (Result t a b c), Monoid (t2 (Result t a b c)), InsertLeft t2 c) =>
    (c -> Bool) ->
    t2 (Result t a b c) ->
    (t2 (Result t a b c), t2 (Result t a b c))
partiR p dataR = partitionG (p . transPropertiesF) dataR
{-# INLINE partiR #-}
{-# SPECIALIZE partiR ::
    (Eq a, Eq c) =>
    (c -> Bool) ->
    [Result [] a Double c] ->
    ([Result [] a Double c], [Result [] a Double c])
    #-}

-----------------------------------------------------------

maximumEl2 ::
    (F.Foldable t2, Ord c) =>
    FuncRep2 a b c ->
    t2 a ->
    Result2 a b c
maximumEl2 !frep2 data0 =
    let !l = F.maximumBy (\x y -> compare (getAC frep2 x) (getAC frep2 y)) data0
        !m = getAB frep2 l
        !tm = getBC frep2 m
     in R2{line2 = l, propertiesF2 = m, transPropertiesF2 = tm}
{-# INLINE maximumEl2 #-}
{-# SPECIALIZE maximumEl2 ::
    (Ord c) => FuncRep2 a Double c -> [a] -> Result2 a Double c
    #-}

-- | Is intended to be used with the structures with at least two elements, though it is not checked.
minMaximumEls2 ::
    (InsertLeft t2 a, Monoid (t2 a), Ord a, Ord c) =>
    FuncRep2 a b c ->
    t2 a ->
    (Result2 a b c, Result2 a b c)
minMaximumEls2 !frep2 data0 =
    let (!ln, !lx) =
            fromJust . minMax11By (\x y -> compare (getAC frep2 x) (getAC frep2 y)) $ data0
        !mn = getAB frep2 ln
        !mx = getAB frep2 lx
        !tmn = getBC frep2 mn
        !tmx = getBC frep2 mx
     in ( R2{line2 = ln, propertiesF2 = mn, transPropertiesF2 = tmn}
        , R2{line2 = lx, propertiesF2 = mx, transPropertiesF2 = tmx}
        )
{-# INLINE minMaximumEls2 #-}
{-# SPECIALIZE minMaximumEls2 ::
    (Ord a, Ord c) =>
    FuncRep2 a Double c ->
    [a] ->
    (Result2 a Double c, Result2 a Double c)
    #-}

maximumElR2 ::
    (F.Foldable t2, Ord c) =>
    t2 (Result2 a b c) ->
    Result2 a b c
maximumElR2 = F.maximumBy (\x y -> compare (transPropertiesF2 x) (transPropertiesF2 y))
{-# INLINE maximumElR2 #-}
{-# SPECIALIZE maximumElR2 ::
    (Ord c) => [Result2 a Double c] -> Result2 a Double c
    #-}

-- | Is intended to be used with the structures with at least two elements, though it is not checked.
minMaximumElRs2 ::
    ( InsertLeft t2 (Result2 a b c)
    , Monoid (t2 (Result2 a b c))
    , Ord a
    , Ord b
    , Ord c
    ) =>
    t2 (Result2 a b c) ->
    (Result2 a b c, Result2 a b c)
minMaximumElRs2 =
    fromJust
        . minMax11By (\x y -> compare (transPropertiesF2 x) (transPropertiesF2 y))
{-# INLINE minMaximumElRs2 #-}
{-# SPECIALIZE minMaximumElRs2 ::
    (Ord a, Ord c) =>
    [Result2 a Double c] ->
    (Result2 a Double c, Result2 a Double c)
    #-}

-----------------------------------------------------------------------------------

-- | The second argument must be not empty for the function to work correctly.
innerPartitioning2 ::
    (InsertLeft t2 a, Monoid (t2 a), InsertLeft t2 c, Monoid (t2 c), Ord c) =>
    FuncRep2 a b c ->
    t2 a ->
    (t2 a, t2 a)
innerPartitioning2 !frep2 data0 =
    let !l = F.maximum . mapG (toTransPropertiesF'2 frep2) $ data0
     in partitionG ((== l) . getAC frep2) data0
{-# INLINE innerPartitioning2 #-}
{-# SPECIALIZE innerPartitioning2 ::
    (Eq a, Ord c) => FuncRep2 a Double c -> [a] -> ([a], [a])
    #-}

-- | The first argument must be not empty for the function to work correctly.
innerPartitioningR2 ::
    ( InsertLeft t2 (Result2 a b c)
    , Monoid (t2 (Result2 a b c))
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Ord c
    ) =>
    t2 (Result2 a b c) ->
    (t2 (Result2 a b c), t2 (Result2 a b c))
innerPartitioningR2 dataR =
    let !l = F.maximum . mapG transPropertiesF2 $ dataR
     in partitionG ((== l) . transPropertiesF2) dataR
{-# INLINE innerPartitioningR2 #-}
{-# SPECIALIZE innerPartitioningR2 ::
    (Eq a, Ord c) =>
    [Result2 a Double c] ->
    ([Result2 a Double c], [Result2 a Double c])
    #-}

maximumGroupsClassification2 ::
    ( InsertLeft t2 a
    , Monoid (t2 a)
    , Ord c
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Integral d
    ) =>
    d ->
    FuncRep2 a b c ->
    (t2 a, t2 a) ->
    (t2 a, t2 a)
maximumGroupsClassification2 !nGroups !frep2 (dataT, dataF)
    | F.null dataF = (dataT, mempty)
    | nGroups <= 0 = (dataT, dataF)
    | otherwise =
        maximumGroupsClassification2 (nGroups - 1) frep2 (dataT `mappend` partT, partF)
  where
    (!partT, !partF) = innerPartitioning2 frep2 dataF
{-# NOINLINE maximumGroupsClassification2 #-}

maximumGroupsClassification12 ::
    ( InsertLeft t2 a
    , Monoid (t2 a)
    , Ord c
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Integral d
    ) =>
    d ->
    FuncRep2 a b c ->
    t2 a ->
    (t2 a, t2 a)
maximumGroupsClassification12 !nGroups !frep2 data0
    | F.null data0 = (mempty, mempty)
    | nGroups <= 0 = innerPartitioning2 frep2 data0
    | otherwise =
        maximumGroupsClassification2 (nGroups - 1) frep2 . innerPartitioning2 frep2 $
            data0
{-# NOINLINE maximumGroupsClassification12 #-}

maximumGroupsClassificationR2_2 ::
    ( Eq a
    , Eq b
    , InsertLeft t2 (Result2 a b c)
    , Monoid (t2 (Result2 a b c))
    , Ord c
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Integral d
    ) =>
    d ->
    (t2 (Result2 a b c), t2 (Result2 a b c)) ->
    (t2 (Result2 a b c), t2 (Result2 a b c))
maximumGroupsClassificationR2_2 !nGroups (dataT, dataF)
    | F.null dataF = (dataT, mempty)
    | nGroups <= 0 = (dataT, dataF)
    | otherwise =
        maximumGroupsClassificationR2_2 (nGroups - 1) (dataT `mappend` partT, partF)
  where
    (!partT, !partF) = innerPartitioningR2 dataF
{-# NOINLINE maximumGroupsClassificationR2_2 #-}

maximumGroupsClassificationR_2 ::
    ( Eq a
    , Eq b
    , InsertLeft t2 (Result2 a b c)
    , Monoid (t2 (Result2 a b c))
    , InsertLeft t2 c
    , Monoid (t2 c)
    , Ord c
    , Integral d
    ) =>
    d ->
    t2 (Result2 a b c) ->
    (t2 (Result2 a b c), t2 (Result2 a b c))
maximumGroupsClassificationR_2 !nGroups dataR
    | F.null dataR = (mempty, mempty)
    | nGroups <= 0 = innerPartitioningR2 dataR
    | otherwise =
        maximumGroupsClassificationR2_2 (nGroups - 1) . innerPartitioningR2 $ dataR
{-# NOINLINE maximumGroupsClassificationR_2 #-}

toResultR2 ::
    FuncRep2 a b c ->
    a ->
    Result2 a b c
toResultR2 !frep2 !y = R2{line2 = y, propertiesF2 = m, transPropertiesF2 = tm}
  where
    !m = getAB frep2 y
    !tm = getBC frep2 m
{-# INLINE toResultR2 #-}

toPropertiesF'2 ::
    FuncRep2 a b c ->
    a ->
    b
toPropertiesF'2 !frep2 !y = getAB frep2 y
{-# INLINE toPropertiesF'2 #-}

toTransPropertiesF'2 ::
    FuncRep2 a b c ->
    a ->
    c
toTransPropertiesF'2 !frep2 !y = getAC frep2 y
{-# INLINE toTransPropertiesF'2 #-}

-- | The second argument must be not empty for the function to work correctly.
partiR2 ::
    (InsertLeft t2 (Result2 a b c), Monoid (t2 (Result2 a b c)), InsertLeft t2 c) =>
    (c -> Bool) ->
    t2 (Result2 a b c) ->
    (t2 (Result2 a b c), t2 (Result2 a b c))
partiR2 p dataR = partitionG (p . transPropertiesF2) dataR
{-# INLINE partiR2 #-}
{-# SPECIALIZE partiR2 ::
    (Eq a, Eq c) =>
    (c -> Bool) ->
    [Result2 a Double c] ->
    ([Result2 a Double c], [Result2 a Double c])
    #-}
