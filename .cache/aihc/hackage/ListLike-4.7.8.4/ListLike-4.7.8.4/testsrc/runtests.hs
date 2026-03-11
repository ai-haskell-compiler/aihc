{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -Wno-missing-signatures #-}


{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck
import qualified Data.ListLike as LL
--import qualified Data.Foldable as F
--import System.Random
import qualified Test.HUnit as HU
import System.IO
--import Text.Printf
--import Data.Word
import Data.List
import Data.Monoid
import TestInfrastructure
import Data.Foldable(foldr', fold, foldMap)
import System.Exit
import System.Info


-- prop_singleton :: (Eq i,LL.ListLike f i) => f -> i -> Bool
--prop_singleton :: (Eq i, LL.ListLike f i, Arbitrary f, Show f, Show i, Arbitrary i) => f -> i -> Bool
prop_singleton f x = (LL.toList $ asTypeOf (LL.singleton x) f) == [x]

prop_empty f = (LL.toList l == []) && (LL.null l) && (LL.length l == 0)
    where l = asTypeOf LL.empty f

prop_tofromlist f =
    LL.toList f == l &&
    LL.length f == length l &&
    f == (LL.fromList . LL.toList $ f)
    where l = LL.toList f

prop_length f = LL.length f == length (LL.toList f)
prop_cons f i = llcmp (LL.cons i f) (i : (LL.toList f))
prop_append f1 f2 = llcmp (LL.append f1 f2) (LL.toList f1 ++ LL.toList f2)
prop_head f = not (LL.null f) ==> LL.head f == head (LL.toList f)
  where
    head (x:xs) = x
    head []     = undefined
  -- Andreas Abel, 2023-10-10, issue #32:
  -- Redefine 'head' and 'tail' to avoid the x-partial warning of GHC 9.8.
prop_tail f = not (LL.null f) ==> llcmp (LL.tail f) (tail (LL.toList f))
  where
    tail (x:xs) = xs
    tail []     = undefined
prop_init f = not (LL.null f) ==> llcmp (LL.init f) (init (LL.toList f))
prop_last f = not (LL.null f) ==> LL.last f == last (LL.toList f)
prop_null f = LL.null f == null (LL.toList f)
prop_length2 f = checkLengths f (LL.toList f)
prop_length3 f1 f2 = llcmp (LL.append f1 f2) (LL.toList f1 ++ LL.toList f2)

prop_map :: forall full item. (TestLL full item, TestLL [item] item) => full -> (item -> item) -> Property
prop_map f func = llcmp llmap (map func (LL.toList f))
    where llmap = asTypeOf (LL.map func f) (LL.toList f)

prop_rigidMap f func = llcmp (LL.rigidMap func f) (map func (LL.toList f))
prop_reverse f = llcmp (LL.reverse f) (reverse (LL.toList f))
prop_intersperse f i = llcmp (LL.intersperse i f) (intersperse i (LL.toList f))

prop_concat f =
    llcmp (LL.concat f) (concat $ map LL.toList (LL.toList f))

prop_concatmap :: forall full item. (TestLL full item, TestLL [item] item) => full -> (item -> [item]) -> Property
prop_concatmap f func =
    llcmp (LL.concatMap func f)
          (concatMap func (LL.toList f))

prop_rigidConcatMap f func =
    llcmp (LL.rigidConcatMap func f)
          (concatMap (LL.toList . func) (LL.toList f))

prop_any f func = (LL.any func f) == (any func (LL.toList f))
prop_all f func = (LL.all func f) == (all func (LL.toList f))
prop_maximum f = not (LL.null f) ==> LL.maximum f == maximum (LL.toList f)
prop_minimum f = not (LL.null f) ==> LL.minimum f == minimum (LL.toList f)
prop_replicate f count i = count <= 1000 ==> llcmp res (replicate count i)
    where res = asTypeOf (LL.replicate count i) f
prop_take f count = llcmp (LL.take count f) (take count (LL.toList f))
prop_drop f count = count >= 0 ==> llcmp (LL.drop count f) (drop count (LL.toList f))
prop_splitAt f count = count >= 0 ==>
    llcmp [(\(x, y) -> (LL.toList x, LL.toList y)) . LL.splitAt count $ f]
          [LL.splitAt count (LL.toList f)]
prop_takeWhile f func = llcmp (LL.takeWhile func f)
                              (takeWhile func (LL.toList f))
prop_dropWhile f func = llcmp (LL.dropWhile func f)
                              (dropWhile func (LL.toList f))
prop_dropWhileEnd f func = llcmp (LL.dropWhileEnd func f)
                                 (dropWhileEnd func (LL.toList f))
prop_span f func =
    llcmp [(\(x, y) -> (LL.toList x, LL.toList y)) . LL.span func $ f]
          [span func (LL.toList f)]
prop_break f func =
    llcmp [(\(x, y) -> (LL.toList x, LL.toList y)) . LL.break func $ f]
          [break func (LL.toList f)]
prop_group f =
    -- llcmp (map LL.toList (LL.group f)) (group (LL.toList f))
    (map LL.toList (LL.group f)) == (group (LL.toList f))
prop_inits f = (map LL.toList (LL.inits f)) == (inits (LL.toList f))
prop_tails f = (map LL.toList (LL.tails f)) == (tails (LL.toList f))
prop_isPrefixOf f1 f2 = LL.isPrefixOf f1 f2 ==
    (isPrefixOf (LL.toList f1) (LL.toList f2))
prop_isSuffixOf f1 f2 = LL.isSuffixOf f1 f2 ==
    (isSuffixOf (LL.toList f1) (LL.toList f2))
prop_isInfixOf f1 f2 = LL.isInfixOf f1 f2 ==
    (isInfixOf (LL.toList f1) (LL.toList f2))
prop_stripPrefix f1 f2 = (LL.toList <$> LL.stripPrefix f1 f2) ==
    (stripPrefix (LL.toList f1) (LL.toList f2))
prop_stripPrefix2 f1 f2 = (LL.toList <$> LL.stripPrefix f1 (f1 <> f2)) ==
    (stripPrefix (LL.toList f1) (LL.toList $ f1 <> f2))
prop_stripSuffix f1 f2 = LL.stripSuffix f1 (f2 <> f1) == Just f2
prop_elem f i = LL.elem i f == elem i (LL.toList f)
prop_notElem f i = LL.notElem i f == notElem i (LL.toList f)
prop_find f func = LL.find func f == find func (LL.toList f)
prop_filter f func = llcmp (LL.filter func f) (filter func (LL.toList f))
prop_partition f func =
    (LL.toList f1, LL.toList f2) == partition func (LL.toList f)
    where (f1, f2) = LL.partition func f
prop_index f i = (i >= 0 && i < LL.length f) ==>
    (LL.index f i == ((LL.toList f) !! i))
prop_elemIndex f i = LL.elemIndex i f == elemIndex i (LL.toList f)
prop_elemIndices f i = LL.elemIndices i f == elemIndices i (LL.toList f)
prop_findIndex f func = LL.findIndex func f == findIndex func (LL.toList f)
prop_findIndices f func =
    LL.findIndices func f == findIndices func (LL.toList f)

prop_sequence f =
    case (llres, sequence testit) of
         (Just ll, Just l) -> llcmp ll l
         _ -> error "Error!"
    where testit = map Just (LL.toList f)
          llres = asTypeOf (LL.sequence testit) (Just f)

prop_mapM :: forall full item. (TestLL full item, TestLL [item] item) => full -> (item -> Maybe item) -> Bool
prop_mapM f func = llmapM == (mapM func (LL.toList f))
    where llmapM = asTypeOf (LL.mapM func f) (Just (LL.toList f))

prop_rigidMapM :: forall full item. (TestLL full item, TestLL [item] item) => full -> (item -> Maybe item) -> Property
prop_rigidMapM f func =
    case (LL.rigidMapM func f, mapM func (LL.toList f)) of
         (Just ll, Just l)  -> llcmp ll l
         (Nothing, Nothing) -> property True
         e -> error $ "error in prop_rigidMapM: " ++ show e

-- FIXME: can we test mapM_?

prop_nub f = llcmp (LL.nub f) (nub (LL.toList f))
prop_delete f i = llcmp (LL.delete i f) (delete i (LL.toList f))
prop_deleteFirsts f1 f2 = llcmp (LL.deleteFirsts f1 f2)
    ((LL.toList f1) \\ (LL.toList f2))
prop_union f1 f2 = llcmp (LL.union f1 f2)
    (union (LL.toList f1) (LL.toList f2))
prop_intersect f1 f2 = llcmp (LL.intersect f1 f2)
    (intersect (LL.toList f1) (LL.toList f2))
prop_sort f1 = llcmp (LL.sort f1) (sort (LL.toList f1))
prop_insert f i = llcmp (LL.insert i f) (insert i (LL.toList f))
prop_nubBy f func = llcmp (LL.nubBy func f) (nubBy func (LL.toList f))
prop_deleteBy f func i = llcmp (LL.deleteBy func i f)
                         (deleteBy func i (LL.toList f))
prop_deleteFirstsBy f1 f2 func = llcmp (LL.deleteFirstsBy func f1 f2)
    (deleteFirstsBy func (LL.toList f1) (LL.toList f2))
prop_unionBy f1 f2 func = llcmp (LL.unionBy func f1 f2)
    (unionBy func (LL.toList f1) (LL.toList f2))
prop_intersectBy f1 f2 func = llcmp (LL.intersectBy func f1 f2)
    (intersectBy func (LL.toList f1) (LL.toList f2))
prop_groupBy f func =
    (map LL.toList (LL.groupBy func f)) == (groupBy func (LL.toList f))
prop_sortBy1 f = llcmp (LL.sortBy compare f) (sortBy compare (LL.toList f))
prop_sortBy2 f = llcmp (LL.sortBy func f) (sortBy func (LL.toList f))
    where func x y = compare y x
prop_sortBy f func = llcmp (LL.sortBy func f) (sortBy func (LL.toList f))
prop_insertBy1 f i = llcmp (LL.insertBy compare i f)
    (insertBy compare i (LL.toList f))
prop_insertBy2 f i = llcmp (LL.insertBy func i f)
    (insertBy func i (LL.toList f))
    where func x y = compare y x
prop_genericLength f =
    LL.genericLength f == genericLength (LL.toList f)
prop_genericTake f (i::Integer) = (i >= 0) ==>
    llcmp (LL.genericTake i f) (genericTake i (LL.toList f))
prop_genericDrop f (i::Integer) = (i >= 0) ==>
    llcmp (LL.genericDrop i f) (genericDrop i (LL.toList f))
prop_genericSplitAt f (i::Integer) = i >= 0 ==>
    llcmp [(\(x, y) -> (LL.toList x, LL.toList y)) . LL.genericSplitAt i $ f]
          [LL.genericSplitAt i (LL.toList f)]
prop_genericReplicate f (count::Integer) i = count >= 0 ==>
    llcmp res (genericReplicate count i)
    where res = asTypeOf (LL.genericReplicate count i) f

--prop_zip :: (LL.ListLike full item, LL.ListLike result (item, Int)) =>
--    full -> Result
prop_zip f = LL.zip f f2 == zip (LL.toList f) f2
    where f2 = [(-5::Int)..]
prop_zipWith f =
    LL.toList res == (zipWith func (LL.toList f) f2)
    where f2 = [(100::Int)..(-100)]
          func x y = (y + 5, x)
          res = asTypeOf (LL.zipWith func f f2) [(5::Int, LL.head f)]
--FIXME: prop_unzip
--FIXME: prop_and
--FIXME: prop_or
--FIXME: prop_sum
--FIXME: prop_product
prop_foldl f func (i::Int) = LL.foldl func i f == foldl func i (LL.toList f)
prop_foldl' f func (i::Integer) =
    LL.foldl' func i f == foldl' func i (LL.toList f)
prop_foldl1 f func = not (LL.null f) ==>
    (LL.foldl1 func f) == (foldl1 func (LL.toList f))
prop_foldr f func (i::Int) = LL.foldr func i f == foldr func i (LL.toList f)
prop_foldr' f func (i::Integer) =
    LL.foldr' func i f == foldr' func i (LL.toList f)
prop_foldr1 f func = not (LL.null f) ==>
    LL.foldl1 func f == foldl1 func (LL.toList f)
prop_fold f = llcmp res resl
    where res = LL.fold f
          resl = fold (map LL.toList (LL.toList f))
prop_foldMap :: (LL.ListLike full item, Eq full) => full -> (item -> [Int]) -> Bool
prop_foldMap f func = res == resl
    where res = LL.foldMap func f
          resl = foldMap func  (LL.toList f) -- asTypeOf (foldMap (LL.toList f)) (head f)

prop_toString f =
    ((LL.fromString . LL.toString $ f) == f)
    where l = LL.toList f
prop_fromString f x =
    LL.toString (asTypeOf (LL.fromString x) f) == x
prop_lines f = map LL.toString res == lines (LL.toString f)
    where res = asTypeOf (LL.lines f) [f]
prop_words f = map LL.toString res == words (LL.toString f)
    where res = asTypeOf (LL.words f) [f]

allt = [apf "empty" (t prop_empty),
        apf "length" (t prop_length),
        apf "to/fromList" (t prop_tofromlist),
        apf "singleton" (t prop_singleton),
        apf "cons" (t prop_cons),
        apf "append" (t prop_append),
        apf "head" (t prop_head),
        apf "last" (t prop_last),
        apf "tail" (t prop_tail),
        apf "init" (t prop_init),
        apf "null" (t prop_null),
        apf "length2" (t prop_length2),
        apf "length3" (t prop_length3),
        apf "map" (t prop_map),
        apf "rigidMap" (t prop_rigidMap),
        apf "reverse" (t prop_reverse),
        apf "intersperse" (t prop_intersperse),
        apw "concat" (LLWrap prop_concat),
        apf "concatMap" (t prop_concatmap),
        apf "rigidConcatMap" (t prop_rigidConcatMap),
        apf "any" (t prop_any),
        apf "all" (t prop_all),
        apf "maximum" (t prop_maximum),
        apf "minimum" (t prop_minimum),
        apf "replicate" (t prop_replicate),
        apf "take" (t prop_take),
        apf "drop" (t prop_drop),
        apf "splitAt" (t prop_splitAt),
        apf "takeWhile" (t prop_takeWhile),
        apf "dropWhile" (t prop_dropWhile),
        apf "dropWhileEnd" (t prop_dropWhileEnd),
        apf "span" (t prop_span),
        apf "break" (t prop_break),
        apf "group" (t prop_group),
        apf "inits" (t prop_inits),
        apf "tails" (t prop_tails),
        apf "isPrefixOf" (t prop_isPrefixOf),
        apf "isSuffixOf" (t prop_isSuffixOf),
        apf "isInfixOf" (t prop_isInfixOf),
        apf "stripPrefix" (t prop_stripPrefix),
        apf "stripPrefix2" (t prop_stripPrefix2),
        apf "stripSuffix" (t prop_stripSuffix),
        apf "elem" (t prop_elem),
        apf "notElem" (t prop_notElem),
        apf "find" (t prop_find),
        apf "filter" (t prop_filter),
        apf "partition" (t prop_partition),
        apf "index" (t prop_index),
        apf "elemIndex" (t prop_elemIndex),
        apf "elemIndices" (t prop_elemIndices),
        apf "findIndex" (t prop_findIndex),
        apf "findIndices" (t prop_findIndices),
        apf "sequence" (t prop_sequence),
        apf "mapM" (t prop_mapM),
        apf "rigidMapM" (t prop_rigidMapM),
        -- FIXME: mapM_ ?
        apf "nub" (t prop_nub),
        apf "delete" (t prop_delete),
        apf "deleteFirsts" (t prop_deleteFirsts),
        apf "union" (t prop_union),
        apf "intersect" (t prop_intersect),
        apf "sort" (t prop_sort),
        apf "insert" (t prop_insert),
        -- toList
        -- fromList
        -- fromListLike
        apf "nubBy" (t prop_nubBy),
        apf "deleteBy" (t prop_deleteBy),
        apf "deleteFirstsBy" (t prop_deleteFirstsBy),
        apf "unionBy" (t prop_unionBy),
        apf "intersectBy" (t prop_intersectBy),
        apf "groupBy" (t prop_groupBy),
        apf "sortBy1" (t prop_sortBy1),
        apf "sortBy2" (t prop_sortBy2),
        apf "insertBy1" (t prop_insertBy1),
        apf "insertBy2" (t prop_insertBy2),
        apf "genericLength" (t prop_genericLength),
        apf "genericTake" (t prop_genericTake),
        apf "genericDrop" (t prop_genericDrop),
        apf "genericSplitAt" (t prop_genericSplitAt),
        apf "genericReplicate" (t prop_genericReplicate),
        apf "zip" (t prop_zip),
        apf "zipWith" (t prop_zipWith)
        -- apf "unzip" (t prop_unzip),
        -- apf "and" (t prop_and),
        -- apf "or" (t prop_or),
        -- apf "sum" (t prop_sum),
        -- apf "propduct" (t prop_product),
        -- sequence_
        ]

allf = (if compilerName == "hugs" then [] else [ apf "foldl" (t prop_foldl),
                                                 apf "foldr1" (t prop_foldr1),
                                                 apf "foldl1" (t prop_foldl1)])
       ++
       [
        apf "foldl'" (t prop_foldl'),
        apf "foldr" (t prop_foldr),
        apf "foldr'" (t prop_foldr'),
        apw "fold" (LLWrap prop_fold),
        apf "foldMap" (t prop_foldMap)
       ]

alls = [
        aps "toString" (t prop_toString),
        aps "fromString" (t prop_fromString),
        aps "lines" (t prop_lines),
        aps "words" (t prop_words)
        -- FIXME: aps (t prop_unlines),
        -- FIXME: aps (t prop_unwords)
       ]
allTests = HU.TestList $ reverse $
                       [HU.TestLabel "ListLike" (HU.TestList allt),
                        HU.TestLabel "FoldableLL" (HU.TestList allf),
                        HU.TestLabel "StringLike" (HU.TestList alls)]

testh = HU.runTestTT $ allTests
testv = runVerbTestText (HU.putTextToHandle stderr True) $ allTests

main = do
  (counts, n) <- testv
  case HU.errors counts + HU.failures counts of
    0 -> return ()
    n -> do
      putStrLn ("counts=" ++ show counts ++ " n=" ++ show n)
      exitWith (ExitFailure 1)
