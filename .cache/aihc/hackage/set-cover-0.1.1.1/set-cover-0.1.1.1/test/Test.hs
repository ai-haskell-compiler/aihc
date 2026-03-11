module Main where

import qualified Mastermind.Test as Mastermind
import qualified Test.Knead as TestKnead
import Test.Utility
         (normalizeSolution, equivalentSolutions,
          setAssigns, genWord, genWords,
          InflatedString, genInflatedWords, forAllShrinkSmall)

import qualified Math.SetCover.Exact.UArray as ESC_UArray
import qualified Math.SetCover.Exact.Priority as ESCP
import qualified Math.SetCover.Exact as ESC
import qualified Math.SetCover.Queue as Queue

import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import qualified Data.Map as Map; import Data.Map(Map)
import qualified Data.Set as Set; import Data.Set(Set)

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Monoid.HT as Mn
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Tuple.HT (mapFst, mapSnd)

import qualified Test.QuickCheck as QC


distinct :: (QC.Arbitrary a, Eq a) => QC.Gen [a]
distinct = List.nub <$> QC.arbitrary

singletonAssigns :: [a] -> [ESC.Assign a (Set a)]
singletonAssigns = map (\x -> ESC.assign x (Set.singleton x))

partitionDistinct :: [Char] -> Bool
partitionDistinct xs =
   case ESC.partitions $ singletonAssigns xs of
      [ys] -> equating List.sort xs ys
      _ -> False

partitionMultiplicity :: [Char] -> Bool
partitionMultiplicity xs =
   (Fold.product $ Map.fromListWith (+) $ map (flip (,) 1) xs)
   ==
   (length $ ESC.partitions $ singletonAssigns xs)


sortInt :: [(Int, a)] -> [(Int, a)]
sortInt = List.sortBy (comparing fst)

genShuffled :: QC.Gen ([String], [String])
genShuffled = do
   kxs <- take 10 <$> QC.listOf (liftM2 (,) QC.arbitrary genWord)
   return (snd <$> kxs, snd <$> sortInt kxs)

partitionShuffled :: [String] -> [String] -> Bool
partitionShuffled xs ys =
   equivalentSolutions
      (ESC.partitions $ setAssigns xs)
      (ESC.partitions $ setAssigns ys)


genExtraSet :: QC.Gen (String, [String])
genExtraSet = do
   xs <- genWords
   let es = Fold.foldMap Set.fromList xs
   x <-
      if Set.null es
        then return ""
        else take 5 <$> (QC.listOf $ QC.elements $ Set.toList es)
   return (x,xs)


{- |
This function compares two multi-sets represented by sorted lists.

@
List.sort xs `orderedSublist` List.sort ys
==>
Set.fromList xs `Set.isSubsetOf` Set.fromList ys
@

The converse is not true, because e.g. @xs == "aa", ys == "a"@.
-}
orderedSublist :: (Ord a) => [a] -> [a] -> Bool
orderedSublist xs0 ys0 =
   foldr
      (\y go xt ->
         case xt of
            [] -> True
            x:xs ->
               case compare x y of
                  LT -> False
                  GT -> go xt
                  EQ -> go xs)
      null ys0 xs0

partitionAddSet :: String -> [String] -> Bool
partitionAddSet x xs =
   (normalizeSolution $ ESC.partitions $ setAssigns xs)
   `orderedSublist`
   (normalizeSolution $ ESC.partitions $ setAssigns $ x : xs)


removeOverlap :: (Ord a) => [Set a] -> [Set a]
removeOverlap xs =
   filter (not . Set.null) $ snd $
   List.mapAccumL
      (\uni y -> (Set.difference uni y, Set.intersection y uni))
      (Set.unions xs) xs

removeOverlapList :: (Ord a) => [[a]] -> [[a]]
removeOverlapList = map Set.toList . removeOverlap . map Set.fromList

genPartition :: QC.Gen ([String], [String])
genPartition = do
   xs <- genWords
   return (removeOverlapList xs, xs)

partitionAddPartition :: [String] -> [String] -> Bool
partitionAddPartition ys xs =
   let nys =
         zipWith (\n y -> ESC.assign (Left n) (Set.fromList y)) [(0::Int) ..] ys
   in  elem
         (map ESC.label nys)
         (map List.sort $ ESC.partitions $
            nys ++ map (\x -> ESC.assign (Right x) (Set.fromList x)) xs)


partitionIntegerBitSet :: [InflatedString] -> Bool
partitionIntegerBitSet xs =
   let asns = setAssigns xs
   in  equivalentSolutions
         (ESC.partitions asns)
         (ESC.partitions $ ESC.bitVectorFromSetAssigns asns)

partitionIntSet :: [InflatedString] -> Bool
partitionIntSet xs =
   let asns = setAssigns xs
   in  equivalentSolutions
         (ESC.partitions asns)
         (ESC.partitions $ ESC.intSetFromSetAssigns asns)

partitionUArray :: [InflatedString] -> Bool
partitionUArray xs =
   let asns = setAssigns xs
   in  equivalentSolutions
         (ESC.partitions asns)
         (ESC_UArray.partitions asns)


genPairs :: QC.Gen [[(Char,Char)]]
genPairs =
   fmap (take 10) $ QC.listOf $
   fmap (take 10) $ QC.listOf $
      liftM2 (,) (QC.choose ('A','C')) (QC.choose ('a','c'))

-- | occasionally add non-overlapping sets such that there are solutions
genSolvablePairs :: QC.Gen [[(Char,Char)]]
genSolvablePairs = do
   pairs <- genPairs
   b0 <- QC.arbitrary
   b1 <- QC.arbitrary
   return $
      pairs
       ++ Mn.when b0 (removeOverlapList pairs)
       ++ Mn.when b1 (removeOverlapList $ reverse pairs)

uncurryMap :: (Ord a, Ord b) => [(a, b)] -> Map a (Set b)
uncurryMap = Map.fromListWith Set.union . map (mapSnd Set.singleton)

partitionMap :: [[(Char,Char)]] -> Bool
partitionMap xs =
   equivalentSolutions
      (ESC.partitions $ setAssigns xs)
      (ESC.partitions $ map (\m -> ESC.assign m $ uncurryMap m) xs)


partitionQueueGen ::
   (Ord label, ESC.Set set) =>
   Queue.Methods queue set -> ([a] -> [ESC.Assign label set]) -> [a] -> Bool
partitionQueueGen methods makeAssigns xs =
   let asns = makeAssigns xs
   in  equivalentSolutions
         (ESC.partitions asns)
         (ESCP.partitions methods asns)

partitionQueueSet :: [String] -> Bool
partitionQueueSet = partitionQueueGen ESCP.queueSet setAssigns

mapAssigns ::
   [[(Char, Char)]] -> [ESC.Assign [(Char, Char)] (Map Char (Set Char))]
mapAssigns = map (\m -> ESC.assign m $ uncurryMap m)

partitionQueueMap :: [[(Char,Char)]] -> Bool
partitionQueueMap =
   partitionQueueGen (ESCP.queueMap ESCP.queueSet) mapAssigns

partitionQueueBit :: [String] -> Bool
partitionQueueBit =
   partitionQueueGen ESCP.queueBit (ESC.bitVectorFromSetAssigns . setAssigns)

partitionQueueBitPQ :: [String] -> Bool
partitionQueueBitPQ =
   partitionQueueGen ESCP.queueBitPQ (ESC.bitVectorFromSetAssigns . setAssigns)

partitionQueueIntSet :: [String] -> Bool
partitionQueueIntSet =
   partitionQueueGen ESCP.queueIntSet (ESC.intSetFromSetAssigns . setAssigns)


flattenTreeForward :: ESC.Tree label set -> [[label]]
flattenTreeForward =
   let go ESC.Leaf = [[]]
       go (ESC.Branch _ xs) =
         concatMap (\(label, subTree) -> (label:) <$> go subTree) xs
   in  go

flattenTree :: ESC.Tree label set -> [[label]]
flattenTree =
   let go labels tree =
         case tree of
            ESC.Leaf -> [labels]
            ESC.Branch _ xs -> concatMap (uncurry $ go . (:labels)) xs
   in  go []

partitionTree :: [String] -> Bool
partitionTree xs =
   let asns = setAssigns xs
   in  equivalentSolutions
         (ESC.partitions asns)
         (flattenTree $ ESC.decisionTree asns)


treeQueueGen ::
   (Ord label, ESC.Choose set, Eq set) =>
   Queue.Methods queue set -> ([a] -> [ESC.Assign label set]) -> [a] -> Bool
treeQueueGen methods makeAssigns xs =
   let asns = makeAssigns xs
   in  ESC.decisionTree asns == ESCP.decisionTree methods asns


forAll ::
   (QC.Testable prop, Show a) =>
   QC.Gen a -> (a -> prop) -> (Int, QC.Property)
forAll gen = (,) 1000 . QC.forAll gen

forAllShrink ::
   (QC.Testable prop, Show a, QC.Arbitrary a) =>
   QC.Gen a -> (a -> prop) -> (Int, QC.Property)
forAllShrink gen = (,) 1000 . QC.forAllShrink gen QC.shrink

tests :: [(String, (Int, QC.Property))]
tests =
   map (mapFst ("Exact."++)) $
   ("partitionDistinct",
      forAll (take 10 <$> distinct) partitionDistinct) :
   ("partitionMultiplicity",
      forAllShrink (take 10 <$> QC.arbitrary) partitionMultiplicity) :
   ("partitionShuffled",
      forAll genShuffled $ uncurry partitionShuffled) :
   ("partitionAddSet",
      forAll genExtraSet $ uncurry partitionAddSet) :
   ("partitionAddPartition",
      forAll genPartition $ uncurry partitionAddPartition) :
   ("partitionIntegerBitSet",
      forAllShrinkSmall genInflatedWords partitionIntegerBitSet) :
   ("partitionIntSet",
      forAllShrinkSmall genInflatedWords partitionIntSet) :
   ("partitionUArray",
      forAllShrinkSmall genInflatedWords partitionUArray) :
   TestKnead.tests ++
   ("partitionMap",
      forAll genSolvablePairs partitionMap) :
   ("partitionQueueSet",
      forAllShrink genWords partitionQueueSet) :
   ("partitionQueueMap",
      forAll genSolvablePairs partitionQueueMap) :
   ("partitionQueueBit",
      forAllShrink genWords partitionQueueBit) :
   ("partitionQueueBitPQ",
      forAllShrink genWords partitionQueueBitPQ) :
   ("partitionQueueIntSet",
      forAllShrink genWords partitionQueueIntSet) :
   ("partitionTree",
      forAllShrink genWords partitionTree) :
   ("treeQueueSet",
      forAllShrink genWords $ treeQueueGen ESCP.queueSet setAssigns) :
   ("treeQueueMap",
      forAll genSolvablePairs $
      treeQueueGen (ESCP.queueMap ESCP.queueSet) mapAssigns) :
   ("treeQueueBit",
      forAllShrink genWords $
      treeQueueGen ESCP.queueBit (ESC.bitVectorFromSetAssigns . setAssigns)) :
   ("treeQueueBitPQ",
      forAllShrink genWords $
      treeQueueGen ESCP.queueBitPQ (ESC.bitVectorFromSetAssigns . setAssigns)) :
   ("treeQueueIntSet",
      forAllShrink genWords $
      treeQueueGen ESCP.queueIntSet (ESC.intSetFromSetAssigns . setAssigns)) :
   []


quickCheck :: (Int, QC.Property) -> IO ()
quickCheck (count, prop) =
   QC.quickCheckWith (QC.stdArgs {QC.maxSuccess = count}) prop

main :: IO ()
main =
   mapM_ (\(msg,prop) -> putStr (msg++": ") >> quickCheck prop) $

   tests ++ map (mapFst ("Mastermind."++)) Mastermind.tests
