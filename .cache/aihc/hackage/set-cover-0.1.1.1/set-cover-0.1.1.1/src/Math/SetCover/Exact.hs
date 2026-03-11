{- |
This module provides a solver for exact set cover problems.
<http://en.wikipedia.org/wiki/Exact_cover>
-}
module Math.SetCover.Exact (
   Assign(..), assign,
   bitVectorFromSetAssigns, intSetFromSetAssigns,
   partitions, search, step,
   State(..), initState, updateState,
   Set(..),
   Tree(..), decisionTree, completeTree,
   Choose(..),
   ) where

import qualified Math.SetCover.BitMap as BitMap
import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import Math.SetCover.EnumMap (constMap)

import Control.Applicative ((<$>), (<$))

import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.Match as Match
import qualified Data.List as List
import qualified Data.Foldable as Fold
import Data.Function.HT (compose2)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (mapFst, mapSnd)
import Data.Bits (setBit)

import Prelude hiding (null)


{- |
This class provides all operations needed for the set cover algorithm.
It allows to use the same algorithm
both for @containers@' 'Set' and for sets represented by bit vectors.
-}
class Set set where
   null :: set -> Bool
   disjoint :: set -> set -> Bool
   unions :: [set] -> set
   difference :: set -> set -> set
   {- |
   @minimize free assigns@ finds a set element @x@ from @free@
   that is contained in the least number of sets in @assigns@.
   Then it returns the assigns where @x@ is contained in the associated set.
   This formulation allows us not to name @x@
   and thus we do not need a second type variable for @set@ elements
   and no type family from @set@ to its element type.

   Unchecked preconditions:
   @free@ must be a superset of all sets in the assign list.
   @free@ must be non-empty.
   The @assigns@ list may be empty.
   The output of assigns must be a subsequence of the input assigns,
   that is, it must be a subset of the input and it must be in the same order.
   This requirement was originally needed by 'minimize' for 'Map.Map',
   but currently it is not utilized anywhere.
   -}
   minimize :: set -> [Assign label set] -> [Assign label set]

class Set set => Choose set where
   {-
   Compute a set containing one element
   that is contained in a minimal number of assignment sets.
   -}
   chooseMinimize :: set -> [Assign label set] -> (set, [Assign label set])

instance (Ord a) => Set (Set.Set a) where
   null = Set.null
   disjoint x y = Set.null $ Set.intersection x y
   unions = Set.unions
   difference = Set.difference
   minimize free = Fold.minimumBy Match.compareLength . histogramSet free

instance (Ord a) => Choose (Set.Set a) where
   chooseMinimize free =
      mapFst Set.singleton .
      List.minimumBy (compose2 Match.compareLength snd) .
      Map.toList . histogramSet free

histogramSet ::
   Ord k =>
   Set.Set k ->
   [Assign label (Set.Set k)] ->
   Map.Map k [Assign label (Set.Set k)]
histogramSet free =
   foldr (Map.unionWith (++)) (constMap [] free) .
   map (\a -> constMap [a] $ labeledSet a)


{- |
This instance supports Maps of Sets.
This way you can structure your sets hierarchically.
You may also use it to combine several low-level bitsets.
A Map must not contain empty subsets.
-}
instance (Ord k, Set set) => Set (Map.Map k set) where
   null = Map.null
   disjoint x y = Fold.and $ Map.intersectionWith disjoint x y
   unions =
      fmap unions . foldr (Map.unionWith (++)) Map.empty . map (fmap (:[]))
   difference =
      Map.differenceWith
         (\x y -> let z = difference x y in toMaybe (not $ null z) z)
   minimize free =
      map label . Fold.minimumBy Match.compareLength .
      Map.intersectionWith minimize free . histogramMap free

instance (Ord k, Choose set) => Choose (Map.Map k set) where
   chooseMinimize free =
      (\(k,(minSet,asns)) -> (Map.singleton k minSet, map label asns)) .
      List.minimumBy (compose2 Match.compareLength (snd.snd)) . Map.toList .
      Map.intersectionWith chooseMinimize free . histogramMap free

histogramMap ::
   (Ord k, Set set) =>
   Map.Map k set ->
   [Assign label (Map.Map k set)] ->
   Map.Map k [Assign (Assign label (Map.Map k set)) set]
histogramMap free =
   foldr (Map.unionWith (++)) ([] <$ free) .
   map (\asn -> (:[]) . assign asn <$> labeledSet asn)


instance (Bit.C a) => Set (BitSet.Set a) where
   null = BitSet.null
   disjoint = BitSet.disjoint
   unions = Fold.fold
   difference = BitSet.difference
   minimize free = snd . chooseMinimize free

instance (Bit.C a) => Choose (BitSet.Set a) where
   chooseMinimize free available =
      let singleMin =
             BitSet.keepMinimum $ BitMap.minimumSet free $
             Fold.foldMap (BitMap.fromSet . labeledSet) available
      in  (singleMin,
           filter (not . BitSet.disjoint singleMin . labeledSet) available)


instance Set IntSet.IntSet where
   null = IntSet.null
   disjoint x y = IntSet.null $ IntSet.intersection x y
   unions = IntSet.unions
   difference = IntSet.difference
   minimize free = snd . chooseMinimize free

instance Choose IntSet.IntSet where
   chooseMinimize free available =
      let singleMin =
            IntSet.findMin $ BitSet.getBits $
            BitMap.minimumSet (BitSet.Set free) $
            Fold.foldMap (BitMap.fromSet . BitSet.Set . labeledSet) available
      in  (IntSet.singleton singleMin,
           filter (IntSet.member singleMin . labeledSet) available)


{- |
'Assign' allows to associate a set with a label.
If a particular set is chosen for a set cover,
then its label is included in the output of 'partitions'.

I have decided to separate sets and labels this way,
since it is the easiest way to assign a meaning to a set.
If you really want to know the sets in a partition,
then you can fill the 'label' field with the set.
-}
data Assign label set =
   Assign {
      label :: label,
      labeledSet :: set
   }

{- |
Construction of a labeled set.
-}
assign :: label -> set -> Assign label set
assign = Assign


{- |
You may use this to post-process a set of 'Assign's
in order to speedup the solver considerably.
You must process the whole set of 'Assign's at once,
i.e. do not process only parts of the assignment list.
The output of 'bitVectorFromSetAssigns' should go into the solver as is.
-}
bitVectorFromSetAssigns ::
   (Ord a) =>
   [Assign label (Set.Set a)] -> [Assign label (BitSet.Set Integer)]
bitVectorFromSetAssigns asns =
   let bitVec = Fold.foldl' setBit 0 . mapIntFromSet asns
   in  map (fmap (BitSet.Set . bitVec)) asns

{- |
Like 'bitVectorFromSetAssigns' but generates 'IntSet.IntSet'
instead of 'Integer' bitvectors.
Since containers-0.5.5 as shipped with GHC-7.8.4,
'IntSet.IntSet' should usually be more efficient than 'Integer'.
-}
intSetFromSetAssigns ::
   (Ord a) => [Assign label (Set.Set a)] -> [Assign label IntSet.IntSet]
intSetFromSetAssigns asns =
   let intSet = IntSet.fromList . Map.elems . mapIntFromSet asns
   in  map (fmap intSet) asns

mapIntFromSet ::
   (Ord a) => [Assign label (Set.Set a)] -> Set.Set a -> Map.Map a Int
mapIntFromSet asns =
   let mapToInt =
         Map.fromList $ zip (Set.toList $ unions $ map labeledSet asns) [0..]
   in  Map.intersection mapToInt . constMap ()

{- |
The state of the search.
@usedSubsets@ contains the partial partition built up so far.
@availableSubsets@ is the list of sets we can still try to put into a partition.
The lists @usedSubsets@ and @availableSubsets@ are disjoint,
but their union is not necessarily equal to the list of initially given sets.
There are sets not contained in the partial partition
that overlap with the partial partition.
Those sets are not available for extending the partition.

@freeElements@ contains the elements that are not covered
by the partial partition in @usedSubsets@.
@unions usedSubset@ and @freeElements@ are disjoint
and their union is the set of all elements.
-}
data State label set =
   State {
      availableSubsets :: [Assign label set],
      freeElements :: set,
      usedSubsets :: [label]
   }

instance Functor (Assign label) where
   fmap f (Assign lab set) = Assign lab (f set)

instance Functor (State label) where
   fmap f (State ab fp pb) =
      State (map (fmap f) ab) (f fp) pb

initState :: Set set => [Assign label set] -> State label set
initState subsets =
   State {
      availableSubsets = subsets,
      freeElements = unions $ map labeledSet subsets,
      usedSubsets = []
   }

{-# INLINE updateState #-}
updateState :: Set set => Assign label set -> State label set -> State label set
updateState (Assign attemptLabel attemptedSet) s =
   State {
      availableSubsets =
         filter (disjoint attemptedSet . labeledSet) $
         availableSubsets s,
      freeElements = difference (freeElements s) attemptedSet,
      usedSubsets = attemptLabel : usedSubsets s
   }


{- |
This is the key of the search algorithm.
The search algorithm tries to build partitions
by adding sets to a partition list successively.
A step starts on a partial partition
and looks for new sets that could be added.
The goal is to avoid to check a set again down in a search branch
and to quickly determine search directions that lead to a dead end.
To this end a search step selects a certain set element
and tries all sets that contain that element
and that do not overlap with the partial partition.
Practically, 'step' selects an element with the minimal number
of non-overlapping sets it is contained in.
If this number is zero, then the search can be aborted in this branch.

Most oftenly the power of the algorithm
originates from the formulation of a problem as a set-cover problem
and from the equal treatment of all elements.
E.g. in the Soma cube example
the algorithm chooses whether to do a case analysis on all bricks
that cover a certain position,
or to do a case analysis on all positions that are possible for a certain brick.

The algorithm might not be extraordinarily fast,
but in all cases it consumes only little memory
since it only has to maintain the current state of search.

Precondition: 'freeElements' of the input state must not be empty.
-}
{-# INLINE step #-}
step :: Set set => State label set -> [State label set]
step s =
   map (flip updateState s) $ minimize (freeElements s) (availableSubsets s)

{- |
Start the search for partitions on a certain search state.
This can be an 'initState' or the result of performing some search 'step's.
In the examples we use this for parallelization:
We perform some steps manually
and then run 'search' on the results in parallel.
-}
{-# INLINE search #-}
search :: Set set => State label set -> [[label]]
search s =
   if null (freeElements s)
     then [usedSubsets s]
     else step s >>= search

{- |
@partitions [assign '0' set0, assign '1' set1, assign '2' set2]@
computes @unions [set0, set1, set2]@ and tries to partition the union set
using the sets @set0@, @set1@, @set2@.
'partitions' returns all such partitions.
If a set is chosen for a partition,
then its label is included in the output.
E.g. @set0 = Set.fromList [0,1], set1 = Set.fromList [2], set2 = Set.fromList [0,1,2]@,
then 'partitions' returns @[\"01\", \"2\"]@.

The order of partitions and the order of labels
depends on the implementation
and you must not rely on them.

You may use 'Data.Maybe.listToMaybe' in order to select only the first solution.
-}
{-# INLINE partitions #-}
partitions :: Set set => [Assign label set] -> [[label]]
partitions = search . initState



data Tree label set = Leaf | Branch set [(label, Tree label set)]
   deriving (Eq)

completeTree :: Choose set => State label set -> Tree label set
completeTree s =
   if null (freeElements s)
      then Leaf
      else
         uncurry Branch $
         mapSnd (map (\asn -> (label asn, completeTree $ updateState asn s))) $
         chooseMinimize (freeElements s) (availableSubsets s)

decisionTree :: Choose set => [Assign label set] -> Tree label set
decisionTree = completeTree . initState
