{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  GraphMatch
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012, 2016, 2018, 2020, 2022, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  FlexibleInstances, MultiParamTypeClasses
--
--  This module contains graph-matching logic.
--
--  The algorithm used is derived from a paper on RDF graph matching
--  by Jeremy Carroll <http://www.hpl.hp.com/techreports/2001/HPL-2001-293.html>.
--
--------------------------------------------------------------------------------

module Swish.GraphMatch
      ( graphMatch,
        -- * Exported for testing
        LabelMap, GenLabelMap(..), LabelEntry, GenLabelEntry(..),
        ScopedLabel(..), makeScopedLabel, makeScopedArc,
        LabelIndex, EquivalenceClass, nullLabelVal, emptyMap,
        labelIsVar, labelHash,
        mapLabelIndex, setLabelHash, newLabelMap,
        graphLabels, assignLabelMap, newGenerationMap,
        graphMatch1, graphMatch2, equivalenceClasses, reclassify
      ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Exception.Base (assert)
import Control.Arrow (second)

import Data.Foldable (Foldable(..))
import Data.Function (on)
import Data.Hashable (hashWithSalt)
import Data.List (sortBy, groupBy, partition)
import Data.Ord (comparing)
import Data.Word

-- Avoid messages added in GHC 9.10 about foldl' import from Data.List
-- being redundant.
import Prelude hiding (Foldable(..))

import Swish.GraphClass (Arc(..), ArcSet, Label(..))
import Swish.GraphClass (getComponents, arcLabels, hasLabel, arcToTriple)

--------------------------
--  Label index value type
--------------------------
--

-- | LabelIndex is a unique value assigned to each label, such that
--  labels with different values are definitely different values
--  in the graph;  e.g. do not map to each other in the graph
--  bijection.  The first member is a generation counter that
--  ensures new values are distinct from earlier passes.

type LabelIndex = (Word32, Word32)

-- | The null, or empty, index value.
nullLabelVal :: LabelIndex
nullLabelVal = (0, 0)

-----------------------
--  Label mapping types
-----------------------

-- | A Mapping between a label and a value (e.g. an index
-- value).
data (Label lb) => GenLabelEntry lb lv = LabelEntry lb lv

-- | A label associated with a 'LabelIndex'
type LabelEntry lb = GenLabelEntry lb LabelIndex

instance (Label lb, Show lv) => Show (GenLabelEntry lb lv) where
    show (LabelEntry k v) = show k ++ ":" ++ show v

instance (Label lb, Eq lv) => Eq (GenLabelEntry lb lv) where
    (LabelEntry k1 v1) == (LabelEntry k2 v2) = (k1,v1) == (k2,v2)

instance (Label lb, Ord lv) => Ord (GenLabelEntry lb lv) where
    (LabelEntry lb1 lv1) `compare` (LabelEntry lb2 lv2) =
        (lb1, lv1) `compare` (lb2, lv2)

-- | Type for label->index lookup table
data (Label lb, Eq lv, Show lv) => GenLabelMap lb lv =
    LabelMap Word32 (M.Map lb lv)

-- | A label lookup table specialized to 'LabelIndex' indices.
type LabelMap lb = GenLabelMap lb LabelIndex

instance (Label lb) => Show (LabelMap lb) where
    show = showLabelMap

instance (Label lb) => Eq (LabelMap lb) where
    LabelMap gen1 lmap1 == LabelMap gen2 lmap2 =
      (gen1, lmap1) == (gen2, lmap2)

-- | The empty label map table.
emptyMap :: (Label lb) => LabelMap lb
emptyMap = LabelMap 1 M.empty

--------------------------
--  Equivalence class type
--------------------------
--

-- | Type for equivalence class description
--  (An equivalence class is a collection of labels with
--  the same 'LabelIndex' value.)

type EquivalenceClass lb = (LabelIndex, [lb])

{-
ecIndex :: EquivalenceClass lb -> LabelIndex
ecIndex = fst
-}

ecLabels :: EquivalenceClass lb -> [lb]
ecLabels = snd

{-
ecSize :: EquivalenceClass lb -> Int
ecSize = length . ecLabels
-}

ecRemoveLabel :: (Label lb) => EquivalenceClass lb -> lb -> EquivalenceClass lb
ecRemoveLabel xs l = second (L.delete l) xs

------------------------------------------------------------
--  Filter, ungroup, sort and group pairs by first member
------------------------------------------------------------

{-
pairSelect :: ((a,b) -> Bool) -> ((a,b) -> c) -> [(a,b)] -> [c]
pairSelect p f as = map f (filter p as)
-}

-- | Ungroup the pairs.
pairUngroup :: 
    (a,[b])    -- ^ Given (a,bs)
    -> [(a,b)] -- ^ Returns (a,b) for all b in bs
pairUngroup (a,bs) = [ (a,b) | b <- bs ]

-- | Order the pairs based on the first argument.
pairSort :: (Ord a) => [(a,b)] -> [(a,b)]
pairSort = sortBy (comparing fst)

-- TODO: use set on input

-- | Group the pairs based on the first argument.
pairGroup :: (Ord a) => [(a,b)] -> [(a,[b])]
pairGroup = map (factor . unzip) . groupBy eqFirst . pairSort 
    where
      -- as is not [] by construction, but would be nice to have
      -- this enforced by the types
      factor ([], _) = error "internal error"
      factor (a:_, bs) = (a, bs)
      eqFirst = (==) `on` fst

------------------------------------------------------------
--  Augmented graph label value - for graph matching
------------------------------------------------------------
--
-- | This instance of class label adds a graph identifier to
--  each variable label, so that variable labels from
--  different graphs are always seen as distinct values.
--
--  The essential logic added by this class instance is embodied
--  in the eq and hash functions.  Note that variable label hashes
--  depend only on the graph in which they appear, and non-variable
--  label hashes depend only on the variable.  Label hash values are
--  used when initializing a label equivalence-class map (and, for
--  non-variable labels, also for resolving hash collisions).

data (Label lb) => ScopedLabel lb = ScopedLabel Int lb

-- | Create a scoped label given an identifier and label.
makeScopedLabel :: (Label lb) => Int -> lb -> ScopedLabel lb
makeScopedLabel = ScopedLabel 

-- | Create an arc containining a scoped label with the given identifier.
makeScopedArc :: (Label lb) => Int -> Arc lb -> Arc (ScopedLabel lb)
makeScopedArc scope = fmap (ScopedLabel scope)

instance (Label lb) => Label (ScopedLabel lb) where
    getLocal  lab    = error $ "getLocal for ScopedLabel: " ++ show lab
    makeLabel locnam = error $ "makeLabel for ScopedLabel: " ++ locnam
    labelIsVar (ScopedLabel _ lab)   = labelIsVar lab
    labelHash seed (ScopedLabel scope lab)
        | labelIsVar lab    = seed `hashWithSalt` scope
        | otherwise         = labelHash seed lab

instance (Label lb) => Eq (ScopedLabel lb) where
    (ScopedLabel s1 l1) == (ScopedLabel s2 l2)
        = l1 == l2 && s1 == s2

instance (Label lb) => Show (ScopedLabel lb) where
    show (ScopedLabel s1 l1) = show s1 ++ ":" ++ show l1

instance (Label lb) => Ord (ScopedLabel lb) where
    compare (ScopedLabel s1 l1) (ScopedLabel s2 l2) =
        case compare s1 s2 of
            LT -> LT
            EQ -> compare l1 l2
            GT -> GT

-- QUS: why doesn't this return Maybe (LabelMap (ScopedLabel lb)) ?

-- TODO: Should this use Set (Arc lb) instead of [Arc lb]?

-- | Graph matching function accepting two lists of arcs and
--  returning a node map if successful
--
graphMatch :: (Label lb) =>
    (lb -> lb -> Bool)
    -- ^ a function that tests for additional constraints
    --   that may prevent the matching of a supplied pair
    --   of nodes.  Returns `True` if the supplied nodes may be
    --   matched.  (Used in RDF graph matching for checking
    --   that formula assignments are compatible.)
    -> ArcSet lb -- ^ the first graph to be compared
    -> ArcSet lb -- ^ the second graph to be compared
    -> (Bool, LabelMap (ScopedLabel lb))
    -- ^ If the first element is `True` then the second element maps each label
    --   to an equivalence class identifier, otherwise it is just
    --   `emptyMap`.
    --
graphMatch matchable gs1 gs2 =
    let
        sgs1    = {- trace "sgs1 " $ -} S.map (makeScopedArc 1) gs1
        sgs2    = {- trace "sgs2 " $ -} S.map (makeScopedArc 2) gs2
        ls1     = {- traceShow "ls1 " $ -} graphLabels sgs1
        ls2     = {- traceShow "ls2 " $ -} graphLabels sgs2
        lmap    = {- traceShow "lmap " $ -}
                  newGenerationMap $
                  assignLabelMap ls1 $
                  assignLabelMap ls2 emptyMap
        ec1     = {- traceShow "ec1 " $ -} equivalenceClasses lmap ls1
        ec2     = {- traceShow "ec2 " $ -} equivalenceClasses lmap ls2
        ecpairs = zip (pairSort ec1) (pairSort ec2)
        matchableScoped (ScopedLabel _ l1) (ScopedLabel _ l2) = matchable l1 l2
        match   = graphMatch1 False matchableScoped sgs1 sgs2 lmap ecpairs
    in
        if length ec1 /= length ec2 then (False,emptyMap) else match

--  TODO:
--
--    * replace Equivalence class pair by @(index,[lb],[lb])@ ?
--
--    * possible optimization:  the @graphMapEq@ test should be
--      needed only if `graphMatch2` has been used to guess a
--      mapping;  either: 
--          a) supply flag saying guess has been used, or
--          b) move test to `graphMatch2` and use different
--             test to prevent rechecking for each guess used.
--

-- | Recursive graph matching function
--
--  This function assumes that no variable label appears in both graphs.
--  (Function `graphMatch`, which calls this, ensures that all variable
--  labels are distinct.)
--

graphMatch1 :: 
  (Label lb) 
  => Bool
  -- ^ `True` if a guess has been used before trying this comparison,
  --   `False` if nodes are being matched without any guesswork
  -> (lb -> lb -> Bool)
  -- ^ Test for additional constraints that may prevent the matching
  --  of a supplied pair of nodes.  Returns `True` if the supplied
  --  nodes may be matched.
  -> ArcSet lb 
  -- ^ (@gs1@ argument)
  --   first of two lists of arcs (triples) to be compared
  -> ArcSet lb
  -- ^ (@gs2@ argument)
  --   secind of two lists of arcs (triples) to be compared
  -> LabelMap lb
  -- ^ the map so far used to map label values to equivalence class
  --   values
  -> [(EquivalenceClass lb,EquivalenceClass lb)]
  -- ^ (the @ecpairs@ argument) list of pairs of corresponding
  --   equivalence classes of nodes from @gs1@ and @gs2@ that have not
  --   been confirmed in 1:1 correspondence with each other.  Each
  --   pair of equivalence classes contains nodes that must be placed
  --   in 1:1 correspondence with each other.
  --
  -> (Bool,LabelMap lb)
  -- ^ the pair @(match, map)@ where @match@ is @True@ if the supplied
  --   sets of arcs can be matched, in which case @map@ is a
  --   corresponding map from labels to equivalence class identifiers.
  --   When @match@ is @False@, @map@ is the most detailed equivalence
  --   class map obtained before a mismatch was detected or a guess
  --   was required -- this is intended to help identify where the
  --   graph mismatch may be.
graphMatch1 guessed matchable gs1 gs2 lmap ecpairs =
    let
        (secs,mecs) = partition uniqueEc ecpairs
        uniqueEc ( (_,[_])  , (_,[_])  ) = True
        uniqueEc (  _       ,  _       ) = False
        
        doMatch  ( (_,[l1]) , (_,[l2]) ) = labelMatch matchable lmap l1 l2
        doMatch  x = error $ "doMatch failue: " ++ show x -- keep -Wall happy

        ecEqSize ( (_,ls1)  , (_,ls2)  ) = length ls1 == length ls2
        eSize    ( (_,ls1)  , _        ) = length ls1
        ecCompareSize = comparing eSize
        (lmap',mecs',newEc,matchEc) = reclassify gs1 gs2 lmap mecs
        match2 = graphMatch2 matchable gs1 gs2 lmap $ sortBy ecCompareSize mecs
    in
        -- trace ("graphMatch1\nsingle ECs:\n"++show secs++
        --                   "\nmultiple ECs:\n"++show mecs++
        --                   "\n\n") $
        --  if mismatch in singleton equivalence classes, fail
        if not $ all doMatch secs then (False,lmap)
        else
        --  if no multi-member equivalence classes,
        --  check and return label map supplied
        -- trace ("graphMatch1\ngraphMapEq: "++show (graphMapEq lmap gs1 gs2)) $
        if null mecs then (graphMapEq lmap gs1 gs2,lmap)
        else
        --  if size mismatch in equivalence classes, fail
        -- trace ("graphMatch1\nall ecEqSize mecs: "++show (all ecEqSize mecs)) $
        
          --  invoke reclassification, and deal with result
          if not (all ecEqSize mecs) || not matchEc
            then (False, lmap)
            else if newEc
                   then graphMatch1 guessed matchable gs1 gs2 lmap' mecs'
                        --  if guess does not result in a match, return supplied label map
                   else if fst match2 then match2 else (False, lmap)

{-
          if not $ all ecEqSize mecs then (False,lmap)
        else
        if not matchEc then (False,lmap)
        else
        if newEc then graphMatch1 guessed matchable gs1 gs2 lmap' mecs'
        else
        if fst match2 then match2 else (False,lmap)
-}

-- | Auxiliary graph matching function
--
--  This function is called when deterministic decomposition of node
--  mapping equivalence classes has run its course.
--
--  It picks a pair of equivalence classes in ecpairs, and arbitrarily matches
--  pairs of nodes in those equivalence classes, recursively calling the
--  graph matching function until a suitable node mapping is discovered
--  (success), or until all such pairs have been tried (failure).
--
--  This function represents a point to which arbitrary choices are backtracked.
--  The list comprehension 'glp' represents the alternative choices at the
--  point of backtracking
--
--  The selected pair of nodes are placed in a new equivalence class based on their
--  original equivalence class value, but with a new NodeVal generation number.

graphMatch2 :: (Label lb) => (lb -> lb -> Bool)
    -> ArcSet lb
    -> ArcSet lb
    -> LabelMap lb 
    -> [(EquivalenceClass lb,EquivalenceClass lb)]
    -> (Bool,LabelMap lb)
graphMatch2 _         _   _   _    [] = error "graphMatch2 sent an empty list" -- To keep -Wall happy
graphMatch2 matchable gs1 gs2 lmap ((ec1@(ev1,ls1),ec2@(ev2,ls2)):ecpairs) =
    let
        v1 = snd ev1
        --  Return any equivalence-mapping obtained by matching a pair
        --  of labels in the supplied list, or Nothing.
        try []            = (False,lmap)
        try ((l1,l2):lps) = if isEquiv try1 l1 l2 then try1 else try lps
            where
                try1     = graphMatch1 True matchable gs1 gs2 lmap' ecpairs'
                lmap'    = newLabelMap lmap [(l1,v1),(l2,v1)]
                ecpairs' = ((ev',[l1]),(ev',[l2])):ec':ecpairs
                ev'      = mapLabelIndex lmap' l1
                ec'      = (ecRemoveLabel ec1 l1, ecRemoveLabel ec2 l2)
                -- [[[TODO: replace this: if isJust try ?]]]
                isEquiv (False,_)   _  _  = False
                isEquiv (True,lm) x1 x2 =
                    mapLabelIndex m1 x1 == mapLabelIndex m2 x2
                    where
                        m1 = remapLabels gs1 lm [x1]
                        m2 = remapLabels gs2 lm [x2]
        --  glp is a list of label-pair candidates for matching,
        --  selected from the first label-equivalence class.
        --  NOTE:  final test is call of external matchable function
        glp = [ (l1,l2) | l1 <- ls1 , l2 <- ls2 , matchable l1 l2 ]
    in
        assert (ev1 == ev2) -- "GraphMatch2: Equivalence class value mismatch" $
        $ try glp

-- this was in Swish.Utils.MiscHelpers along with a simple hash-based function
-- based on Sedgewick, Algorithms in C, p233. As we have now moved to using
-- Data.Hashable it is not clear whether this is still necessary or sensible.
--
hashModulus :: Int
hashModulus = 16000001

-- | Returns a string representation  of a LabelMap value
--
showLabelMap :: (Label lb) => LabelMap lb -> String
showLabelMap (LabelMap gn lmap) =
    "LabelMap gen=" ++ Prelude.show gn ++ ", map=" ++
    foldl' (++) "" (map (("\n    " ++) . Prelude.show) es)
    where
        es = M.toList lmap

-- | Map a label to its corresponding label index value in the
--   supplied LabelMap.
--
mapLabelIndex :: (Label lb) => LabelMap lb -> lb -> LabelIndex
mapLabelIndex (LabelMap _ lxms) lb = M.findWithDefault nullLabelVal lb lxms

-- | Confirm that a given pair of labels are matchable, and are
--  mapped to the same value by the supplied label map
--
labelMatch :: (Label lb)
    =>  (lb -> lb -> Bool) -> LabelMap lb -> lb -> lb -> Bool
labelMatch matchable lmap l1 l2 =
    matchable l1 l2 && (mapLabelIndex lmap l1 == mapLabelIndex lmap l2)

-- | Replace selected values in a label map with new values from the supplied
--  list of labels and new label index values.  The generation number is
--  supplied from the current label map.  The generation number in the
--  resulting label map is incremented.
--
newLabelMap :: (Label lb) => LabelMap lb -> [(lb, Word32)] -> LabelMap lb
newLabelMap lmap []       = newGenerationMap lmap
newLabelMap lmap (lv:lvs) = setLabelHash (newLabelMap lmap lvs) lv

-- | Replace a label and its associated value in a label map
--  with a new value using the supplied hash value and the current
--  `LabelMap` generation number.  If the key is not found, then no change
--  is made to the label map.

setLabelHash :: (Label lb)
    => LabelMap lb -> (lb, Word32) -> LabelMap lb
setLabelHash  (LabelMap g lmap) (lb,lh) =
    LabelMap g $ M.insert lb (g,lh) lmap

-- | Increment the generation of the label map.
--
--  Returns a new label map identical to the supplied value
--  but with an incremented generation number.
--
newGenerationMap :: (Label lb) => LabelMap lb -> LabelMap lb
newGenerationMap (LabelMap g lvs) = LabelMap (g + 1) lvs

-- | Scan label list, assigning initial label map values,
--  adding new values to the label map supplied.
--
--  Label map values are assigned on the basis of the
--  label alone, without regard for it's connectivity in
--  the graph.  (cf. `reclassify`).
--
--  All variable node labels are assigned the same initial
--  value, as they may be matched with each other.
--
assignLabelMap :: (Label lb) => S.Set lb -> LabelMap lb -> LabelMap lb
assignLabelMap ns lmap = S.foldl' (flip assignLabelMap1) lmap ns

assignLabelMap1 :: (Label lb) => lb -> LabelMap lb -> LabelMap lb
assignLabelMap1 lab (LabelMap g lvs) = 
    LabelMap g $ M.insertWith (const id) lab (g, initVal lab) lvs

--  Calculate initial value for a node

initVal :: (Label lb) => lb -> Word32
initVal = fromIntegral . hashVal 0

hashVal :: (Label lb) => Word32 -> lb -> Int
hashVal seed lab =
  if labelIsVar lab then 23 `hashWithSalt` seed else labelHash (fromIntegral seed) lab

-- | Return the equivalence classes of the supplied nodes 
-- using the label map.
equivalenceClasses :: 
  (Label lb) 
  => LabelMap lb     -- ^ label map
  -> S.Set lb        -- ^ nodes to be reclassified
  -> [EquivalenceClass lb]
equivalenceClasses lmap ls =
    pairGroup $ S.toList $ S.map labelPair ls
    where
        labelPair l = (mapLabelIndex lmap l,l)

-- | Reclassify labels
--
--  Examines the supplied label equivalence classes (based on the supplied
--  label map), and evaluates new equivalence subclasses based on node
--  values and adjacency (for variable nodes) and rehashing
--  (for non-variable nodes).
--
--  Note, assumes that all all equivalence classes supplied are
--  non-singletons;  i.e. contain more than one label.
--
reclassify :: 
  (Label lb) 
  => ArcSet lb 
  -- ^ (the @gs1@ argument) the first of two sets of arcs to perform a
  --   basis for reclassifying the labels in the first equivalence
  --   class in each pair of @ecpairs@.
  -> ArcSet lb
  -- ^ (the @gs2@ argument) the second of two sets of arcs to perform a
  --   basis for reclassifying the labels in the second equivalence
  --   class in each pair of the @ecpairs@ argument
  -> LabelMap lb 
  -- ^ the label map used for classification of the labels in
  --   the supplied equivalence classes
  -> [(EquivalenceClass lb,EquivalenceClass lb)]
  -- ^ (the @ecpairs@ argument) a list of pairs of corresponding equivalence classes of
  --   nodes from @gs1@ and @gs2@ that have not been confirmed
  --   in 1:1 correspondence with each other.
  -> (LabelMap lb,[(EquivalenceClass lb,EquivalenceClass lb)],Bool,Bool)
  -- ^ The output tuple consists of:
  --
  --  1) a revised label map reflecting the reclassification
  --
  --  2) a new list of equivalence class pairs based on the
  --   new node map
  --
  --  3) if the reclassification partitions any of the
  --     supplied equivalence classes then `True`, else `False`
  --
  --  4) if reclassification results in each equivalence class
  --     being split same-sized equivalence classes in the two graphs,
  --     then `True`, otherwise `False`.

reclassify gs1 gs2 lmap@(LabelMap _ lm) ecpairs =
    assert (gen1 == gen2) -- "Label map generation mismatch"
      (LabelMap gen1 lm',ecpairs',newPart,matchPart)
    where
        LabelMap gen1 lm1 =
            remapLabels gs1 lmap $ foldl1 (++) $ map (ecLabels . fst) ecpairs
        LabelMap gen2 lm2 =
            remapLabels gs2 lmap $ foldl1 (++) $ map (ecLabels . snd) ecpairs

        lm' = classifyCombine lm $ M.union lm1 lm2
        
        tmap f (a,b) = (f a, f b)
        
        -- ecGroups :: [([EquivalenceClass lb],[EquivalenceClass lb])]
        ecGroups  = map (tmap remapEc) ecpairs
        ecpairs'  = concatMap (uncurry zip) ecGroups
        newPart   = any pairG1 lenGroups
        matchPart = all pairEq lenGroups
        lenGroups = map (tmap length) ecGroups
        pairEq = uncurry (==)
        pairG1 (p1,p2) = p1 > 1 || p2 > 1
        remapEc = pairGroup . map (newIndex lm') . pairUngroup 
        newIndex x (_,lab) = (M.findWithDefault nullLabelVal lab x,lab)

-- Replace the values in lm1 with those from lm2, but do not copy over new
-- keys from lm2
classifyCombine :: (Ord a) => M.Map a b -> M.Map a b -> M.Map a b
classifyCombine = M.mergeWithKey (\_ _ v -> Just v) id (const M.empty)

-- | Calculate a new index value for a supplied set of labels based on the
--  supplied label map and adjacency calculations in the supplied graph
--
remapLabels :: 
  (Label lb) 
  => ArcSet lb -- ^ arcs used for adjacency calculations when remapping
  -> LabelMap lb -- ^ the current label index values
  -> [lb] -- ^ the graph labels for which new mappings are to be created
  -> LabelMap lb
  -- ^ the updated label map containing recalculated label index values
  -- for the given graph labels. The label map generation number is
  -- incremented by 1.
remapLabels gs lmap@(LabelMap gen _) ls =
    LabelMap gen' $ M.fromList newEntries
    where
        gen'                = gen + 1
        newEntries          = [ (l, (gen', fromIntegral (newIndex l))) | l <- ls ]
        -- TODO: should review this given the changes to the hash code
        --       since it was re-written
        newIndex l
            | labelIsVar l  = mapAdjacent l    -- adjacency classifies variable labels
            | otherwise     = hashVal gen l    -- otherwise rehash (to disentangle collisions)  TODO: BRANCH IS UNTESTED

        -- mapAdjacent used to use `rem` hashModulus
        mapAdjacent l       = hashModulus `hashWithSalt` sum (sigsOver l)

        gls = S.toList gs

        sigsOver l          = select (hasLabel l) gls (arcSignatures lmap gls)

-- |Select is like filter, except that it tests one list to select
--  elements from a second list.
select :: ( a -> Bool ) -> [a] -> [b] -> [b]
select _ [] []           = []
select f (e1:l1) (e2:l2)
    | f e1      = e2 : select f l1 l2
    | otherwise = select f l1 l2
select _ _ _    = error "select supplied with different length lists"

-- | Return the set of distinct labels used in the graph.

graphLabels :: (Label lb) => ArcSet lb -> S.Set lb
graphLabels = getComponents arcLabels

-- TODO: worry about overflow?

-- TODO: should probably return a Set of (Int, Arc lb) or something, 
-- as may be useful for the calling code

-- | Calculate a signature value for each arc that can be used in 
--   constructing an adjacency based value for a node.  The adjacancy
--   value for a label is obtained by summing the signatures of all
--   statements containing that label.
--
arcSignatures :: 
  (Label lb) 
  => LabelMap lb -- ^ the current label index values
  -> [Arc lb] -- ^ calculate signatures for these arcs
  -> [Int] -- ^ the signatures of the arcs
arcSignatures lmap =
    map (sigCalc . arcToTriple) 
    where
        sigCalc (s,p,o)  =
            hashModulus `hashWithSalt`
              ( labelVal2 s +
                labelVal2 p * 3 +
                labelVal2 o * 5 )
          
        labelVal         = mapLabelIndex lmap
        labelVal2        = uncurry (*) . labelVal

-- | Return a new graph that is supplied graph with every node/arc
--  mapped to a new value according to the supplied function.
--
--  Used for testing for graph equivalence under a supplied
--  label mapping;  e.g.
--
--  >  if ( graphMap nodeMap gs1 ) == ( graphMap nodeMap gs2 ) then (same)
--
graphMap ::
    (Label lb)
    => LabelMap lb
    -> ArcSet lb
    -> ArcSet LabelIndex
graphMap = S.map . fmap . mapLabelIndex

-- | Compare a pair of graphs for equivalence under a given mapping
--   function.
--
--  This is used to perform the ultimate test that two graphs are
--  indeed equivalent:  guesswork in `graphMatch2` means that it is
--  occasionally possible to construct a node mapping that generates
--  the required singleton equivalence classes, but does not fully
--  reflect the topology of the graphs.

graphMapEq ::
    (Label lb) 
    => LabelMap lb
    -> ArcSet lb
    -> ArcSet lb 
    -> Bool
graphMapEq lmap = (==) `on` graphMap lmap

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2016, 2018, 2020, 2022, 2024 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
