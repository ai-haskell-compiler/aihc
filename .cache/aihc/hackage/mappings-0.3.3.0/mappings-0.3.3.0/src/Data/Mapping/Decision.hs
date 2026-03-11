{-# LANGUAGE
      CPP,
      DerivingVia,
      MultiParamTypeClasses,
      OverloadedStrings,
      RankNTypes,
      StandaloneDeriving,
      TupleSections
  #-}

-- | Decision diagrams, parametric in the mapping type for the decisions.
--
-- This is inspired by binary decision diagrams (as described in detail in
-- Knuth's The Art of Computer Programming, volume 4A); these are the specific
-- case where m is `BoolMapping` and v is `Bool`. Our algorithms are mostly
-- straightforward generalisations of those considered there.
--

-- TODO
--  * Format types of functions better
--  * Decisions go upwards in order currently; should they go
--    downwards, to coincide with lexicographical orderings on maps
--    and hence maybe make smaller decision diagrams?
--    We can use Down if necessary to amend this
--  * Increase test coverage
--  * Examples:
--     - finding optima
--     - finding random elements
--    (as examples of the more general functions, already coded, I hope)
--  * Separate out "Base" stuff into other modules?
--  * Documentation
--
-- MAYBE TO DO
--  * Composition algorithm?
--  * Optimisation by reordering
module Data.Mapping.Decision where

#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
#else
import Control.Applicative (liftA2)
#endif
import Control.Monad ((<=<))
import Data.Algebra.Boolean (Boolean(..))
import Data.Bifunctor (first)
import Data.Bijection (Bij)
import qualified Data.Bijection as B
import Data.Bits (complement)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Foldable.WithIndex (FoldableWithIndex(..))
import Data.Functor.Identity (Identity(..))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ord (comparing)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Q
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Data.Mapping.Util (insertIfAbsent)
import Formatting ((%))
import qualified Formatting as F

import Data.Mapping


-- | A node of a decision diagram: which value do we scrutinise, and what do we
-- do with it?
data Node k m a = Node {
  nodeDecision :: !a,
  nodeBranch :: !(m Int)
}

deriving instance (Eq a, Eq (m Int)) => Eq (Node k m a)

deriving instance (Ord a, Ord (m Int)) => Ord (Node k m a)


-- | A decision diagram (with no preferred starting point), containing
-- leaves (representing final values of the decision process) indexed
-- from -1 downwards, and nodes (representing the need to scrutinise a
-- value) indexed from 0 upwards
data Base k m a v = Base {
  leaves :: Seq v,
  nodes :: Seq (Node k m a)
}

baseLength :: Base k m a v -> Int
baseLength (Base l m) = Q.length l + Q.length m

-- | A decision diagram with a starting point
data Decision k m a v = Decision {
  base :: !(Base k m a v),
  start :: !Int
}

decisionLength :: Decision k m a v -> Int
decisionLength = baseLength . base

-- | A value for every node of a base
data BaseMap v = BaseMap {
  onLeaves :: Seq v,
  onNodes :: Seq v
}

-- | Index a BaseMap
bindex :: BaseMap v -> Int -> v
bindex (BaseMap l m) x
  | x < 0     = Q.index l $ complement x
  | otherwise = Q.index m x


-- | Close a set under an operation
closure :: (Int -> IntSet) -> IntSet -> IntSet
closure f = let
  inner old new = case IS.minView new of
    Nothing -> old
    Just (x, new') -> let
      old' = IS.insert x old
      in inner old' (new' `IS.union` (f x `IS.difference` old'))
  in inner IS.empty


-- | A general kind of recursive function on a Base
baseRecurse :: (Ord c,
                Mapping k m)
            => (v -> c)
               -- ^ What to do on a value
            -> (a -> m c -> c)
               -- ^ What do do on a node
            -> Base k m a v
               -- ^ Input base
            -> BaseMap c
baseRecurse p q (Base l m) = let
  l' = p <$> l
  f v (Node x n) = v |> q x (mmap (bindex (BaseMap l' v)) n)
  in BaseMap l' $ foldl f Q.empty m

-- | A general kind of recursive function on a Decision
decisionRecurse :: (Ord c,
                    Mapping k m)
                 => (v -> c)
                 -- ^ What to do on a value
                 -> (a -> m c -> c)
                 -- ^ What do do on a node
                 -> Decision k m a v
                 -- ^ Input decision
                 -> c
decisionRecurse p q (Decision b s) = bindex (baseRecurse p q b) s


-- | A general counting function
generalCounts :: (Ord a, Ord n, Mapping k m)
              => (a -> a -> Int)
                 -- ^ In the list of decisions, how far apart are these?
              -> a
                 -- ^ The first possible decision
              -> a
                 -- ^ The last possible decision
              -> (v -> n)
                 -- ^ The count of a value
              -> (m n -> n)
                 -- ^ How to combine counts at a node
              -> Decision k m a v
                 -- ^ The input decision diagram
              -> n
                 -- ^ The count
generalCounts d x0 x1 onVal combine = let
  d' Nothing Nothing = 2 + d x0 x1
  d' Nothing (Just y) = 1 + d x0 y
  d' (Just x) Nothing = 1 + d x x1
  d' (Just x) (Just y) = d x y
  p x (y, a) = let
    q 1 v = v
    q n v = q (n-1) . combine $ cst v
    in q (d' x y) a
  f x = (Nothing, onVal x)
  g a m = let
    b = Just a
    in (b, combine $ mmap (p b) m)
  in p Nothing . decisionRecurse f g

-- | How many values are true in a decision diagram with integer leaves?
numberTrueGeneral :: Mapping k m => (m Integer -> Integer) -> Int -> Int -> Decision k m Int Bool -> Integer
numberTrueGeneral g x0 x1 = let
  f a = if a then 1 else 0
  in generalCounts subtract x0 x1 f g

-- | How many values are True in a binary decision diagram with integer leaves?
numberTrue :: Int -> Int -> Decision Bool OnBool Int Bool -> Integer
numberTrue = numberTrueGeneral sum

-- | Assignments of variables that result in True
chunksTrue :: (Mapping k m, FoldableWithIndex k m, Ord k, Ord a) => Decision k m a Bool -> [Map a k]
chunksTrue = let
  f False = []
  f True = [M.empty]
  g a = ifoldMap (\x -> fmap (M.insert a x))
  in decisionRecurse f g

-- | All true values (may be a very long list even for reasonable Decisions)
listTrue :: forall k m a.
           (Mapping k m,
            FoldableWithIndex k m,
            Ord k,
            Ord a)
         => Set a
         -> Decision k m a Bool
         -> [Map a k]
listTrue s = let
  m = M.fromSet (const ()) s
  u = ifoldMap (\i _ -> [i]) $ cst @k @m ()
  fillIn = let
    onL = M.traverseMissing (\_ () -> u)
    onR = M.mapMissing (const (error "Expected a key"))
    onB = M.zipWithMatched (\_ () -> id)
    in M.mergeA onL onR onB
  in fillIn m <=< chunksTrue

-- | What is the best assignment of keys to values resulting in a
-- value on which `p` is `True`?
bestSuchThat :: (Mapping k m, Ord k, Ord a, Ord v) => (v -> Bool) -> (forall w. a -> m w -> Maybe (k, w)) -> Decision k m a v -> Maybe ([(a,k)], v)
bestSuchThat p q = let
  f x = if p x then Just ([], x) else Nothing
  g i = uncurry (\x -> fmap (first ((i,x):))) <=< q i
  in decisionRecurse f g

-- | Build a sequence from key-value pairs; we take on trust that all
-- values are represented once.
fromKeyVals :: (Foldable f) => f (Int,a) -> Seq a
fromKeyVals = fmap snd . Q.sortBy (comparing fst) . Q.fromList . toList


-- | A data structure for work-in-progress decision diagrams
data Builder o k m a v = Builder {
  leavesMap :: Map v Int,
  nodesMap :: Map (Node k m a) Int,
  fromOld :: Map o Int
}

emptyBuilder :: Builder o k m a v
emptyBuilder = Builder M.empty M.empty M.empty

addLeaf :: (Ord o,
            Ord v)
        => v
        -> o
        -> Builder o k m a v
        -> Builder o k m a v
addLeaf x y (Builder l m o) = let
  i = complement (M.size l)
  (j, s) = insertIfAbsent x i l
  o' = M.insert y j o
  in case s of
    Nothing -> Builder l m o'
    Just l' -> Builder l' m o'

addNode :: (Ord o,
            Ord (m Int),
            Ord a,
            Mapping k m)
        => a
        -> m o
        -> o
        -> Builder o k m a v
        -> Builder o k m a v
addNode r a y (Builder l m o) = let
  b = mmap (o M.!) a
  in case isConst b of
    Just j -> Builder l m (M.insert y j o)
    Nothing -> let
      i = M.size m
      (j, s) = insertIfAbsent (Node r b) i m
      o' = M.insert y j o
      in case s of
        Nothing -> Builder l m o'
        Just m' -> Builder l m' o'

makeBuilder :: (Mapping k m,
                Ord o,
                Ord (m Int),
                Ord a,
                Ord v)
             => Map o v
             -> Map o (a, m o)
             -> Builder o k m a v
makeBuilder l m = let
  b0 = emptyBuilder
  makeL b i x = addLeaf x i b
  b1 = M.foldlWithKey' makeL b0 l
  makeN b i (r, o) = addNode r o i b
  b2 = M.foldlWithKey' makeN b1 m
  in b2

buildBase :: Builder o k m a v -> Base k m a v
buildBase (Builder l m _) = let
  l' = fromKeyVals . fmap (\(x,i) -> (complement i,x)) $ M.toList l
  m' = fromKeyVals . fmap (\(x,i) -> (i,x)) $ M.toList m
  in Base l' m'

buildDecision :: Ord o => o -> Builder o k m a v -> Decision k m a v
buildDecision s b@(Builder _ _ o) = Decision (buildBase b) (o M.! s)

-- | A decision tree based on a single decision
singleNode :: (Mapping k m, Ord (m Int), Ord a, Ord v) => a -> m v -> Decision k m a v
singleNode r n = let
  f b x = addLeaf x (Just x) b
  d = addNode r (mmap Just n) Nothing $ foldl f emptyBuilder n
  in buildDecision Nothing d

-- | A building block for BDD's - tests if a variable is true
genTest :: Boolean b => a -> Decision Bool OnBool a b
genTest r = let
  l = Q.fromList [false, true]
  m = pure . Node r $ OnBool (-1) (-2)
  s = 0
  in Decision (Base l m) s

-- | Test if a variable is true (specialised to `Bool`)
test :: a -> Decision Bool OnBool a Bool
test = genTest


-- | Rapidly take the conjunction of the inputs
buildAll :: Mapping k m => Map a (m Bool) -> Decision k m a Bool
buildAll d = let
  l = Q.fromList [true, false]
  s = M.size d
  m = Q.fromList $ do
    (i,(r,n)) <- zip [0..] (M.toDescList d)
    pure (Node r (mmap (bool (-2) (i-1)) n))
  in Decision (Base l m) (s-1)

-- | Rapidly take the disjunction of the inputs
buildAny :: Mapping k m => Map a (m Bool) -> Decision k m a Bool
buildAny d = let
  l = Q.fromList [false, true]
  s = M.size d
  m = Q.fromList $ do
    (i,(r,n)) <- zip [0..] (M.toDescList d)
    pure (Node r (mmap (bool (i-1) (-2)) n))
  in Decision (Base l m) (s-1)


-- | Traverse bases
baseTraverse :: (Applicative f, Ord a, Ord (m Int), Ord w, Mapping k m) => (v -> f w) -> Base k m a v -> f (Builder Int k m a w)
baseTraverse p (Base l m) = let
  t0 = pure emptyBuilder

  t1 = let
    f b i x = liftA2 (\b' px' -> addLeaf px' (complement i) b') b (p x)
    in Q.foldlWithIndex f t0 l

  t2 = let
    f b i (Node r d) = addNode r d i <$> b
    in Q.foldlWithIndex f t1 m

  in t2


-- | Map bases
baseMap :: (Ord a, Ord (m Int), Ord w, Mapping k m) => (v -> w) -> Base k m a v -> Builder Int k m a w
baseMap p = runIdentity . baseTraverse (Identity . p)


-- | A more general map for `Base`, where the shape of nodes can change
baseTransform ::    (Ord a, Ord (n Int), Mapping l n, Ord w)
                 => (v -> w)
                 -> (forall x. a -> m x -> n x)
                 -> Base k m a v
                 -> IntSet
                 -> Builder Int l n a w
baseTransform p q (Base l m) = let

  close aL aN s = case IS.maxView s of
   Nothing -> makeBuilder aL aN
   Just (i, s') -> if i < 0
     then let
       x = p (Q.index l $ complement i)
       in close (M.insert i x aL) aN s'
     else let
       Node r n = Q.index m i
       o = q r n
       s'' = IS.union s' . IS.fromList $ toList o
       in close aL (M.insert i (r, o) aN) s''

  in close M.empty M.empty


-- | A more general map for `Decision`, where the shape of nodes can change
decisionTransform :: (Mapping l n,
                      Ord (n Int),
                      Ord a,
                      Ord w)
                   => (v -> w)
                   -> (forall x. a -> m x -> n x)
                   -> Decision k m a v
                   -> Decision l n a w
decisionTransform p q (Decision b s) = let
  in buildDecision s $ baseTransform p q b (IS.singleton s)


-- | Fill in some values of a map
-- > act (restrict h d) f = let
-- >   f' x = case h x of
-- >     Just y  -> y
-- >     Nothing -> f x
-- >   in act d f'
restrict :: (Ord (m Int), Ord v, Ord a, Mapping k m) => (a -> Maybe k) -> Decision k m a v -> Decision k m a v
restrict f = let
  g x m = case f x of
    Nothing -> m
    Just c -> cst (act m c)
  in decisionTransform id g


-- | A general function for merging bases
baseGenMerge ::    (Ord a, Ord w, Ord (o Int), Mapping l o)
                => (u -> v -> w)
                -> (forall x . Ord x => a -> m x -> o x)
                -> (forall y . Ord y => a -> n y -> o y)
                -> (forall x y. (Ord x, Ord y) => a -> m x -> n y -> o (x, y))
                -> Base h m a u -> Base k n a v -> Set (Int, Int) -> Builder (Int, Int) l o a w
baseGenMerge pLL pNL pLN pNN (Base l1 m1) (Base l2 m2) = let

  close aLL aNL aLN aNN s = case S.maxView s of
    Nothing -> make aLL aNL aLN aNN
    Just ((i1, i2), s') -> case (i1 < 0, i2 < 0) of
      ( True,  True) -> let
        x = pLL (Q.index l1 $ complement i1) (Q.index l2 $ complement i2)
        in close (M.insert (i1, i2) x aLL) aNL aLN aNN s'
      ( True, False) -> let
        Node r2 n2 = Q.index m2 i2
        o = mmap (i1,) $ pLN r2 n2
        s'' = S.union s' . S.fromList $ toList o
        in close aLL aNL (M.insert (i1, i2) (r2, o) aLN) aNN s''
      (False,  True) -> let
        Node r1 n1 = Q.index m1 i1
        o = mmap (,i2) $ pNL r1 n1
        s'' = S.union s' . S.fromList $ toList o
        in close aLL (M.insert (i1, i2) (r1, o) aNL) aLN aNN s''
      (False, False) -> let
        Node r1 n1 = Q.index m1 i1
        Node r2 n2 = Q.index m2 i2
        (r, o) = case compare r1 r2 of
          LT -> (r1, mmap (,i2) $ pNL r1 n1)
          GT -> (r2, mmap (i1,) $ pLN r2 n2)
          EQ -> (r1, pNN r1 n1 n2)
        s'' = S.union s' . S.fromList $ toList o
        in close aLL aNL aLN (M.insert (i1, i2) (r, o) aNN) s''

  make aLL aNL aLN aNN = let

    b0 = emptyBuilder

    makeL b (i, j) x = addLeaf x (i, j) b
    b1 = M.foldlWithKey' makeL b0 aLL

    makeN b (i, j) (r, o) = addNode r o (i, j) b
    b2 = M.foldlWithKey' makeN b1 aNL
    b3 = M.foldlWithKey' makeN b2 aLN
    b4 = M.foldlWithKey' makeN b3 aNN
    in b4

  in close M.empty M.empty M.empty M.empty


-- | Merge two bases in an applicative functor
baseMergeA ::    (Applicative f, Ord a, Ord w, Ord (m Int), Mapping k m)
              => (u -> v -> f w)
              -> Base k m a u -> Base k m a v -> Set (Int, Int) -> f (Builder (Int, Int) k m a w)
baseMergeA p (Base l1 m1) (Base l2 m2) = let

  close aLL aNL aLN aNN s = case S.maxView s of
    Nothing -> make aLL aNL aLN aNN
    Just ((i1, i2), s') -> case (i1 < 0, i2 < 0) of
      ( True,  True) -> let
        x = p (Q.index l1 $ complement i1) (Q.index l2 $ complement i2)
        in close (M.insert (i1, i2) x aLL) aNL aLN aNN s'
      ( True, False) -> let
        Node r2 n2 = Q.index m2 i2
        o = mmap (i1,) n2
        s'' = S.union s' . S.fromList $ toList o
        in close aLL aNL (M.insert (i1, i2) (r2, o) aLN) aNN s''
      (False,  True) -> let
        Node r1 n1 = Q.index m1 i1
        o = mmap (,i2) n1
        s'' = S.union s' . S.fromList $ toList o
        in close aLL (M.insert (i1, i2) (r1, o) aNL) aLN aNN s''
      (False, False) -> let
        Node r1 n1 = Q.index m1 i1
        Node r2 n2 = Q.index m2 i2
        (r,o) = case compare r1 r2 of
          LT -> (r1, mmap (,i2) n1)
          GT -> (r2, mmap (i1,) n2)
          EQ -> (r1, merge (,) n1 n2)
        s'' = S.union s' . S.fromList $ toList o
        in close aLL aNL aLN (M.insert (i1, i2) (r, o) aNN) s''

  make aLL aNL aLN aNN = let

    b0 = pure emptyBuilder

    makeL b (i, j) = liftA2 (\b' x'-> addLeaf x' (i, j) b') b
    b1 = M.foldlWithKey' makeL b0 aLL

    makeN b (i, j) (r, o) = addNode r o (i, j) <$> b
    b2 = M.foldlWithKey' makeN b1 aNL
    b3 = M.foldlWithKey' makeN b2 aLN
    b4 = M.foldlWithKey' makeN b3 aNN
    in b4

  in close M.empty M.empty M.empty M.empty


-- | Merge two bases
baseMerge ::    (Ord a, Ord w, Ord (m Int), Mapping k m)
             => (u -> v -> w)
             -> Base k m a u -> Base k m a v -> Set (Int, Int) -> Builder (Int, Int) k m a w
baseMerge p b1 b2 = let
  p' x y = Identity $ p x y
  in runIdentity . baseMergeA p' b1 b2


-- | Folds over *all* the leaves; not something you want to do to an
-- arbitrary base
instance Foldable (Base k m a) where
  foldMap p = foldMap p . leaves

instance Foldable m => Foldable (Decision k m a) where
  foldMap p (Decision (Base l m) s) = let
    inner x old new = case IS.minView new of
      Nothing        -> x
      Just (i, new') -> if i < 0
        then inner (x <> p (Q.index l (complement i))) (IS.insert i old) new'
        else let
          old' = IS.insert i old
          extra = IS.difference (IS.fromList . toList . nodeBranch $ Q.index m i) old'
          in inner x old' (IS.union new' extra)
    in inner mempty IS.empty $ IS.singleton s

instance (Ord a, Ord (m Int), Mapping k m) => Mapping (a -> k) (Decision k m a) where

  cst x = Decision (Base (Q.singleton x) Q.empty) (-1)

  act (Decision (Base l n) s) f = let
    inner i
      | i < 0 = Q.index l $ complement i
      | otherwise = let
          Node a m = Q.index n i
          in inner . act m $ f a
    in inner s

  -- We assume the diagram is optimised, so it is constant only if it starts
  -- with a leaf.
  isConst (Decision (Base l _) s)
    | s < 0     = Just . Q.index l $ complement s
    | otherwise = Nothing

  mtraverse p (Decision (Base l m) s) = buildDecision s <$> baseTraverse p (Base l m)

  mmap p (Decision b s) = buildDecision s $ baseMap p b

  merge p (Decision b1 s1) (Decision b2 s2) = buildDecision (s1, s2) $ baseMerge p b1 b2 (S.singleton (s1, s2))

  mergeA p (Decision b1 s1) (Decision b2 s2) = buildDecision (s1, s2) <$> baseMergeA p b1 b2 (S.singleton (s1, s2))


deriving via (AlgebraWrapper (a -> k) (Decision k m a) v)
  instance (Mapping k m, Ord (m Int), Ord a, Ord v, Semigroup v) => Semigroup (Decision k m a v)

deriving via (AlgebraWrapper (a -> k) (Decision k m a) v)
  instance (Mapping k m, Ord (m Int), Ord a, Ord v, Monoid v) => Monoid (Decision k m a v)

deriving via (AlgebraWrapper (a -> k) (Decision k m a) v)
  instance (Mapping k m, Ord (m Int), Ord a, Ord v, Num v) => Num (Decision k m a v)

deriving via (AlgebraWrapper (a -> k) (Decision k m a) v)
  instance (Mapping k m, Ord (m Int), Ord a, Ord v, Boolean v) => Boolean (Decision k m a v)


-- | Attempt to extend to a bijection
checkBijection :: (Eq a, Eq v, Mapping k m) => Base k m a v -> Base k m a v -> Bij -> Maybe Bij
checkBijection (Base l1 m1) (Base l2 m2) = let
  consequences i j = case (i < 0, j < 0) of
    (True, True) -> if Q.index l1 (complement i) == Q.index l2 (complement j)
      then Just B.empty
      else Nothing
    (False, False) -> let
      Node r1 o1 = Q.index m1 i
      Node r2 o2 = Q.index m2 j
      in if r1 == r2
        then B.getMaybeBij $ pairMappings B.msingleton o1 o2
        else Nothing
    _ -> Nothing
  in B.closeBijection consequences

-- | Are these Decisions isomorphic?
findBijection :: (Eq a, Eq v, Mapping k m) => Decision k m a v -> Decision k m a v -> Maybe Bij
findBijection (Decision b1 s1) (Decision b2 s2) = checkBijection b1 b2 (B.singleton s1 s2)

instance (Eq a, Eq v, Mapping k m) => Eq (Decision k m a v) where
  u == v = case findBijection u v of
    Just _ -> True
    Nothing -> False


-- | A ludicrously short definition!
instance (Ord a, Ord v, Ord (m Int), Mapping k m) => Ord (Decision k m a v) where
  compare = pairMappings compare


-- | Output the structure of a Decision
debugShow :: (Show a, Show v, Show (m Int)) => Decision k m a v -> String
debugShow (Decision (Base l m) s) = let

  p = 1 + max (1 + length (show (Q.length l))) (length (show (1 + Q.length m)))

  prefix i = ((if i == s then "->" else "  ") <>)

  leafLine t i x = let
    j = complement i
    in prefix j (F.formatToString (F.left p ' ' % ": " % F.shown % "\n") j x) <> t

  nodeLine i (Node r n) t =
    prefix i (F.formatToString (F.left p ' ' % ": " % F.shown % "; " % F.shown % "\n") i r n) <> t

  in Q.foldlWithIndex leafLine (Q.foldrWithIndex nodeLine "" m) l


instance (Mapping k m,
          Neighbourly m,
          Ord a,
          Ord (m Int))
       => Neighbourly (Decision k m a) where
  neighbours (Decision (Base l m) s) = let
    f v (Node _ n) = let
      here = let
        b = Base l m
        e (i, j) = S.filter (uncurry (/=)) $ mutualValues (Decision b i) (Decision b j)
        in foldMap e $ neighbours n
      there = let
        g i
          | i < 0     = mempty
          | otherwise = Q.index v i
        in foldMap g n
      in v |> (here <> there)
    in Q.index (foldl f Q.empty m) s


