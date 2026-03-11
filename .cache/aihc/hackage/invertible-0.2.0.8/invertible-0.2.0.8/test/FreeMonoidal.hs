{-# LANGUAGE CPP, GADTs, FlexibleContexts, FlexibleInstances, TypeOperators, TupleSections, ConstraintKinds, GeneralizedNewtypeDeriving, QuasiQuotes #-}
module FreeMonoidal (tests) where

import Control.Monad (join, guard)
import Data.Functor.Classes (Show1(..))
import Data.Maybe (isJust)
import qualified Test.QuickCheck as Q
import Unsafe.Coerce (unsafeCoerce)

import Control.Invertible.Monoidal.Free
import qualified Data.Invertible as I

genTree :: [Q.Gen a] -> Q.Gen a
genTree = Q.scale (max 0 . pred) . join . Q.growingElements

newtype Const a b = Const a
  deriving (Eq, Ord, Show, Q.Arbitrary)

instance Functor (Const a) where
  fmap _ (Const x) = Const x

instance Show a => Show1 (Const a) where
#if MIN_VERSION_base(4,9,0)
  liftShowsPrec _ _ =
#else
  showsPrec1 =
#endif
    showsPrec

data Tree a
  = TreeEmpty
  | TreeFree a
  | TreeJoin !(Tree a, Tree a)
  | TreeChoose !(Either (Tree a) (Tree a))
  deriving (Eq, Show)

instance Q.Arbitrary a => Q.Arbitrary (Tree a) where
  arbitrary = genTree
    [ return TreeEmpty
    , TreeFree <$> Q.arbitrary
    , TreeJoin <$> Q.arbitrary
    , TreeChoose <$> Q.arbitrary
    ]

  shrink TreeEmpty = []
  shrink (TreeFree x) = TreeEmpty : map TreeFree (Q.shrink x) 
  shrink (TreeJoin (p, q)) =
    [TreeEmpty, p, q] ++ map TreeJoin (Q.shrink (p, q))
  shrink (TreeChoose (Left p)) =
    [TreeEmpty, p] ++ map (TreeChoose . Left) (Q.shrink p)
  shrink (TreeChoose (Right p)) =
    [TreeEmpty, p] ++ map (TreeChoose . Right) (Q.shrink p)

type FreeTree a = Free (Const a) (Tree a)

emptyTree :: FreeTree a
emptyTree = Transform (I.const TreeEmpty) Empty

leaf :: a -> FreeTree a
leaf = Free . Const

unJoin :: (Tree a, Tree a) I.<-> Tree a
unJoin = [I.biCase|t <-> TreeJoin t|]

unChoose :: Either (Tree a) (Tree a) I.<-> Tree a
unChoose = [I.biCase|t <-> TreeChoose t|]

instance Q.Arbitrary a => Q.Arbitrary (FreeTree a) where
  arbitrary = genTree
    [ return emptyTree
    , leaf <$> Q.arbitrary
    , Transform unJoin   <$> (Join   <$> Q.arbitrary <*> Q.arbitrary)
    , Transform unChoose <$> (Choose <$> Q.arbitrary <*> Q.arbitrary)
    , Transform I.id <$> Q.arbitrary
    ]

  shrink (Free x) = map Free (Q.shrink x) 
  shrink (Transform _ Empty) = []
  shrink (Transform _ (Join p q)) =
    [emptyTree, pt, qt] ++ map (Transform unJoin . uncurry Join) (Q.shrink (pt, qt)) where
    pt = unsafeCoerce p :: FreeTree a
    qt = unsafeCoerce q :: FreeTree a
  shrink (Transform _ (Choose p q)) =
    [emptyTree, pt, qt] ++ map (Transform unChoose . uncurry Choose) (Q.shrink (pt, qt)) where
    pt = unsafeCoerce p :: FreeTree a
    qt = unsafeCoerce q :: FreeTree a
  shrink (Transform _ p) = [pt] where
    pt = unsafeCoerce p :: FreeTree a

type ShowFree f = (Functor f, Show1 f)

ok :: Q.Property
ok = Q.property True

bad :: ShowFree f => Free f a -> Q.Property
bad f = Q.counterexample (show f) False

checkNoT :: ShowFree f => Free f a -> Q.Property
checkNoT t@(Transform _ _) = bad t
checkNoT (Join p q) = checkNoT p Q..&&. checkNoT q
checkNoT (Choose p q) = checkNoT p Q..&&. checkNoT q
checkNoT _ = ok

checkTNF :: ShowFree f => Free f a -> Q.Property
checkTNF (Transform _ p) = checkNoT p
checkTNF p = checkNoT p

joinDNF :: ShowFree f => Free f a -> Q.Property
joinDNF t@(Transform _ _) = bad t
joinDNF c@(Choose _ _) = bad c
joinDNF (Join p q) = joinDNF p Q..&&. joinDNF q
joinDNF _ = ok

checkDNF :: ShowFree f => Free f a -> Q.Property
checkDNF t@(Transform _ _) = bad t
checkDNF (Choose p q) = checkDNF p Q..&&. checkDNF q
checkDNF p = joinDNF p

checkTDNF :: ShowFree f => Free f a -> Q.Property
checkTDNF (Transform _ p) = checkDNF p
checkTDNF p = checkDNF p

produceParse :: FreeTree Int -> [Int] -> Q.Property
produceParse f l = isJust p Q.==> check where
  check = prod f r ++ t Q.=== l Q..&&. reverse t ++ prod (reverseFree f) r Q.=== reverse l
  Just (r, t) = p
  p = pars f l
  pars = parseFree parse
  parse (Const i) j = unsafeCoerce (TreeFree j) <$ guard (j >= i)
  prod = produceFree produce
  produce (Const _) x = case unsafeCoerce x of { ~(TreeFree j) -> j }

joinSorted :: (Show a, Ord a) => Maybe a -> Free (Const a) b -> (Maybe a, Q.Property)
joinSorted b (Join p q) = (qb, pr Q..&&. qr) where
  (pb, pr) = joinSorted b p
  (qb, qr) = joinSorted pb q
joinSorted b (Free (Const x)) = (Just x, Q.counterexample (show x ++ " < " ++ show b) $ all (x >=) b)
joinSorted b Empty = (b, ok)
joinSorted b o = (b, bad o)

chooseSorted :: (Show a, Ord a) => Free (Const a) b -> Q.Property
chooseSorted t@(Transform _ _) = bad t
chooseSorted (Choose p q) = chooseSorted p Q..&&. chooseSorted q
chooseSorted p = snd $ joinSorted Nothing p

checkSorted :: (Show a, Ord a) => Free (Const a) b -> Q.Property
checkSorted (Transform _ p) = chooseSorted p
checkSorted p = chooseSorted p

compareConst :: Ord a => Const a b -> Const a c -> Ordering
compareConst (Const x) (Const y) = compare x y

tests :: Q.Property
tests = Q.conjoin
  [ Q.label "TNF" $ checkTNF . (freeTNF :: FreeTree Int -> FreeTree Int)
  , Q.label "TDNF" $ checkTDNF . (freeTDNF :: FreeTree Int -> FreeTree Int)
  , Q.label "parseProduce" $ produceParse
  , Q.label "sort" $ checkSorted . (sortFreeTDNF compareConst :: FreeTree Int -> FreeTree Int)
  ]
