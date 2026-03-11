module Numeric.Interpolation.NodeList (
   T(Interval, Node),
   fromList,
   toList,
   singleton,
   lookup,
   ) where

import Data.Tuple.HT (mapFst)

import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mempty, (<>))

import Control.Applicative (liftA3, pure)

import Prelude hiding (lookup)

{- $setup
>>> import qualified Numeric.Interpolation.NodeList as Nodes
>>> import qualified Data.Traversable as Trav
>>> import qualified Data.Foldable as Fold
>>> import qualified Data.List as List
>>> import Data.Tuple.HT (mapSnd)
>>> import Data.Char (ord)
-}


data T x y = Interval | Node (x, y) (T x y) (T x y)
   deriving (Eq, Ord, Show)

{- |
prop> \xs -> map (mapSnd ord) xs == Nodes.toList (fmap ord (Nodes.fromList (xs::[(Integer,Char)])))
-}
instance Functor (T x) where
   fmap f =
      let go Interval = Interval
          go (Node (x,y) l r) = Node (x, f y) (go l) (go r)
      in  go

{- |
prop> \xs -> map snd xs == Fold.toList (Nodes.fromList (xs::[(Integer,Char)]))
-}
instance Foldable (T x) where
   foldMap f =
      let go Interval = mempty
          go (Node (_x,y) l r) = go l <> f y <> go r
      in  go

{- |
prop> \x xs -> let f acc y = (acc+y,acc) in List.mapAccumL f x (map snd xs) == mapSnd Fold.toList (Trav.mapAccumL f x (Nodes.fromList (xs::[(Int,Integer)])))
-}
instance Traversable (T x) where
   traverse f =
      let go Interval = pure Interval
          go (Node (x,y) l0 r0) =
             liftA3 (\l m r -> Node (x,m) l r) (go l0) (f y) (go r0)
      in  go


{- |
list must be sorted with respect to first element
-}
fromList :: [(x,y)] -> T x y
fromList =
   let merge n0 xys0 =
          case xys0 of
             (xy0,n1):(xy1,n2):xys ->
                (Node xy0 n0 n1,
                 uncurry (:) $ mapFst ((,) xy1) $ merge n2 xys)
             (xy0,n1):[] -> (Node xy0 n0 n1, [])
             [] -> (n0, [])
       rep (n,xyns) = if null xyns then n else rep $ merge n xyns
   in  rep . merge Interval . map (flip (,) Interval)

{- |
prop> \x y -> Nodes.singleton x y == Nodes.fromList [(x,y)::(Integer,Char)]
-}
singleton :: x -> y -> T x y
singleton x y = Node (x,y) Interval Interval

{- |
prop> \xs -> xs == Nodes.toList (Nodes.fromList (xs::[(Integer,Char)]))
-}
toList :: T x y -> [(x,y)]
toList =
   let go Interval = []
       go (Node p l r) = go l ++ p : go r
   in  go

{- |
>>> Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) (-1)
(Nothing,Just (0,'a'))
>>> Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) 0
(Just (0,'a'),Just (2,'b'))
>>> Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) 1
(Just (0,'a'),Just (2,'b'))
>>> Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) 2
(Just (2,'b'),Nothing)
>>> Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) 3
(Just (2,'b'),Nothing)
>>> Nodes.lookup (Nodes.fromList ([(0,'a'),(2,'b'),(5::Int,'c')])) 3
(Just (2,'b'),Just (5,'c'))
-}
lookup :: Ord x => T x y -> x -> (Maybe (x,y), Maybe (x,y))
lookup nodes0 x0 =
   let go lb rb Interval = (lb, rb)
       go lb rb (Node n@(x,_y) ln rn) =
          if x0>=x
            then go (Just n) rb rn
            else go lb (Just n) ln
   in  go Nothing Nothing nodes0
