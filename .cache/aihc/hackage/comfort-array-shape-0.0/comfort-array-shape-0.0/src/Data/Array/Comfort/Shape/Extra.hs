{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Data.Array.Comfort.Shape.Extra
   {- DEPRECATED "use Data.Array.Comfort.Shape.Simplex instead" -}
   (
   Simplex(..),
   ) where

import qualified Data.Array.Comfort.Shape as Shape

import qualified Type.Data.Num.Unary as Unary
import Type.Data.Num (integralFromProxy)
import Type.Base.Proxy (Proxy(Proxy))

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.FixedLength as FL
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT

import Control.Applicative ((<$>))


{- $setup
>>> import qualified Data.Array.Comfort.Shape.Extra as ShapeExtra
>>> import qualified Data.Array.Comfort.Shape as Shape
>>>
>>> import qualified Type.Data.Num.Unary.Literal as TypeNum
>>> import qualified Type.Data.Num.Unary as Unary
-}


{- |
Simplex is a generalization of 'Shape.Triangular' to more than two dimensions.
Indices are tuples of fixed size
with elements ordered in ascending, strictly ascending,
descending or strictly descending order.
\"Order\" refers to the index order in 'indices'.
In order to avoid confusion we suggest that the order of 'indices'
is consistent with '<='.

Obviously, 'offset' implements ranking
and 'indexFromOffset' implements unranking
of combinations (in the combinatorial sense)
with or without repetitions.

>>> Shape.indices $ ShapeExtra.Simplex (Unary.unary TypeNum.u3) $ Shape.ZeroBased (4::Int)
[0!:1!:2!:end,0!:1!:3!:end,0!:2!:3!:end,1!:2!:3!:end]
-}
data Simplex d size =
   Simplex {
      simplexDimension :: UnaryProxy d,
      simplexSize :: size
   } deriving (Show)

type UnaryProxy d = Proxy (Unary.Un d)

-- cf. package combinatorial
binomials :: Integral a => a -> [a]
binomials n =
   scanl (\acc (num,den) -> div (acc*num) den) 1 (zip [n, pred n ..] [1..n])

simplexLayoutSize :: Integral i => Int -> i -> i
simplexLayoutSize d n =
   case drop d $ binomials n of
      [] -> 0
      m:_ -> m

instance (Unary.Natural d, Shape.C size) => Shape.C (Simplex d size) where
   size (Simplex d sz) =
      simplexLayoutSize (integralFromProxy d) (Shape.size sz)

headSingletonFromProxy ::
   (Unary.Natural d) => UnaryProxy d -> Unary.HeadSingleton d
headSingletonFromProxy Proxy = Unary.headSingleton

predHeadSingleton :: Unary.HeadSingleton (Unary.Succ d) -> UnaryProxy d
predHeadSingleton Unary.Succ = Proxy

simplexIndices :: (Unary.Natural d) => UnaryProxy d -> [a] -> [FL.T d a]
simplexIndices d =
   case headSingletonFromProxy d of
      Unary.Zero -> const [FL.end]
      m@Unary.Succ -> \as -> do
         (a,ts) <- zip as $ NonEmpty.tail $ NonEmpty.tails as
         (a FL.!:) <$> simplexIndices (predHeadSingleton m) ts

instance
   (Unary.Natural d, Shape.Indexed size) =>
      Shape.Indexed (Simplex d size) where
   type Index (Simplex d size) = FL.T d (Shape.Index size)
   indices (Simplex d sz) = simplexIndices d $ Shape.indices sz
   inBounds (Simplex _d sz) ix =
      let ixs = Fold.toList ix
          getOffset = Shape.offset sz
      in all (Shape.inBounds sz) ixs &&
         and (ListHT.mapAdjacent (<) $ map getOffset ixs)
   unifiedSizeOffset (Simplex d sz) =
      let (n, getOffset) = Shape.unifiedSizeOffset sz
          dInt = integralFromProxy d
      in (simplexLayoutSize dInt n,
          -- cf. Combinatorics.chooseRank
          \ixs -> do
            ks <- Trav.traverse getOffset $ Fold.toList ixs
            return $
               simplexLayoutSize dInt n - 1
               -
               sum
                  (zipWith simplexLayoutSize
                     (iterate pred dInt) (map (n-1-) ks)))

instance
   (Unary.Natural d, Shape.InvIndexed size) =>
      Shape.InvIndexed (Simplex d size) where
   unifiedIndexFromOffset (Simplex d sh) k =
      let n = Shape.size sh in
      let dInt = integralFromProxy d in
      Trav.sequenceA $ snd $
      Trav.mapAccumL
         (\(a,k0) m ->
            case dropWhile ((<0) . snd) $
                  map (\bi -> (bi, k0 - simplexLayoutSize m (n-bi-1))) $
                  takeWhile (<n) $ iterate (1+) a of
               [] -> error "unifiedIndexFromOffset: offset out of range"
               (b,k1):_ -> ((b+1, k1), Shape.unifiedIndexFromOffset sh b))
         (0, simplexLayoutSize dInt n - 1 - k)
         (NonEmpty.init $ NonEmpty.scanl (-) dInt $ FL.repeat 1)
