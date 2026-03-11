module Solve where

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.NonEmpty ((!:))
import Data.Traversable (traverse)
import Data.Tuple.HT (mapPair, mapFst)

import Control.Monad (liftM2, guard, mfilter)

{- $setup
>>> import qualified Solve
>>> import qualified Data.List as List
>>> import Data.Eq.HT (equating)
>>> import Control.Functor.HT (void)
>>> import Test.Utility
>>>    (solve, genOperands, genResult, genEquation,
>>>     normalizeSubExpr, normalizeSum)
>>> import qualified Test.QuickCheck as QC
-}


type NEList = NonEmpty.T []

{- |
The 'Ord' instance is needed for normalization in the tests
but does not refer to the value of the expression.
In order to prevent accidental use of '==' or '<'
we would have to implement those functions manually,
which is possible but a bit cumbersome.
-}
data SubExpr a = Number Integer | SubExpr a
   deriving (Eq, Ord, Show)
data Sum =
      Sum {positive :: NEList (SubExpr Product), negative :: [SubExpr Product]}
   deriving (Eq, Ord, Show)
data Product =
      Product {normal :: NEList (SubExpr Sum), reciprocal :: [SubExpr Sum]}
   deriving (Eq, Ord, Show)


split :: [a] -> [([a],[a])]
split =
   map (mapPair (map fst, map fst) . List.partition snd) .
   sequence . flip (ListHT.outerProduct (,)) [True, False]

neSplit :: NEList a -> [(NEList a, [a])]
neSplit (NonEmpty.Cons x xs) =
   map (mapFst (NonEmpty.Cons x)) $ split xs

splitMultiOrdered :: [a] -> [[NEList a]]
splitMultiOrdered [] = return []
splitMultiOrdered (x:xs) = do
   (ys,zs) <- neSplit (x!:xs)
   map (ys:) $ splitMultiOrdered zs

neSplitMultiOrdered :: [a] -> [(NEList (NEList a), [a])]
neSplitMultiOrdered xs = do
   (us,w,ws) <- ListHT.splitEverywhere xs
   (ys,zs) <- neSplit (w!:ws)
   map (mapPair (NonEmpty.cons ys, (us++))) $
      ([], zs) : map (mapFst NonEmpty.flatten) (neSplitMultiOrdered zs)


subExprsFromSet :: (NEList Integer -> [a]) -> NEList Integer -> [SubExpr a]
subExprsFromSet f xt@(NonEmpty.Cons x xs) =
   if null xs then [Number x] else map SubExpr $ f xt

topExprsFromSet :: [Integer] -> [SubExpr Sum]
topExprsFromSet xs = do
   y:ys <- fmap fst $ split xs
   flip subExprsFromSet (y!:ys) $ \yt ->
      exprsFromSet yt ++ map topProduct (exprsFromSet yt)

topProduct :: SubExpr Product -> Sum
topProduct zs = Sum (NonEmpty.singleton zs) []

anyExprsFromSet ::
   (Expression a) =>
   (NEList (SubExpr a) -> [SubExpr a] -> expr) ->
   NEList Integer -> [expr]
anyExprsFromSet cons xs = do
   (lhs,rs) <- neSplitMultiOrdered $ NonEmpty.flatten xs
   rhs <- splitMultiOrdered rs
   guard $ not $ null (NonEmpty.tail lhs) && null rhs
   liftM2 cons
      (traverse exprsFromSet lhs)
      (traverse exprsFromSet rhs)


class Expression expr where
   exprsFromSet :: NEList Integer -> [expr]
   format :: expr-> String
   eval :: expr -> Maybe Integer

instance (Expression a) => Expression (SubExpr a) where
   exprsFromSet = subExprsFromSet exprsFromSet
   format = formatSubExpr format
   eval (Number k) = Just k
   eval (SubExpr expr) = eval expr

instance Expression Sum where
   exprsFromSet = anyExprsFromSet Sum
   format (Sum pos neg) =
      let NonEmpty.Cons p ps = fmap format pos
          ns = map format neg
      in p ++ concatMap ("+"++) ps ++ concatMap ("-"++) ns
   eval (Sum pos neg) =
      mfilter (>=0) $
      liftM2 (-)
         (fmap NonEmpty.sum $ traverse eval pos)
         (fmap sum $ mapM eval neg)

instance Expression Product where
   exprsFromSet = anyExprsFromSet Product
   format (Product norm rec) =
      let NonEmpty.Cons n ns =
              fmap (formatSubExpr (addParen.format)) norm
          rs = map (formatSubExpr (addParen.format)) rec
      in n ++ concatMap ("*"++) ns ++ concatMap ("/"++) rs
   eval (Product norm rec) = do
      denom <- fmap product $ mapM eval rec
      guard $ denom/=0
      (q,r) <- fmap (flip divMod denom . NonEmpty.product) $ traverse eval norm
      guard $ r==0
      return q

formatSubExpr :: (a -> String) -> SubExpr a -> String
formatSubExpr _fmt (Number k) = show k
formatSubExpr fmt (SubExpr expr) = fmt expr

addParen :: String -> String
addParen str = "("++str++")"


{- |
>>> solve [25, 50, 75, 100, 3, 6] 952
25+6*75*(3+100)/50
(3*75*(6+100)-50)/25

>>> solve [75, 50, 2, 3, 8, 7] 812
50+(2+75)*(3+7)-8
2*7*(8+50)
...
50*(7+75)/(2+3)-8
(3+(2+75)/7)*(8+50)

>>> solve [100, 75, 50, 10, 5, 1] 102
1+100+5*10/50
1+100+50/5/10
...
(100+50/(75-5*10))/1
(100+(5+75)/(50-10))/1
(100+(75-5-50)/10)/1


prop> :{
   QC.forAll genOperands $ \xs ->
   QC.forAll (genResult xs) $ \x ->
      not $ null $ Solve.run (xs,x)
:}

prop> :{
   QC.forAll genOperands $ \xs ->
   QC.forAll (genResult xs) $ \x ->
   QC.forAll (QC.shuffle xs) $ \xs1 ->
      void (Solve.run (xs,x)) == void (Solve.run (xs1,x))
:}

prop> :{
   QC.forAll genOperands $ \xs ->
   QC.forAll (genResult xs) $ \x ->
   QC.forAll (QC.shuffle xs) $ \xs1 ->
      equating (List.sort . map (normalizeSubExpr normalizeSum))
         (Solve.run (xs,x)) (Solve.run (xs1,x))
:}

prop> :{
   QC.forAll genOperands $ \xs ->
   QC.forAll (genEquation xs) $ \(expr,x) ->
      elem expr $
      List.sort $ map (normalizeSubExpr normalizeSum) $ Solve.run (xs,x)
:}
-}
run :: ([Integer], Integer) -> [SubExpr Sum]
run (operands, result) =
   filter ((Just result ==) . Solve.eval) $ Solve.topExprsFromSet operands
