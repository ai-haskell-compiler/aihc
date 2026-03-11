module Test.Utility where

import qualified Solve

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.NonEmpty ((!:))
import Data.Tuple.Strict (zipWithPair)
import Data.Tuple.HT (mapPair)

import Control.Monad (guard)
import Control.Applicative ((<$>))

import qualified Test.QuickCheck as QC


newtype FormatMany a = FormatMany [a]

instance (Solve.Expression a) => Show (FormatMany a) where
   show (FormatMany xs) = unlines $ map Solve.format xs

solve :: [Integer] -> Integer -> FormatMany (Solve.SubExpr Solve.Sum)
solve =
   curry $
   FormatMany . List.sort . map (normalizeSubExpr normalizeSum) . Solve.run



type List1 = NonEmpty.T []
type List2 = NonEmpty.T List1

genResult :: [Integer] -> QC.Gen Integer
genResult xs0 =
   maybe (return 0) genResultNE $ NonEmpty.fetch xs0

genResultNE :: List1 Integer -> QC.Gen Integer
genResultNE (NonEmpty.Cons x0 xs0) =
   case NonEmpty.fetch xs0 of
      Nothing -> return x0
      Just xs1 -> do
         (ys,zs) <- genSplit $ x0!:xs1
         y <- genResultNE ys
         z <- genResultNE zs
         QC.elements $
            (y+z) :
            (y*z) :
            abs (y-z) :
            (let a = max y z; b = min y z in
             if b/=0 && mod a b == 0 then [div a b] else [])

genEquation :: [Integer] -> QC.Gen (Solve.SubExpr Solve.Sum, Integer)
genEquation xs0 =
   maybe (return (Solve.Number 0, 0)) genEquationNE $ NonEmpty.fetch xs0

genEquationNE :: List1 Integer -> QC.Gen (Solve.SubExpr Solve.Sum, Integer)
genEquationNE (NonEmpty.Cons x0 xs0) =
   case NonEmpty.fetch xs0 of
      Nothing -> return (Solve.Number x0, x0)
      Just xs1 -> do
         (ys,zs) <- genSplit $ x0!:xs1
         y <- genEquationNE ys
         z <- genEquationNE zs
         QC.elements $
            zipWithPair (addExpr, (+)) y z :
            zipWithPair (mulExpr, (*)) y z :
            (if snd y >= snd z
             then zipWithPair (subExpr, (-)) y z
             else zipWithPair (subExpr, (-)) z y) :
            (if snd y >= snd z then divEqu y z else divEqu z y)

addExpr ::
   Solve.SubExpr Solve.Sum -> Solve.SubExpr Solve.Sum -> Solve.SubExpr Solve.Sum
addExpr a b =
   case (sumFromSubExpr a, sumFromSubExpr b) of
      (Solve.Sum posA negA, Solve.Sum posB negB) ->
         Solve.SubExpr $
         Solve.Sum (mergeByNE (<) posA posB) (ListHT.mergeBy (<) negA negB)

subExpr ::
   Solve.SubExpr Solve.Sum -> Solve.SubExpr Solve.Sum -> Solve.SubExpr Solve.Sum
subExpr a b =
   case (sumFromSubExpr a, sumFromSubExpr b) of
      (Solve.Sum posA negA, Solve.Sum posB negB) ->
         Solve.SubExpr $
         Solve.Sum
            (mergeLeftByNE (<) posA negB) 
            (ListHT.mergeBy (<) negA $ NonEmpty.flatten posB)

sumFromSubExpr :: Solve.SubExpr Solve.Sum -> Solve.Sum
sumFromSubExpr (Solve.SubExpr a) = a
sumFromSubExpr (Solve.Number a) =
   Solve.Sum (NonEmpty.singleton $ Solve.Number a) []

mulExpr ::
   Solve.SubExpr Solve.Sum -> Solve.SubExpr Solve.Sum -> Solve.SubExpr Solve.Sum
mulExpr a b =
   case (productFromSubExpr a, productFromSubExpr b) of
      (Solve.Product normA recA, Solve.Product normB recB) ->
         Solve.SubExpr $ singletonSum $
         Solve.SubExpr $
            Solve.Product
               (mergeByNE (<) normA normB)
               (ListHT.mergeBy (<) recA recB)


divEqu ::
   (Solve.SubExpr Solve.Sum, Integer) ->
   (Solve.SubExpr Solve.Sum, Integer) ->
   [(Solve.SubExpr Solve.Sum, Integer)]
divEqu (exprA,resA) (exprB,resB) =
   guard (resB/=0 && mod resA resB == 0) >>
      [(divExpr exprA exprB, div resA resB)]

divExpr ::
   Solve.SubExpr Solve.Sum -> Solve.SubExpr Solve.Sum -> Solve.SubExpr Solve.Sum
divExpr a b =
   case (productFromSubExpr a, productFromSubExpr b) of
      (Solve.Product normA recA, Solve.Product normB recB) ->
         Solve.SubExpr $ singletonSum $
         Solve.SubExpr $
            Solve.Product
               (mergeLeftByNE (<) normA recB)
               (ListHT.mergeBy (<) recA $ NonEmpty.flatten normB)

productFromSubExpr :: Solve.SubExpr Solve.Sum -> Solve.Product
productFromSubExpr (Solve.Number a) = singletonProduct $ Solve.Number a
productFromSubExpr (Solve.SubExpr (Solve.Sum (NonEmpty.Cons expr []) [])) =
   case expr of
      Solve.SubExpr a -> a
      Solve.Number a -> singletonProduct $ Solve.Number a
productFromSubExpr a = singletonProduct a

singletonSum :: Solve.SubExpr Solve.Product -> Solve.Sum
singletonSum a = Solve.Sum (NonEmpty.singleton a) []

singletonProduct :: Solve.SubExpr Solve.Sum -> Solve.Product
singletonProduct a = Solve.Product (NonEmpty.singleton a) []

mergeByNE ::
   (a -> a -> Bool) -> NonEmpty.T [] a -> NonEmpty.T [] a -> NonEmpty.T [] a
mergeByNE lt (NonEmpty.Cons x xs) (NonEmpty.Cons y ys) =
   if lt x y
   then NonEmpty.Cons x $ ListHT.mergeBy lt xs (y:ys)
   else NonEmpty.Cons y $ ListHT.mergeBy lt (x:xs) ys

mergeLeftByNE ::
   (a -> a -> Bool) -> NonEmpty.T [] a -> [a] -> NonEmpty.T [] a
mergeLeftByNE lt xt@(NonEmpty.Cons x xs) yt =
   case yt of
      [] -> xt
      y:ys ->
         if lt x y
         then NonEmpty.Cons x $ ListHT.mergeBy lt xs (y:ys)
         else NonEmpty.Cons y $ ListHT.mergeBy lt (x:xs) ys

genSplit :: List2 a -> QC.Gen (List1 a, List1 a)
genSplit xs0 = do
   (x1,xs1) <-
      QC.elements $
      NonEmpty.flatten $ NonEmpty.flatten $ NonEmpty.removeEach xs0
   (x2,xs2) <- QC.elements $ NonEmpty.flatten $ NonEmpty.removeEach xs1
   (xsA,xsB) <-
      fmap (mapPair (map fst, map fst) . ListHT.partition snd) $
      mapM (\x -> (,) x <$> QC.arbitrary) xs2
   return (x1!:xsA, x2!:xsB)

genOperands :: QC.Gen [Integer]
genOperands =
   take 5 . map QC.getNonNegative . QC.getNonEmpty <$> QC.arbitrary



normalizeSubExpr ::
   (a -> a) -> Solve.SubExpr a -> Solve.SubExpr a
normalizeSubExpr normalize expr =
   case expr of
      Solve.Number k -> Solve.Number k
      Solve.SubExpr a -> Solve.SubExpr $ normalize a

normalizeSum :: Solve.Sum -> Solve.Sum
normalizeSum (Solve.Sum pos neg) =
   Solve.Sum
      (NonEmptyC.sort $ fmap (normalizeSubExpr normalizeProduct) pos)
      (List.sort $ map (normalizeSubExpr normalizeProduct) neg)

normalizeProduct :: Solve.Product -> Solve.Product
normalizeProduct (Solve.Product norm rec) =
   Solve.Product
      (NonEmptyC.sort $ fmap (normalizeSubExpr normalizeSum) norm)
      (List.sort $ map (normalizeSubExpr normalizeSum) rec)
