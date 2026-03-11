module Combinatorics.Private where

import qualified PowerSeries
import Combinatorics.Utility (scalarProduct, )

import Data.Function.HT (nest, )
import Data.Maybe (mapMaybe, catMaybes, )
import Data.List.HT (tails, partition, removeEach, splitEverywhere, viewL, )
import Data.List
         (mapAccumL, intersperse, genericIndex, genericReplicate, genericTake, )

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.HT as Monad
import Control.Monad (MonadPlus, liftM, forM, guard, )


{- $setup
>>> import qualified Combinatorics.Private as CombPriv
>>> import Test.Combinatorics (genPermuteRep, genVariate, genChooseIndex)
>>>
>>> import qualified Test.QuickCheck as QC
>>> import Control.Applicative ((<$>))
>>> import Data.List.HT (allEqual)
>>> import Data.Eq.HT (equating)
>>>
>>> genChoose :: QC.Gen (Int, Int)
>>> genChoose = do
>>>    n <- QC.choose (0,15)
>>>    k <- QC.choose (-2,n)
>>>    return (n,k)
>>>
>>> genTuples :: QC.Gen (Int, [Char])
>>> genTuples = do
>>>    xs <- take 16 <$> QC.arbitrary
>>>    n <- QC.choose (-1, length xs + 1)
>>>    return (n,xs)
-}


replicateM :: (MonadPlus m) => Int -> m a -> m [a]
replicateM n m = guard (n>=0) >> Monad.replicate n m


{- |
prop> QC.forAll (take 6 <$> QC.arbitrary) $ \xs -> CombPriv.permuteRec xs == CombPriv.permuteMSL (xs::[Int])
-}
permuteRec :: [a] -> [[a]]
permuteRec =
   let go [] = [[]]
       go x = concatMap (\(y, ys) -> map (y:) (go ys)) (removeEach x)
   in  go

permuteMSL :: [a] -> [[a]]
permuteMSL xs = variateMSL (length xs) xs



runPermuteRep :: ([(a,Int)] -> [[a]]) -> [(a,Int)] -> [[a]]
runPermuteRep f xs =
   let (ps,ns) = partition ((>0) . snd) xs
   in  if any ((<0) . snd) ns
         then []
         else f ps

{- |
prop> QC.forAll (genPermuteRep 10) $ \xs -> CombPriv.permuteRep xs == CombPriv.permuteRepM xs
-}
permuteRep :: [(a,Int)] -> [[a]]
permuteRep =
   let go [] = [[]]
       go xs =
         concatMap (\(ys,(a,n),zs) ->
            let m = pred n
            in  map (a:) (go (ys ++ (m>0, (a, m)) ?: zs))) $
         filter (\(_,(_,n),_) -> n>0) $
         splitEverywhere xs
   in runPermuteRep go

permuteRepM :: [(a,Int)] -> [[a]]
permuteRepM =
   let go [] = [[]]
       go xs =
         do (ys,(a,n),zs) <- splitEverywhere xs
            let m = pred n
            liftM (a:) (go (ys ++ (m>0, (a, m)) ?: zs))
   in runPermuteRep go


infixr 5 ?:

(?:) :: (Bool, a) -> [a] -> [a]
(True,a)  ?: xs = a:xs
(False,_) ?: xs = xs


{- |
prop> QC.forAll genChoose $ \(n,k) -> allEqual $ CombPriv.chooseRec n k : CombPriv.chooseMSL n k : CombPriv.chooseMSL0 n k : []
-}
chooseRec :: Int -> Int -> [[Bool]]
chooseRec =
   let go n k =
         if k<0 || k>n
           then []
           else
             if n==0
               then [[]]
               else
                 map (False:) (go (pred n) k) ++
                 map (True:)  (go (pred n) (pred k))
   in go

chooseMSL :: Int -> Int -> [[Bool]]
chooseMSL n0 k0 =
   flip MS.evalStateT k0 $ fmap catMaybes $ sequence $
   intersperse (MS.StateT $ \k -> [(Just False, k), (Just True, pred k)]) $
   flip map [n0,n0-1..0] $ \n ->
   MS.gets (\k -> 0<=k && k<=n) >>= guard >> return Nothing

chooseMSL0 :: Int -> Int -> [[Bool]]
chooseMSL0 n0 k0 =
   flip MS.evalStateT k0 $ do
   count <-
      forM [n0,n0-1..1] $ \n ->
      MS.StateT $ \k ->
      guard (0<=k && k<=n) >> [(False, k), (True, pred k)]
   MS.gets (0==) >>= guard
   return count


{- |
prop> QC.forAll (QC.choose (-1,7)) $ \n -> QC.forAll genVariate $ \xs -> CombPriv.variateRep n xs == CombPriv.variateRepM n xs
-}
variateRep :: Int -> [a] -> [[a]]
variateRep n x =
   if n<0 then [] else nest n (\y -> concatMap (\z -> map (z:) y) x) [[]]

variateRepM :: Int -> [a] -> [[a]]
variateRepM = replicateM


{- |
prop> QC.forAll (QC.choose (-1,7)) $ \n -> QC.forAll genVariate $ \xs -> CombPriv.variateRec n xs == CombPriv.variateMSL n xs
-}
variateRec :: Int -> [a] -> [[a]]
variateRec =
   let go n =
         case compare n 0 of
            LT -> const []
            EQ -> const [[]]
            GT -> concatMap (\(y, ys) -> map (y:) (go (n-1) ys)) . removeEach
   in  go

variateMSL :: Int -> [a] -> [[a]]
variateMSL n = MS.evalStateT $ replicateM n $ MS.StateT removeEach



{- |
prop> QC.forAll genTuples $ \(n,xs) -> allEqual $ CombPriv.tuplesRec n xs : CombPriv.tuplesRec0 n xs : CombPriv.tuplesMSL n xs : CombPriv.tuplesMSL0 n xs : []
-}
tuplesRec :: Int -> [a] -> [[a]]
tuplesRec =
   let go r =
         case compare r 0 of
            LT -> const []
            EQ -> const [[]]
            GT -> concatMap (\(y:ys) -> map (y:) (go (r-1) ys)) . init . tails
   in  go

tuplesRec0 :: Int -> [a] -> [[a]]
tuplesRec0 =
   let go k =
         if k<0
           then const []
           else
             \ xt ->
             case xt of
                [] -> guard (k==0) >> [[]]
                x:xs -> map (x:) (go (pred k) xs) ++ go k xs
   in go

tuplesMSL :: Int -> [a] -> [[a]]
tuplesMSL n xs =
   flip MS.evalStateT xs $ replicateM n $
   MS.StateT $ mapMaybe viewL . tails

tuplesMSL0 :: Int -> [a] -> [[a]]
tuplesMSL0 n xs =
   flip MS.evalStateT xs $
   replicateM n $ do
      yl <- MS.get
      (y:ys) <- MT.lift $ tails yl
      MS.put ys
      return y


{- |
prop> QC.forAll genChooseIndex $ \(n,k,i) -> CombPriv.chooseUnrankRec n k i  ==  CombPriv.chooseUnrankList n k i
-}
chooseUnrankRec :: Integral a => a -> a -> a -> [Bool]
chooseUnrankRec =
   let go n 0 _ = genericReplicate n False
       go n k i =
          let n1 = pred n
              p = binomial n1 k
              b = i>=p
              (k1,i1) = if b then (pred k, i-p) else (k,i)
          in  b : go n1 k1 i1
   in go

chooseUnrankList :: Integral a => a -> a -> a -> [Bool]
chooseUnrankList n k0 i0 =
--   (\((0,0), xs) -> xs) $
   snd $
   mapAccumL
      (\(k,i) bins ->
          let p = genericIndex (bins++[0]) k
              b = i>=p
          in  (if b
                 then (pred k, i-p)
                 else (k, i),
               b))
      (k0,i0) $
   reverse $
   genericTake n binomials


binomial :: Integral a => a -> a -> a
binomial n k =
   let bino n' k' =
         if k'<0
           then 0
           else genericIndex (binomialSeq n') k'
   in  if n<2*k
         then bino n (n-k)
         else bino n k

binomialSeq :: Integral a => a -> [a]
binomialSeq n =
   {- this does not work because the corresponding numbers are not always divisible
    product (zipWith div [n', pred n' ..] [1..k'])
   -}
   scanl (\acc (num,den) -> div (acc*num) den) 1
         (zip [n, pred n ..] [1..n])


factorials :: Num a => [a]
factorials = scanl (*) 1 (iterate (+1) 1)

{-|
Pascal's triangle containing the binomial coefficients.
Only efficient if a prefix of all rows is required.
It is not efficient for picking particular rows
or even particular elements.
-}
binomials :: Num a => [[a]]
binomials =
   let conv11 x = zipWith (+) ([0]++x) (x++[0])
   in  iterate conv11 [1]


{- |
prop> allEqual $ map (take 1000) (CombPriv.derangementNumbersPS0 : CombPriv.derangementNumbersPS1 : CombPriv.derangementNumbersInclExcl : [] :: [[Integer]])
-}
derangementNumbersPS0 :: Num a => [a]
derangementNumbersPS0 =
   -- OEIS-A166: a(n) = n·a(n-1)+(-1)^n
   -- y(x) = 1/(1+x) + x · (t -> y(t)·t)'(x)
   let xs = PowerSeries.add
               (cycle [1,-1])
               (0 : PowerSeries.differentiate (0 : xs))
   in  xs

derangementNumbersPS1 :: Num a => [a]
derangementNumbersPS1 =
   -- OEIS-A166: a(n) = (n-1)·(a(n-1)+a(n-2))
   -- y(x) = 1 + x^2 · (t -> y(t)·(1+t))'(x)
   let xs = 1 : 0 : PowerSeries.differentiate (PowerSeries.add xs (0 : xs))
   in  xs

derangementNumbersInclExcl :: Num a => [a]
derangementNumbersInclExcl =
   let xs = zipWith (-) factorials (map (scalarProduct xs . init) binomials)
   in  xs


setPartitionNumbers :: Num a => [[a]]
setPartitionNumbers =
   -- s_{n+1,k} = s_{n,k-1} + k·s_{n,k}
   iterate (\x -> 0 : PowerSeries.add x (PowerSeries.differentiate x)) [1]


{- |
prop> equating (take 20) CombPriv.surjectiveMappingNumbersPS (CombPriv.surjectiveMappingNumbersStirling :: [[Integer]])
-}
surjectiveMappingNumbersPS :: Num a => [[a]]
surjectiveMappingNumbersPS =
   iterate
      (\x -> 0 : PowerSeries.differentiate (PowerSeries.add x (0 : x)))
      [1]

surjectiveMappingNumbersStirling :: Num a => [[a]]
surjectiveMappingNumbersStirling =
   map (zipWith (*) factorials) setPartitionNumbers
