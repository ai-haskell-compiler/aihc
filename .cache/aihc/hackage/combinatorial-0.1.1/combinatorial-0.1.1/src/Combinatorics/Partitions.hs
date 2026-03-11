module Combinatorics.Partitions (
   pentagonalPowerSeries,
   numPartitions,
   partitionsInc,
   partitionsDec,
   allPartitionsInc,

   propInfProdLinearFactors,
   propPentagonalPowerSeries,
   propPentagonalsDifP,
   propPentagonalsDifN,
   ) where

import qualified PowerSeries as PS
import Data.Eq.HT (equating)


{- $setup
>>> import qualified Combinatorics.Partitions as Parts
>>> import qualified Test.QuickCheck as QC
>>> import Data.List (genericLength)
>>> import Data.Eq.HT (equating)
-}


{-
  a(n) denotes the number in how many ways n can be presented as a sum of
  positive integers:
  a(n) n
    1  1 : 1
    2  2 : 2, 1+1
    3  3 : 3, 2+1, 1+1+1
    5  4 : 4, 3+1, 2+2, 2+1+1, 1+1+1+1
    7  5 : 5, 4+1, 3+2, 3+1+1, 2+2+1, 2+1+1+1, 1+1+1+1+1

  Number of partitions: http://oeis.org/A000041
  Pentagonal numbers: http://oeis.org/A001318
-}

{- |
Pentagonal numbers are used to simplify the infinite product
\\prod_{i>0} (1-t^i)
It is known that the coefficients of the power series
are exclusively -1, 0 or 1.
The following is a very simple but inefficient implementation,
because of many multiplications with zero.
-}
prodLinearFactors :: Int -> PS.T Integer
prodLinearFactors n =
   foldl PS.mul [1] $ take n $ map (1:) $ iterate (0:) [-1]

infProdLinearFactors :: PS.T Integer
infProdLinearFactors =
   zipWith (!!)
      (scanl (\prod i -> delayedSub prod i prod) [1] [1..])
      [0..]

{- |
prop> QC.forAll (QC.choose (0,100)) Parts.propInfProdLinearFactors
-}
propInfProdLinearFactors :: Int -> Bool
propInfProdLinearFactors n =
   and $
   take (n+1) $
   zipWith (==)
      infProdLinearFactors
      (prodLinearFactors n)


pentagonalsP, pentagonalsN,
  pentagonalsDifP, pentagonalsDifN :: [Int]

pentagonalsP = map (\n -> div (n*(3*n-1)) 2) [0..]
pentagonalsN = map (\n -> div (n*(3*n+1)) 2) [0..]

{-
  (n+1)*(3*n+2) - n*(3*n-1) = 6*n+2
  (n+1)*(3*n+4) - n*(3*n+1) = 6*n+4
-}
pentagonalsDifP = map (\n -> 3*n+1) [0..]
pentagonalsDifN = map (\n -> 3*n+2) [0..]

{- |
prop> Parts.propPentagonalsDifP 10000
-}
propPentagonalsDifP :: Int -> Bool
propPentagonalsDifP n =
   equating (take n)
      pentagonalsDifP (zipWith (-) (tail pentagonalsP) pentagonalsP)

{- |
prop> Parts.propPentagonalsDifN 10000
-}
propPentagonalsDifN :: Int -> Bool
propPentagonalsDifN n =
   equating (take n)
      pentagonalsDifN (zipWith (-) (tail pentagonalsN) pentagonalsN)

{-
  delay y by del and subtract it from x
-}
delayedSub :: [Integer] -> Int -> [Integer] -> [Integer]
delayedSub x del y =
   let (a,b) = splitAt del x
   in  a ++ PS.sub b y

{-
  p00 p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12 p13 p14 p15 p16 p17
 -    p00 p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12 p13 p14 p15 p16
 +                    p00 p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12
 -                                                p00 p01 p02 p03 p04 p05
  ...
 -        p00 p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12 p13 p14 p15
 +                            p00 p01 p02 p03 p04 p05 p06 p07 p08 p09 p10
 -                                                            p00 p01 p02
  ...
-}
numPartitions :: [Integer]
numPartitions =
   let accu = foldr (delayedSub numPartitions) (error "never evaluated")
       ps   = accu (tail pentagonalsDifP)
       ns   = accu (tail pentagonalsDifN)
   in  1 : zipWith (+) ps (0:ns)

{- |
This is a very efficient implementation of 'prodLinearFactors'.
-}
pentagonalPowerSeries :: [Integer]
pentagonalPowerSeries =
   let make = concat . zipWith (\s n -> s : replicate (n-1) 0) (cycle [1,-1])
   in  flip PS.sub [1] $
       PS.add
          (make pentagonalsDifP)
          (make pentagonalsDifN)

{- |
prop> Parts.propPentagonalPowerSeries 1000
-}
propPentagonalPowerSeries :: Int -> Bool
propPentagonalPowerSeries n =
   equating (take n) infProdLinearFactors pentagonalPowerSeries



{- | Give all partitions of the natural number n
     with summands which are at least k.
     Not quite correct for k>n. -}
partitionsInc :: (Integral a) => a -> a -> [[a]]
partitionsInc k n =
   concatMap (\y -> map (y:) (partitionsInc y (n-y))) [k .. div n 2] ++ [[n]]

partitionsDec :: (Integral a) => a -> a -> [[a]]
partitionsDec 0 0 = [repeat 0]
partitionsDec _ 0 = []
partitionsDec k n =
   (if k>=n then [[n]] else []) ++
      concatMap (\y -> map (y:) (partitionsDec y (n-y)))
                (takeWhile (>0) (iterate pred (min n k)))

_partitionsInc :: (Integral a) => a -> a -> [[a]]
_partitionsInc k n =
   if k>n
     then []
     else concatMap (\y -> map (y:) (_partitionsInc y (n-y))) [k..(n-1)]
            ++ [[n]]

{- |
type Int is needed because of list node indexing

prop> QC.forAll (QC.choose (1,10)) $ \k -> QC.forAll (QC.choose (0,50)) $ \n -> Parts.partitionsInc k n == Parts.allPartitionsInc !! k !! n
prop> equating (take 30) (map genericLength (Parts.allPartitionsInc !! 1)) Parts.numPartitions
-}
allPartitionsInc :: [[[[Int]]]]
allPartitionsInc =
   let part :: Int -> Int -> [[Int]]
       part k n = concatMap (\y -> map (y:) (xs !! y !! (n-y)))
                            [k .. div n 2]
                      ++ [[n]]
       xs = repeat [[]] : map (\k -> map (part k) [0..]) [1..]
   in  xs
