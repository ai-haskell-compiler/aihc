{- |
Number type based on Float with formatting in percents.
-}
module Numeric.Probability.Percentage where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Random as Rnd

import Numeric.Probability.Show (showR)
import Numeric.Probability.Trace (Trace)

import Data.List.HT (padLeft)
import Data.Tuple.HT (mapFst)

import qualified System.Random as Random


-- ** Probabilities
newtype T = Cons Float
   deriving (Eq, Ord)


percent :: Float -> T
percent x = Cons (x/100)


showPfix :: (RealFrac prob, Show prob) => Int -> prob -> String
showPfix precision x =
   if precision==0
     then showR 3 (round (x*100) :: Integer) ++ "%"
     else
        let str =
               padLeft '0' (precision+1)
                  (show (round (x*10^(precision+2)) :: Integer))
            (int,frac) =
               splitAt (length str - precision) str
        in  padLeft ' ' 3 int ++ '.' : frac ++ "%"

{-# DEPRECATED roundRel "was used to implemented showPfix, but is no longer needed for this purpose, and should not be exported anyway, and does not contribute to a safe way to format fixed point values, because the rounded value may not be accurate" #-}
roundRel :: (RealFrac a) => Int -> a -> a
roundRel p x =
   let d = 10^p
   in  fromIntegral (round (x*d) :: Integer)/d

-- -- mixed precision
-- --
-- showP :: ProbRep -> String
-- showP f | f>=0.1    = showR 3 (round (f*100))++"%"
--         | otherwise = show (f*100)++"%"

-- fixed precision
--
-- showP :: ProbRep -> String
-- showP = showPfix 1


instance Show T where
   show (Cons p) = showPfix 1 p



infix 0 //

{- |
Print distribution as table with configurable precision.
-}
(//) :: (Ord a, Show a) => Dist a -> Int -> IO ()
(//) x prec = putStr (Dist.pretty (\(Cons p) -> showPfix prec p) x)

(//*) :: (Ord a, Show a) => Dist a -> (Int,Int) -> IO ()
(//*) x (prec,width) = putStr $ flip Dist.pretty x $
   \(Cons p) ->
      showPfix prec p ++ " " ++
      replicate (round (p * fromIntegral width)) '*'



liftP :: (Float -> Float) -> T -> T
liftP f (Cons x) = Cons (f x)

liftP2 :: (Float -> Float -> Float) -> T -> T -> T
liftP2 f (Cons x) (Cons y) = Cons (f x y)

instance Num T where
   fromInteger = Cons . fromInteger
   (+) = liftP2 (+)
   (-) = liftP2 (-)
   (*) = liftP2 (*)
   abs = liftP abs
   signum = liftP signum
   negate = liftP negate

instance Fractional T where
   fromRational = Cons . fromRational
   recip = liftP recip
   (/) = liftP2 (/)

instance Floating T where
   pi = Cons pi
   exp = liftP exp
   sqrt = liftP sqrt
   log = liftP log
   (**) = liftP2 (**)
   logBase = liftP2 logBase
   sin = liftP sin
   tan = liftP tan
   cos = liftP cos
   asin = liftP asin
   atan = liftP atan
   acos = liftP acos
   sinh = liftP sinh
   tanh = liftP tanh
   cosh = liftP cosh
   asinh = liftP asinh
   atanh = liftP atanh
   acosh = liftP acosh

instance Random.Random T where
   randomR (Cons l, Cons r) = mapFst Cons . Random.randomR (l,r)
   random = mapFst Cons . Random.random


type Dist a = Dist.T T a


type Spread a = [a] -> Dist a

type RDist a = Rnd.T (Dist a)

type Trans a = a -> Dist a

type Space a  = Trace (Dist a)
type Expand a = a -> Space a

type RTrans a = a -> RDist a

type RSpace a  = Rnd.T (Space a)
type RExpand a = a -> RSpace a
