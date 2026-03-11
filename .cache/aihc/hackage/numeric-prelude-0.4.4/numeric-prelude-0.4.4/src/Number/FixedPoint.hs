{-# LANGUAGE RebindableSyntax #-}
{- |
Copyright   :  (c) Henning Thielemann 2006

Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Fixed point numbers.
They are implemented as ratios with fixed denominator.
Many routines fail for some arguments.
When they work,
they can be useful for obtaining approximations of some constants.
We have not paid attention to rounding errors
and thus some of the trailing digits may be wrong.
-}
module Number.FixedPoint where

import qualified Algebra.RealRing    as RealRing
import qualified Algebra.Transcendental as Trans
import qualified MathObj.PowerSeries.Example as PSE

import qualified Data.List.Reverse.StrictElement as Rev
import NumericPrelude.List (mapLast, )
import Data.Function.HT (powerAssociative, )
import Data.List.HT (padLeft)
import Data.Maybe.HT (toMaybe, )
import Data.List (transpose, unfoldr, )
import Data.Char (intToDigit, )

import NumericPrelude.Base
import NumericPrelude.Numeric hiding (recip, sqrt, exp, sin, cos, tan,
                              fromRational')

import qualified NumericPrelude.Numeric as NP


{- ** Conversion -}

{- ** other number types -}

fromFloat :: RealRing.C a => Integer -> a -> Integer
fromFloat den x =
   round (x * NP.fromInteger den)

-- | denominator conversion
fromFixedPoint :: Integer -> Integer -> Integer -> Integer
fromFixedPoint denDst denSrc x = div (x*denDst) denSrc


{- ** text -}

{- |
very efficient because it can make use of the decimal output of 'show'
-}
showPositionalDec :: Integer -> Integer -> String
showPositionalDec den = liftShowPosToInt $ \x ->
   let packetSize = 50  -- process digits in packets of this size
       basis = ringPower packetSize 10
       (int,frac) = toPositional basis den x
   in  show int ++ "." ++
          concat (mapLast (Rev.dropWhile ('0'==))
             (map (padLeft '0' packetSize . show) frac))

showPositionalHex :: Integer -> Integer -> String
showPositionalHex = showPositionalBasis 16

showPositionalBin :: Integer -> Integer -> String
showPositionalBin = showPositionalBasis 2

showPositionalBasis :: Integer -> Integer -> Integer -> String
showPositionalBasis basis den = liftShowPosToInt $ \x ->
   let (int,frac) = toPositional basis den x
   in  show int ++ "." ++ map (intToDigit . fromInteger) frac

liftShowPosToInt :: (Integer -> String) -> (Integer -> String)
liftShowPosToInt f n =
   if n>=0
     then       f   n
     else '-' : f (-n)

toPositional :: Integer -> Integer -> Integer -> (Integer, [Integer])
toPositional basis den x =
   let (int, frac) = divMod x den
   in  (int, unfoldr (\rm -> toMaybe (rm/=0) (divMod (basis*rm) den)) frac)


{- * Additive -}

add :: Integer -> Integer -> Integer -> Integer
add _ = (+)

sub :: Integer -> Integer -> Integer -> Integer
sub _ = (-)


{- * Ring -}

mul :: Integer -> Integer -> Integer -> Integer
mul den x y = div (x*y) den


{- * Field -}

divide :: Integer -> Integer -> Integer -> Integer
divide den x y = div (x*den) y

recip :: Integer -> Integer -> Integer
recip den x = div (den^2) x


{- * Algebra -}

{-
Newton's method for computing roots.
-}

magnitudes :: [Integer]
magnitudes =
   concat (transpose [iterate (^2) 4, iterate (^2) 8])

{-
Maybe we can speed up the algorithm
by calling sqrt recursively on deflated arguments.

ToDo:
The algorithm just computes floor(sqrt(den*x)).
We might factor out the algorithm for (floor.sqrt)
and move it to a different module
together with Fermat factors and so on.
-}
sqrt :: Integer -> Integer -> Integer
sqrt den x =
   let xden     = x*den
       initial  = fst (head (dropWhile ((<= xden) . snd)
                                (zip magnitudes (tail (tail magnitudes)))))
       approxs  = iterate (\y -> div (y + div xden y) 2) initial
       isRoot y = y^2 <= xden && xden < (y+1)^2
   in  head (dropWhile (not . isRoot) approxs)

-- bug: needs too long:  root (12::Int) (fromIntegerBase 10 1000 2)
root :: Integer -> Integer -> Integer -> Integer
root n den x =
   let n1       = n-1
       xden     = x * den^n1
       initial  = fst (head (dropWhile ((\y -> y^n <= xden) . snd)
                                (zip magnitudes (tail magnitudes))))
       approxs  = iterate (\y -> div (n1*y + div xden (y^n1)) n) initial
       isRoot y = y^n <= xden && xden < (y+1)^n
   in  head (dropWhile (not . isRoot) approxs)



{- * Transcendental -}

-- very simple evaluation by power series with lots of rounding errors
evalPowerSeries :: [Rational] -> Integer -> Integer -> Integer
evalPowerSeries series den x =
   let powers   = iterate (mul den x) den
       summands = zipWith (\c p -> round (c * fromInteger p)) series powers
   in  sum (map snd (takeWhile (\(c,s) -> s/=0 || c==0)
                               (zip series summands)))

cos, sin, tan :: Integer -> Integer -> Integer
cos = evalPowerSeries PSE.cos
sin = evalPowerSeries PSE.sin
-- tan will suffer from inaccuracies for small cosine
tan den x = divide den (sin den x) (cos den x)

-- it must abs x <= den
arctanSmall :: Integer -> Integer -> Integer
arctanSmall = evalPowerSeries PSE.atan

-- will fail for large inputs
arctan :: Integer -> Integer -> Integer
arctan den x =
   let estimate = fromFloat den
                     (Trans.atan (NP.fromRational' (x % den)) :: Double)
       tanEst   = tan den estimate
       residue  = divide den (x-tanEst) (den + mul den x tanEst)
   in  estimate + arctanSmall den residue

piConst :: Integer -> Integer
piConst den =
   let den4 = 4*den
       stArcTan k x = let d = k*den4 in arctanSmall d (div d x)
   in  {- formula 4 * (8 * arctan (1/10) - arctan (1/239) - 4 * arctan (1/515))
             from "Bartsch: Mathematische Formeln" -}
       -- (stArcTan 8 10 - stArcTan 1 239 - stArcTan 4 515)
       -- formula by Stoermer
       (stArcTan 44 57 + stArcTan 7 239 - stArcTan 12 682 + stArcTan 24 12943)


expSmall :: Integer -> Integer -> Integer
expSmall = evalPowerSeries PSE.exp

eConst :: Integer -> Integer
eConst den = expSmall den den

recipEConst :: Integer -> Integer
recipEConst den = expSmall den (-den)

exp :: Integer -> Integer -> Integer
exp den x =
   let den2 = div den 2
       (int,frac) = divMod (x + den2) den
       expFrac = expSmall den (frac-den2)
   in  case compare int 0 of
          EQ -> expFrac
          GT -> powerAssociative (mul den) expFrac (eConst      den)   int
          LT -> powerAssociative (mul den) expFrac (recipEConst den) (-int)
          -- LT -> nest (-int) (divide den e) expFrac


approxLogBase :: Integer -> Integer -> (Int, Integer)
approxLogBase base x =
   until ((<=base) . snd) (\(xE,xM) -> (succ xE, div xM base)) (0,x)

lnSmall :: Integer -> Integer -> Integer
lnSmall den x =
   evalPowerSeries PSE.log den (x-den)

-- uses Double's log for an estimate and dramatic speed up
ln :: Integer -> Integer -> Integer
ln den x =
   let fac = 10^50 {- A constant which is representable by Double
                      and which will quickly split our number it pieces
                      small enough for Double. -}
       (denE, denM) = approxLogBase fac den
       (xE,   xM)   = approxLogBase fac x
       approxDouble :: Double
       approxDouble =
          log (NP.fromInteger fac) * fromIntegral (xE-denE) +
          log (NP.fromInteger xM / NP.fromInteger denM)
       {- We convert first with respect to @fac@
          in order to keep in the range of Double values. -}
       approxFac = round (approxDouble * NP.fromInteger fac)
       approx    = fromFixedPoint den fac approxFac
       xSmall    = divide den x (exp den approx)
   in  add den approx (lnSmall den xSmall)
