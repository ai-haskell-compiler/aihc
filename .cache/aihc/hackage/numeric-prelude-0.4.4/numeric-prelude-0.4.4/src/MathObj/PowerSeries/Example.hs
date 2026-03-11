{-# LANGUAGE RebindableSyntax #-}
module MathObj.PowerSeries.Example where

import qualified MathObj.PowerSeries.Core as PS

import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.ZeroTestable   as ZeroTestable
import qualified Algebra.Transcendental as Transcendental

import Algebra.Additive (zero, subtract, negate)

import Data.List (intersperse, )
import Data.List.HT (sieve, )

import NumericPrelude.Numeric (one, (*), (/),
                       fromInteger, {-fromRational,-} pi)
import NumericPrelude.Base -- (Bool, const, map, zipWith, id, (&&), (==))


{- $setup
>>> import qualified MathObj.PowerSeries.Core as PS
>>> import qualified MathObj.PowerSeries.Example as PSE
>>> import Test.NumericPrelude.Utility (equalTrunc)
>>> import NumericPrelude.Numeric as NP
>>> import NumericPrelude.Base as P
>>> import Prelude ()
-}


{- * Default implementations. -}

recip :: (Ring.C a) => [a]
recip = recipExpl

exp, sin, cos,
  log, asin, atan, sqrt :: (Field.C a) => [a]
acos :: (Transcendental.C a) => [a]
tan :: (ZeroTestable.C a, Field.C a) => [a]
exp = expODE
sin = sinODE
cos = cosODE
tan = tanExplSieve
log = logODE
asin = asinODE
acos = acosODE
atan = atanODE

sinh, cosh, atanh :: (Field.C a) => [a]
sinh  = sinhODE
cosh  = coshODE
atanh = atanhODE


-- | prop> \m n -> equalTrunc 30 (PS.mul (PSE.pow m) (PSE.pow n)) (PSE.pow (m+n))
pow :: (Field.C a) => a -> [a]
pow = powExpl
sqrt = sqrtExpl


{- * Generate Taylor series explicitly. -}

recipExpl :: (Ring.C a) => [a]
recipExpl = cycle [1,-1]

-- | prop> equalTrunc 500 PSE.expExpl PSE.expODE
expExpl :: (Field.C a) => [a]
expExpl = scanl (*) one PS.recipProgression
-- | prop> equalTrunc 500 PSE.sinExpl PSE.sinODE
sinExpl :: (Field.C a) => [a]
sinExpl = zero : PS.holes2alternate (tail expExpl)
-- | prop> equalTrunc 500 PSE.cosExpl PSE.cosODE
cosExpl :: (Field.C a) => [a]
cosExpl = PS.holes2alternate expExpl

-- | prop> equalTrunc 50 PSE.tanExpl PSE.tanODE
tanExpl :: (ZeroTestable.C a, Field.C a) => [a]
tanExpl = PS.divide sinExpl cosExpl
-- ignore zero values
-- | prop> equalTrunc 50 PSE.tanExpl PSE.tanExplSieve
tanExplSieve :: (ZeroTestable.C a, Field.C a) => [a]
tanExplSieve =
   concatMap
      (\x -> [zero,x])
      (PS.divide (sieve 2 (tail sin)) (sieve 2 cos))

-- | prop> equalTrunc 500 PSE.logExpl PSE.logODE
logExpl :: (Field.C a) => [a]
logExpl  = zero : PS.alternate       PS.recipProgression
-- | prop> equalTrunc 500 PSE.atanExpl PSE.atanODE
atanExpl :: (Field.C a) => [a]
atanExpl = zero : PS.holes2alternate PS.recipProgression

-- | prop> equalTrunc 500 PSE.sinhExpl PSE.sinhODE
sinhExpl :: (Field.C a) => [a]
sinhExpl  = zero : PS.holes2 (tail expExpl)
-- | prop> equalTrunc 500 PSE.coshExpl PSE.coshODE
coshExpl :: (Field.C a) => [a]
coshExpl  =        PS.holes2       expExpl
-- | prop> equalTrunc 500 PSE.atanhExpl PSE.atanhODE
atanhExpl :: (Field.C a) => [a]
atanhExpl = zero : PS.holes2 PS.recipProgression

{- * Power series of (1+x)^expon using the binomial series. -}

-- | prop> \expon -> equalTrunc 50 (PSE.powODE expon) (PSE.powExpl expon)
powExpl :: (Field.C a) => a -> [a]
powExpl expon =
   scanl (*) 1 (zipWith (/)
      (iterate (subtract 1) expon) PS.progression)

-- | prop> equalTrunc 100 PSE.sqrtExpl PSE.sqrtODE
sqrtExpl :: (Field.C a) => [a]
sqrtExpl = powExpl (1/2)

{- |
Power series of error function (almost).
More precisely @ erf = 2 \/ sqrt pi * integrate (\x -> exp (-x^2)) @,
with @erf 0 = 0@.
-}

erf :: (Field.C a) => [a]
erf = PS.integrate 0 $ intersperse 0 $ PS.alternate exp

{-
integrate (\x -> exp (-x^2/2)) :

erf = PS.integrate 0 $ intersperse 0 $
    snd $ mapAccumL (\twoPow c -> (twoPow/(-2), twoPow*c)) 1 exp
-}


{- * Generate Taylor series from differential equations. -}

{-
exp' x == exp x
sin' x == cos x
cos' x == - sin x

tan' x == 1 + tan x ^ 2
       == cos x ^ (-2)
-}

expODE, sinODE, cosODE, tanODE :: (Field.C a) => [a]
expODE = PS.integrate 1 expODE
sinODE = PS.integrate 0 cosODE
cosODE = PS.integrate 1 (PS.negate sinODE)
tanODE = PS.integrate 0 (PS.add [1] (PS.mul tanODE tanODE))
-- | prop> equalTrunc 50 PSE.tanODE PSE.tanODESieve
tanODESieve :: (Field.C a) => [a]
tanODESieve =
   -- sieve is too strict here because it wants to detect end of lists
   let tan2 = map head (iterate (drop 2) (tail tanODESieve))
   in  PS.integrate 0 (intersperse zero (1 : PS.mul tan2 tan2))

{-
log' (1+x) == 1/(1+x)
asin' x == acos' x == 1/sqrt(1-x^2)
atan' x == 1/(1+x^2)
-}

logODE, recipCircle, atanODE, sqrtODE :: (Field.C a) => [a]
logODE  = PS.integrate zero recip
recipCircle = intersperse zero (PS.alternate (powODE (-1/2)))
-- | prop> equalTrunc 50 PSE.asinODE (snd $ PS.inv PSE.sinODE)
asinODE :: (Field.C a) => [a]
asinODE = PS.integrate 0 recipCircle
atanODE = PS.integrate zero (cycle [1,0,-1,0])
sqrtODE = powODE (1/2)

acosODE :: (Transcendental.C a) => [a]
acosODE = PS.integrate (pi/2) recipCircle

sinhODE, coshODE, atanhODE :: (Field.C a) => [a]
sinhODE = PS.integrate 0 coshODE
coshODE = PS.integrate 1 sinhODE
atanhODE = PS.integrate zero (cycle [1,0])


{-
Power series for y with
   y x = (1+x) ** alpha
by solving the differential equation
   alpha * y x = (1+x) * y' x
-}

powODE :: (Field.C a) => a -> [a]
powODE expon =
   let y  = PS.integrate 1 y'
       y' = PS.scale expon (scanl1 subtract y)
   in  y
