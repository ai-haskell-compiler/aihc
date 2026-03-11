{-# LANGUAGE RebindableSyntax #-}
{- |
Copyright    :   (c) Henning Thielemann 2007
Maintainer   :   numericprelude@henning-thielemann.de
Stability    :   provisional
Portability  :   portable

Implementation of partial fractions.
Useful e.g. for fractions of integers and fractions of polynomials.

For the considered ring the prime factorization must be unique.
-}

module MathObj.PartialFraction where

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.IntegralDomain       as Integral
import qualified Number.Ratio                 as Ratio
import qualified Algebra.Ring                 as Ring
import qualified Algebra.Additive             as Additive
import qualified Algebra.ZeroTestable         as ZeroTestable
import qualified Algebra.Indexable            as Indexable

import Number.Ratio((%))
import Algebra.IntegralDomain(divMod, divModZero, decomposeVarPositionalInf)
import Algebra.Units(stdAssociate, stdUnitInv)
import Algebra.Field((/))
import Algebra.Ring((*), one, product)
import Algebra.Additive((+), zero, negate)
import Algebra.ZeroTestable (isZero)

import qualified Data.List.Reverse.StrictSpine as Rev
import qualified Data.List.Match as Match
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (group, sortBy, mapAccumR)
import Data.Maybe (fromMaybe)

import NumericPrelude.Base hiding (zipWith)

import NumericPrelude.Numeric(Int, fromInteger)


{- $setup
>>> import qualified MathObj.PartialFraction as PartialFraction
>>> import qualified MathObj.Polynomial.Core as PolyCore
>>> import qualified MathObj.Polynomial as Poly
>>> import qualified Algebra.PrincipalIdealDomain as PID
>>> import qualified Algebra.Indexable as Indexable
>>> import qualified Algebra.Laws as Laws
>>> import qualified Number.Ratio as Ratio
>>> import Test.NumericPrelude.Utility ((/\))
>>> import qualified Test.QuickCheck as QC
>>> import NumericPrelude.Numeric as NP
>>> import NumericPrelude.Base as P
>>> import Prelude ()
>>>
>>> import Control.Applicative (liftA2)
>>>
>>> {- |
>>> Generator of irreducible elements for tests.
>>> Choosing from a list of examples is a simple yet effective design.
>>> If we would construct irreducible elements by a clever algorithm
>>> we might obtain multiple primes only rarely.
>>> -} --
>>> genSmallPrime :: QC.Gen Integer
>>> genSmallPrime =
>>>    let primes = [2,3,5,7,11,13]
>>>    in  QC.elements (primes ++ map negate primes)
>>>
>>> genPartialFractionInt :: QC.Gen (PartialFraction.T Integer)
>>> genPartialFractionInt =
>>>    liftA2 PartialFraction.fromFactoredFraction
>>>       (QC.listOf genSmallPrime) QC.arbitrary
>>>
>>>
>>> genIrreduciblePolynomial :: QC.Gen (Poly.T Rational)
>>> genIrreduciblePolynomial = do
>>>    QC.NonZero unit <- QC.arbitrary
>>>    fmap (Poly.fromCoeffs . map (unit*)) $
>>>       QC.elements [[2,3],[2,0,1],[3,0,1],[1,-3,0,1]]
>>>
>>> genPartialFractionPoly :: QC.Gen (PartialFraction.T (Poly.T Rational))
>>> genPartialFractionPoly =
>>>    liftA2 PartialFraction.fromFactoredFraction
>>>       (fmap (take 3) $ QC.listOf genIrreduciblePolynomial)
>>>       (fmap (Poly.fromCoeffs . PolyCore.normalize . take 5) QC.arbitrary)
>>>
>>>
>>> fractionConv :: (PID.C a, Indexable.C a) => [a] -> a -> Bool
>>> fractionConv xs y =
>>>    PartialFraction.toFraction (PartialFraction.fromFactoredFraction xs y) ==
>>>    y % product xs
>>>
>>> fractionConvAlt :: (PID.C a, Indexable.C a) => [a] -> a -> Bool
>>> fractionConvAlt xs y =
>>>    PartialFraction.fromFactoredFraction xs y ==
>>>    PartialFraction.fromFactoredFractionAlt xs y
>>>
>>> scaleInt :: (PID.C a, Indexable.C a) => a -> PartialFraction.T a -> Bool
>>> scaleInt k a =
>>>    PartialFraction.toFraction (PartialFraction.scaleInt k a) ==
>>>    Ratio.scale k (PartialFraction.toFraction a)
>>>
>>> add, sub, mul ::
>>>    (PID.C a, Indexable.C a) =>
>>>    PartialFraction.T a -> PartialFraction.T a -> Bool
>>> add = Laws.homomorphism PartialFraction.toFraction (+) (+)
>>> sub = Laws.homomorphism PartialFraction.toFraction (-) (-)
>>> mul = Laws.homomorphism PartialFraction.toFraction (*) (*)
-}


{- |
@Cons z (indexMapFromList [(x0,[y00,y01]), (x1,[y10]), (x2,[y20,y21,y22])])@
represents the partial fraction
@z + y00/x0 + y01/x0^2 + y10/x1 + y20/x2 + y21/x2^2 + y22/x2^3@
The denominators @x0, x1, x2, ...@ must be irreducible,
but we can't check this in general.
It is also not enough to have relatively prime denominators,
because when adding two partial fraction representations
there might concur denominators that have non-trivial common divisors.
-}
data T a =
   Cons a (Map (Indexable.ToOrd a) [a])
      deriving (Eq)

{- |
Unchecked construction.
-}
fromFractionSum :: (Indexable.C a) => a -> [(a,[a])] -> T a
fromFractionSum z m =
   Cons z (indexMapFromList m)

toFractionSum :: (Indexable.C a) => T a -> (a, [(a,[a])])
toFractionSum (Cons z m) =
   (z, indexMapToList m)

appPrec :: Int
appPrec  = 10

instance (Show a) => Show (T a) where
  showsPrec p (Cons z m) =
    showParen (p >= appPrec)
       (showString "PartialFraction.fromFractionSum " .
        showsPrec (succ appPrec) z . showString " " .
        shows (indexMapToList m))


toFraction :: PID.C a => T a -> Ratio.T a
toFraction (Cons z m) =
   let fracs = map (uncurry multiToFraction) (indexMapToList m)
   in  foldl (+) (Ratio.fromValue z) fracs

{- |
'PrincipalIdealDomain.C' is not really necessary here and
only due to invokation of 'toFraction'.
-}
toFactoredFraction :: (PID.C a) => T a -> ([a], a)
toFactoredFraction x@(Cons _ m) =
   let r = toFraction x
       denoms = concat $ Map.elems $ indexMapMapWithKey (flip Match.replicate) m
       numer = foldl (flip Ratio.scale) r denoms
       {- From the theory it must be Ratio.denominator denom==1.
          We could check this dynamically, if there would be an Eq instance.
          We could omit this completely,
          if we would reimplement Ratio addition. -}
   in  (denoms, Ratio.numerator numer)

{- |
'PrincipalIdealDomain.C' is not really necessary here and
only due to invokation of 'Ratio.%'.
-}
multiToFraction :: PID.C a => a -> [a] -> Ratio.T a
multiToFraction denom =
   foldr (\numer acc ->
            (Ratio.fromValue numer + acc) / Ratio.fromValue denom) zero

hornerRev :: Ring.C a => a -> [a] -> a
hornerRev x = foldl (\val c -> val*x+c) zero


{- |
@fromFactoredFraction x y@
computes the partial fraction representation of @y % product x@,
where the elements of @x@ must be irreducible.
The function transforms the factors into their standard form
with respect to unit factors.

There are more direct methods for special cases
like polynomials over rational numbers
where the denominators are linear factors.

prop> QC.listOf genSmallPrime /\ fractionConv
prop> fmap (take 3) (QC.listOf genIrreduciblePolynomial) /\ fractionConv
-}
fromFactoredFraction :: (PID.C a, Indexable.C a) => [a] -> a -> T a
fromFactoredFraction denoms0 numer0 =
   let denoms = group $ sortBy Indexable.compare $ map stdAssociate denoms0
       numer  = foldl (*) numer0 $ map stdUnitInv denoms0
       denomPowers = map product denoms
          {- since the sub-lists contain the same value,
             the products are powers,
             which could be computed more efficiently -}
       partProdLeft         = scanl (*) one denomPowers
       (prod:partProdRight) = scanr (*) one denomPowers
       (intPart,numerRed) = divMod numer prod
       facs = List.zipWith (*) partProdLeft partProdRight
       numers =
          fromMaybe
             (error $ "PartialFraction.fromFactoredFraction: " ++
                      "denominators must be relatively prime")
             (PID.diophantineMulti numerRed facs)
       pairs = List.zipWith multiFromFraction denoms numers
       -- Is reduceHeads also necessary for polynomial partial fractions?
   in  removeZeros $ reduceHeads $ Cons intPart (indexMapFromList pairs)

{- |
prop> QC.listOf genSmallPrime /\ fractionConvAlt
prop> fmap (take 3) (QC.listOf genIrreduciblePolynomial) /\ fractionConvAlt
-}
fromFactoredFractionAlt :: (PID.C a, Indexable.C a) => [a] -> a -> T a
fromFactoredFractionAlt denoms numer =
   foldl (\p d -> scaleFrac (one%d) p) (fromValue numer) denoms

{- |
The list of denominators must contain equal elements.
Sorry for this hack.
-}
multiFromFraction :: PID.C a => [a] -> a -> (a,[a])
multiFromFraction (d:ds) n =
   (d, reverse $ decomposeVarPositionalInf ds n)
multiFromFraction [] _ =
   error "PartialFraction.multiFromFraction: there must be one denominator"

fromValue :: a -> T a
fromValue x = Cons x Map.empty


{- |
A normalization step which separates the integer part
from the leading fraction of each sub-list.
-}
reduceHeads :: Integral.C a => T a -> T a
reduceHeads (Cons z m0) =
   let m1 = indexMapMapWithKey (\x (y:ys) -> let (q,r) = divMod y x in (q,r:ys)) m0
   in  Cons
          (foldl (+) z (map fst $ Map.elems m1))
          (fmap snd m1)

{- |
Cf. Number.Positional
-}
carryRipple :: Integral.C a => a -> [a] -> (a,[a])
carryRipple b =
   mapAccumR (\carry y -> divMod (y+carry) b) zero


{- |
A normalization step which reduces all elements in sub-lists
modulo their denominators.
Zeros might be the result, that must be remove with 'removeZeros'.
-}
normalizeModulo :: Integral.C a => T a -> T a
normalizeModulo (Cons z0 m0) =
   let m1 = indexMapMapWithKey carryRipple m0
       -- would be nice to have a Map.unzip function
       ints = Map.elems $ fmap fst m1
   in  Cons (foldl (+) z0 ints) (fmap snd m1)



{- |
Remove trailing zeros in sub-lists
because if lists are converted to fractions by 'multiToFraction'
we must be sure that the denominator of the (cancelled) fraction
is indeed the stored power of the irreducible denominator.
Otherwise 'mulFrac' leads to wrong results.
-}
removeZeros :: (Indexable.C a, ZeroTestable.C a) => T a -> T a
removeZeros (Cons z m) =
   Cons z $ Map.filter (not . null) $ Map.map (Rev.dropWhile isZero) m


{-
instance Functor (T a) where
   fmap f (Cons x) = Cons (fmap f x)
-}

zipWith :: (Indexable.C a) => (a -> a -> a) -> ([a] -> [a] -> [a]) ->
   (T a -> T a -> T a)
zipWith opS opV (Cons za ma) (Cons zb mb) =
   Cons (opS za zb) (Map.unionWith opV ma mb)

{- |
prop> genPartialFractionInt /\ \x -> genPartialFractionInt /\ \y -> add x y
prop> genPartialFractionInt /\ \x -> genPartialFractionInt /\ \y -> sub x y

prop> genPartialFractionPoly /\ \x -> genPartialFractionPoly /\ \y -> add x y
prop> genPartialFractionPoly /\ \x -> genPartialFractionPoly /\ \y -> sub x y
-}
instance
   (Indexable.C a, Integral.C a, ZeroTestable.C a) =>
      Additive.C (T a) where
   a + b = removeZeros $ normalizeModulo $ zipWith (+) (+) a b
   {- This implementation is attracting but wrong.
     It fails if terms are present in b that are missing in a.
     Default implementation is better here.
     a - b = removeZeros $ normalizeModulo $ zipWith (-) (-) a b
   -}
   negate (Cons z m) = Cons (negate z) (fmap negate m)
   zero = fromValue zero

{- |
Transforms a product of two partial fractions
into a sum of two fractions.
The denominators must be at least relatively prime.
Since 'T' requires irreducible denominators,
these are also relatively prime.

Example: @mulFrac (1%6) (1%4)@ fails because of the common divisor @2@.
-}
mulFrac :: (PID.C a) => Ratio.T a -> Ratio.T a -> (a, a)
mulFrac x y =
   let dx = Ratio.denominator x
       dy = Ratio.denominator y
   in  fromMaybe
          (error "PartialFraction.mulFrac: denominators must be relatively prime")
          (PID.diophantine (Ratio.numerator x * Ratio.numerator y) dy dx)

{-
nx/dx * ny/dy = a/dx + b/dy
nx*ny = a*dy + b*dx
-}

mulFrac' :: (PID.C a) => Ratio.T a -> Ratio.T a -> (Ratio.T a, Ratio.T a)
mulFrac' x y =
   let (na,nb) = mulFrac x y
   in  (na % Ratio.denominator x, nb % Ratio.denominator y)

{-
Also works if the operands share a non-trivial divisor.

mulFracOverlap :: (PID.C a) =>
   Ratio.T a -> Ratio.T a -> ((Ratio.T a, Ratio.T a), Ratio.T a)
mulFracOverlap x y =
   let dx = Ratio.denominator x
       dy = Ratio.denominator y
       (g,(a0,b0)) = extendedGCD dy dx
       (q,r) = divModZero (Ratio.numerator x * Ratio.numerator y) g
   in  if (isZero r)
         then ((q*a, q*b), zero)
         else
           let fx = divChecked dx g
               fy = divChecked dy g
               (g,(k,c)) = extendedGCD (g^2) (fx*fy)

given dx=fx*g and dy=fy*g with fx and fy are relatively prime:
nx/(g*fx) * ny/(g*fy) = a/fx + b/fy + c/g^2
nx*ny = a*fy*g^2 + b*fx*g^2 + c*fx*fy
      = a*dy*g   + b*dx*g   + c*fx*fy
a0*dy + b0*dx = g
a=a0*k
b=b0*k

This approach does still fail on 1%2 * 1%4.
-}

{- |
Works always but simply puts the product into the last fraction.
-}
mulFracStupid :: (PID.C a) =>
   Ratio.T a -> Ratio.T a -> ((Ratio.T a, Ratio.T a), Ratio.T a)
mulFracStupid x y =
   let dx = Ratio.denominator x
       dy = Ratio.denominator y
       [a,b,c] =
          fromMaybe
             (error "PartialFraction.mulFracOverlap: (gcd 1 x) must always be a unit")
             (PID.diophantineMulti
                 (Ratio.numerator x * Ratio.numerator y) [dy, dx, one])
   in  ((a % dx, b % dy), c%(dx*dy))

{- |
Also works if the operands share a non-trivial divisor.
However the results are quite arbitrary.
-}
mulFracOverlap :: (PID.C a) =>
   Ratio.T a -> Ratio.T a -> ((Ratio.T a, Ratio.T a), Ratio.T a)
mulFracOverlap x y =
   let dx = Ratio.denominator x
       dy = Ratio.denominator y
       nx = Ratio.numerator x
       ny = Ratio.numerator y
       (g,(a,b)) = PID.extendedGCD dy dx
       (q,r) = divModZero (nx*ny) g
   in  (((q*a)%dx, (q*b)%dy), r%(dx*dy))


{- |
Expects an irreducible denominator as associate in standard form.
-}
scaleFrac :: (PID.C a, Indexable.C a) => Ratio.T a -> T a -> T a
scaleFrac s (Cons z0 m) =
   let ns = Ratio.numerator s
       ds = Ratio.denominator s
       dsOrd = Indexable.toOrd ds
       -- (z,zr) = Ratio.split (Ratio.scale z0 s)
       (z,zr) = divMod (z0*ns) ds
       scaleFracs =
          (\(scs,fracs) ->
             Map.insert dsOrd [foldl (+) zr scs] $
                indexMapFromList $
                   map (uncurry multiFromFraction) fracs) .
          unzip .
          map (\(dis,r) ->
                 let (sc,rc) = mulFrac s r
                 in  (sc, (dis, rc))) .
          Map.elems .
          indexMapMapWithKey
             (\d l -> (Match.replicate l d, multiToFraction d l))
   in  removeZeros $ reduceHeads $ Cons z
          (mapApplySplit dsOrd (+)
             (uncurry (:) . carryRipple ds . map (ns*))
             scaleFracs m)

{- |
prop> genPartialFractionInt /\ \x k -> scaleInt k x
prop> genPartialFractionPoly /\ \x k -> scaleInt k x
-}
scaleInt :: (PID.C a, Indexable.C a) => a -> T a -> T a
scaleInt x (Cons z m) =
   removeZeros $ normalizeModulo $
      Cons (x*z) (Map.map (map (x*)) m)


mul :: (PID.C a, Indexable.C a) => T a -> T a -> T a
mul (Cons z m) a =
   foldl
      (+) (scaleInt z a)
      (map (\(d,l) ->
              -- cf. to multiToFraction
              foldr (\numer acc ->
                 scaleFrac (one%d) (scaleInt numer a + acc)) zero l)
           (indexMapToList m))

{- |
prop> genPartialFractionInt /\ \x -> genPartialFractionInt /\ \y -> mul x y
prop> genPartialFractionPoly /\ \x -> genPartialFractionPoly /\ \y -> mul x y
-}
mulFast :: (PID.C a, Indexable.C a) => T a -> T a -> T a
mulFast pa pb =
   let ra = toFactoredFraction pa
       rb = toFactoredFraction pb
   in  fromFactoredFraction (fst ra ++ fst rb) (snd ra * snd rb)


instance (PID.C a, Indexable.C a) => Ring.C (T a) where
   one = fromValue one
   (*) = mulFast


{- * Helper functions for work with Maps with Indexable keys -}

indexMapMapWithKey :: (a -> b -> c)
                      -> Map (Indexable.ToOrd a) b
                      -> Map (Indexable.ToOrd a) c
indexMapMapWithKey f = Map.mapWithKey (f . Indexable.fromOrd)

indexMapToList :: Map (Indexable.ToOrd a) b -> [(a, b)]
indexMapToList = map (\(k,e) -> (Indexable.fromOrd k, e)) . Map.toList

indexMapFromList :: Indexable.C a => [(a, b)] -> Map (Indexable.ToOrd a) b
indexMapFromList = Map.fromList . map (\(k,e) -> (Indexable.toOrd k, e))

{- |
Apply a function on a specific element if it exists,
and another function to the rest of the map.
-}
mapApplySplit :: Ord a =>
   a -> (c -> c -> c) -> 
   (b -> c) -> (Map a b -> Map a c) -> Map a b -> Map a c
mapApplySplit key addOp f g m =
   maybe
      (g m)
      (\x -> Map.insertWith addOp key (f x) $ g (Map.delete key m))
      (Map.lookup key m)

