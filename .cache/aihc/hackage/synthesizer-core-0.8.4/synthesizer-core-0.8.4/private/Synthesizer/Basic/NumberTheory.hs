{-# LANGUAGE NoImplicitPrelude #-}
{-
Some of these functions might be moved to NumericPrelude.

Wikipedia: (primitive) roots of unity modulo n
   (primitive) roots must be units and all units are (primitive) roots
   maximum possible order for primitive roots - Carmichael
   all possible orders: divisor of Carmichael (proof? statement already in Carmichael-function-article)
   sum of primitive roots that vanishes
   order of primitive root is a divisor of each possible exponent
      proof with GCD and diophantine in exponent
   check for primitive root: fast exponentiation,
      primitivity: check exponents that are prime divisors
   how to find a primitive root: just try
   sum of powers of a primitive root is zero
   number of primitive roots of given order
      g(n,k) > 0 if k|lambda(n)
      g(n,k) = 0 else
      g(n,1) = 1
      g(4,2) = 1
      g(2^n,2) = 3 for n>=3  ((-1) is always a square root of 1)
      g(2^n,2^k) = 2^k for k>=2 && k<n-1
      g(n,2) = 1 for n>=3 and n in http://oeis.org/A033948
      sum(g(n,k), k\in\N) = phi(n)
      There are only a few patterns that occur as rows of g,
      but a row of g (i.e. g(n)) does functionally depend on
      either lambda(n) nor phi(n)
      lambda(14) = 6   nozeros(g(14)) = [1,1,2,2]   (f ~ [1,2,3,6])
      lambda(21) = 6   nozeros(g(21)) = [1,3,2,6]   (f ~ [1,4,3,12])
      phi(13) = 12   nozeros(g(13)) = [1,1,2,2,2,4]   (f ~ [1,2,3,4,6,12])
      phi(21) = 12   nozeros(g(21)) = [1,3,2,6]       (f ~ [1,4,3,12])
      However length(nozeros(f(n))) = numberofdivisors(lambda(n))
      numberofdivisors=A000005
   number of roots of given order
      easier to compute
      k|m => f(n,k) | f(n,m)
      g(n,k) = f(n,k) - sum(f(n,d), d|k and k/d prime) + ...
         inclusion-exclusion-principle
      better to write the other round:
      f(n,k) = sum(g(n,d), d|k) - this is Dirichlet convolution
      RUNM says f(n,k) is multiplicative
         list it in multiplicative function
      f(n,1) = 1 for n>=2
      f(n,lambda(n)) = phi(n)
      f(n,a·b) = f(n,a)·f(n,b) if a and b are coprime (conjecture)
      f(n,lcm(a,b)) = lcm(f(n,a),f(n,b)) (conjecture)
      If this conjecture is true, then we only need to know f(n,p^i).
      The following conjecture is wrong:
         for prime p it is   f(n,p^i) = gcd(lambda(n),p^i)
      counterexamples
         f(8,2) = 4, lambda(8)=2
         f(63,3) = 9, lambda(63)=6
         f(275,5) = 25, lambda(275)=20
         f(1247,7) = 49, lambda(1247)=84
      It seems to be:
         for prime p it is   f(n,p^i) = p^j for some j
   How to find a modulus where there is a primitive root of order o?
      just try numbers from the sequence o+1, 2*o+1, 3*o+1
      Because of [[Dirichlet's theorem on arithmetic progressions]]
      you will somewhen find a prime p,
      and its Carmichael value is p-1, which is a multiple of o.
      In this ring even 'o' is a unit.
   How to find a modulus where there are primitive roots of orders o1,..,ok?
      Just search for a modulus with roots of order lcm(o1,...,ok).
      The smallest such modulus should also be the smallest one
      that has primitive roots of orders o1,..,ok?
      Proof: If a ring has primitive roots of orders o1,..,ok
      then all orders divide the carmichael value of that ring,
      thus lcm(o1,...,ok) divides the carmichael value of that ring,
      thus there is a primitive root of order lcm(o1,...,ok).
-}
module Synthesizer.Basic.NumberTheory (
   fermatFactors,
   uniquePrimeFactors,
   primeFactors,
   multiplicativeGenerator,
   Order (Order, getOrder),
   PrimitiveRoot(primitiveRootCandidates, maximumOrderOfPrimitiveRootsOfUnity),
   primitiveRootsOfUnity,
   lcmMulti,
   primitiveRootsOfUnityFullOrbit,
   primitiveRootsOfOrbit,
   hasPrimitiveRootOfUnityNaive,
   ordersOfPrimitiveRootsOfUnityTest,
   orderOfOrbit,
   hasPrimitiveRootOfUnityInteger,
   ordersOfPrimitiveRootsOfUnityInteger,
   ordersOfRootsOfUnityInteger,
   ordersOfRootsOfUnityIntegerCondensed,
   rootsOfUnityPower,
   ringsWithPrimitiveRootOfUnityAndUnit,
   ringsWithPrimitiveRootsOfUnityAndUnitsNaive,
   ringWithPrimitiveRootsOfUnityAndUnits,
   ringWithPrimitiveRootsOfUnity,
   is3Smooth,
   is5Smooth,
   numbers3Smooth,
   numbers5Smooth,
   ceilingPowerOfTwo,
   ceilingPower,
   ceilingLog,
   powerOfTwoFactors,
   divideByMaximumPower,
   ceiling3Smooth,
   ceiling5Smooth,
   isPrime,
   raderWorstCases,
   fastFourierRing,

   -- for testing
   multiplicativeGeneratorSet,
   multiplicativeGeneratorDivisors,
   primitiveRootsOfUnityPower,
   primitiveRootsOfUnityNaive,
   primitiveRootsOfUnityFullOrbitTest,
   maximumOrderOfPrimitiveRootsOfUnityNaive,
   maximumOrderOfPrimitiveRootsOfUnityInteger,
   divideByMaximumPowerRecursive,
   numbers3SmoothCorec,
   numbers3SmoothFoldr,
   numbers3SmoothSet,
   numbers5SmoothCorec,
   numbers5SmoothFoldr,
   numbers5SmoothSet,
   ceiling3SmoothScan,
   ceiling5SmoothScan,
   ceiling3SmoothNaive,
   ceiling5SmoothNaive,
   ceiling3SmoothTrace,
   ceiling5SmoothTrace,
   ) where

import qualified Synthesizer.State.Signal as SigS

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Algebra.Ring as Ring
import qualified Algebra.Units as Units
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.ZeroTestable as ZeroTestable

import qualified Number.ResidueClass.Check as RC
import Number.ResidueClass.Check ((/:), )

import qualified Number.FixedPoint as FP
import Data.Bits (Bits, (.&.), (.|.), shiftR, )

import qualified Data.List.HT as ListHT
import Data.List (unfoldr, mapAccumL, genericDrop, genericSplitAt, )
import Data.Tuple.HT (mapFst, mapSnd, mapPair, swap, )
import Data.Maybe.HT (toMaybe, )

import Test.QuickCheck (Arbitrary(arbitrary), )

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
The first pair member in @powerOfTwoFactors n@
is the maximum factor of @n@, that is a power of two.
-}
powerOfTwoFactors ::
   (Bits a, Integral.C a) => a -> (a, a)
powerOfTwoFactors n =
   let powerOfTwo = n .&. (-n)
   in  (powerOfTwo, div n powerOfTwo)


{- |
List all factorizations of an odd number
where the first factor is at most the second factor
and the first factors are in descending order.
-}
fermatFactors :: Integer -> [(Integer,Integer)]
fermatFactors n =
   let root = FP.sqrt 1 n
   in  map (\(a,b) -> (b-a,b+a)) $
       mergeAndFilter
          (zip (scanl (+) n [1,3..]) [0 .. div (n-1) 2])
          (zip (scanl (+) (root*root) $ iterate (2+) (2*root+1)) [root..])

mergeAndFilter :: (Ord a) => [(a,b)] -> [(a,c)] -> [(b,c)]
mergeAndFilter ((a0,b):a0s) ((a1,c):a1s) =
   case compare a0 a1 of
      LT -> mergeAndFilter a0s ((a1,c):a1s)
      GT -> mergeAndFilter ((a0,b):a0s) a1s
      EQ -> (b,c) : mergeAndFilter a0s a1s
mergeAndFilter _ _ = []



multiplicativeGenerator :: Integer -> Integer
multiplicativeGenerator = multiplicativeGeneratorDivisors

{- |
Argument must be a prime.
Usage of Set for efficient filtering of candidates seems to be overkill,
since the multiplicative generator seems to be small in most cases,
i.e. 2 or 3.

Smallest multiplicative generators for primes:
<http://oeis.org/A001918>

Especially large generators:
$ filter ((>31) . snd) $ map (\n -> (n, multiplicativeGenerator n)) $ tail NumberTheory.primes
[(36721,37),(48889,34),(51361,37),(55441,38),(63361,37),(64609,35),(71761,44),(88321,34),(92401,34),(93481,35),(95471,43),(97441,37),(104711,43),(110881,69)

$ filter ((>63) . snd) $ map (\n -> (n, multiplicativeGenerator n)) $ tail NumberTheory.primes
[(110881,69),(760321,73)

A solution with medium complexity
could at least observe the least 64 numbers using a Word64.
-}
multiplicativeGeneratorSet :: Integer -> Integer
multiplicativeGeneratorSet p =
   let search candidates =
          case Set.minView candidates of
             Nothing -> error $ show p ++ " is not a prime"
             Just (x,rest) ->
                case orbitSet $ orbit p x of
                   new ->
                      -- fromIntegral (Set.size new) == p-2
                      if new == Set.fromList [1..p-1]
                        then x
                        else search (Set.difference rest new)
   in  search $ Set.fromList [1..p-1]

multiplicativeGeneratorDivisors :: Integer -> Integer
multiplicativeGeneratorDivisors p =
   head $ primitiveRootsOfUnity p (Order $ p-1)


newtype Order = Order {getOrder :: Integer}
   deriving (Show, Eq, Ord)

instance Arbitrary Order where
   arbitrary = fmap (Order . (1+) . abs) arbitrary

instance Enum Order where
   succ (Order n) = Order (n+1)
   pred (Order n) = Order (n-1)
   fromEnum (Order n) = fromEnum n
   toEnum n = Order (toEnum n)
   enumFrom (Order from) =
      map Order $ enumFrom from
   enumFromThen (Order from) (Order thn) =
      map Order $ enumFromThen from thn
   enumFromTo (Order from) (Order to) =
      map Order $ enumFromTo from to
   enumFromThenTo (Order from) (Order thn) (Order to) =
      map Order $ enumFromThenTo from thn to

countOrder :: [a] -> Order
countOrder = foldl (\o _ -> succ o) (Order 0)

dividesOrder :: Order -> Order -> Bool
dividesOrder (Order k) (Order n) =
   divides k n


-- class Integral.C a => PrimitiveRoot a where
class PID.C a => PrimitiveRoot a where
   primitiveRootCandidates :: a -> [a]
   maximumOrderOfPrimitiveRootsOfUnity :: a -> Order

instance PrimitiveRoot Integer where
   primitiveRootCandidates modu = [1 .. modu-1]
   maximumOrderOfPrimitiveRootsOfUnity =
      maximumOrderOfPrimitiveRootsOfUnityInteger

{-
For 'ordersOfPrimitiveRootsOfUnityInteger'
and the connection to Euler's totient function
we have chosen to have

> primitiveRootsOfUnity m 1 == [1].
-}
primitiveRootsOfUnity ::
   (PrimitiveRoot a, Eq a) => a -> Order -> [a]
primitiveRootsOfUnity =
   primitiveRootsOfUnityPower

{-
First check, that element x is a root of unity.
If x is not primitive,
this means there is a non-maximal exponent y with x^y=1.
This y must be a divisor of order.
Thus it is enough to check all possibilities of order/q as exponents,
where q is a prime divisor of order.
Computing a single power is much faster
than computing all powers up to the maximum power.

Verifying that a ring has no primitive root of the wanted order
takes a long time if we do it by exhaustive search.
In the case of a=Integer we could first check,
whether the considered residue ring has a primitive root of wanted order
using the Carmichael function.
We could certainly count the number of primitive roots
and stop searching if we reach that count.
-}
primitiveRootsOfUnityPower ::
   (PrimitiveRoot a, Eq a) => a -> Order -> [a]
primitiveRootsOfUnityPower modu (Order order) =
   let greatDivisors = map (div order) $ uniquePrimeFactors order
   in  filter
          (\n ->
             let pow y = RC.representative $ (n /: modu) ^ y
             in  PID.coprime n modu
                 &&
                 pow order == one
                 &&
                 all (\y -> pow y /= one) greatDivisors) $
       primitiveRootCandidates modu

primitiveRootsOfUnityNaive ::
   (PrimitiveRoot a, Eq a) => a -> Order -> [a]
primitiveRootsOfUnityNaive _ (Order 0) = []
primitiveRootsOfUnityNaive modu (Order expo) =
   filter
      (\n ->
         let (prefix,end:_) =
                genericSplitAt (expo-1) $ SigS.toList $ orbit modu n
         in  all (1/=) prefix && end==1) $
   primitiveRootCandidates modu

orbitSet :: Ord a => SigS.T a -> Set.Set a
orbitSet list =
   SigS.foldR
      (\new cont seen ->
         if Set.member new seen
           then seen
           else cont (Set.insert new seen))
      id list Set.empty

orbit :: (Integral.C a) => a -> a -> SigS.T a
orbit p x = SigS.iterate (\y -> mod (x*y) p) x


{- |
Does not emit values in ascending order
and may return duplicates (e.g. primitiveRootsOfUnityFullOrbit 70000 10).
I hoped it would be faster than the other implementations
since it eliminates non-roots in large blocks.
However it seems that managing the root candidates in a Set
reduces performance significantly.

The idea:
Start with a seed that is a unit.
Compute its orbit until a one is reached.
Since it is a unit, we always encounter a one.
We do not need to check for non-unit seeds,
since (gcd modu seed) will be a divisor of all seed powers.
In the orbit all numbers are powers of each other.
Now finding the roots is a matter of solving
a Diophantine equation of the exponents.
In one such step all powers in an orbit are classified as roots or non-roots
and thus we can remove them all from the set of root candidates
and continue with the remaining candidates.
Duplicates can occur if a seed
in a later iteration is found again as power of another seed.
-}
primitiveRootsOfUnityFullOrbit ::
   (PrimitiveRoot a, Ord a) => a -> Order -> [a]
primitiveRootsOfUnityFullOrbit modu expo =
   let search candidates =
          flip fmap (Set.minView candidates) $ \(x,rest) ->
          mapSnd (Set.difference rest . Set.fromList) $
          primitiveRootsOfOrbit modu expo x
   in  concat $ unfoldr search $ Set.fromList $
       -- needed for modules with many divisors
       filter (PID.coprime modu) $
       primitiveRootCandidates modu

primitiveRootsOfUnityFullOrbitTest ::
   (PrimitiveRoot a, Ord a) => a -> Order -> [(a,[a])]
primitiveRootsOfUnityFullOrbitTest modu expo =
   let search candidates =
          flip fmap (Set.minView candidates) $ \(x,rest) ->
          mapPair ((,) x,
                   Set.difference rest . Set.fromList) $
          primitiveRootsOfOrbit modu expo x
   in  unfoldr search $ Set.fromList $
       filter (PID.coprime modu) $
       primitiveRootCandidates modu

primitiveRootsOfOrbit ::
   (PrimitiveRoot a, Ord a) => a -> Order -> a -> ([a], [a])
primitiveRootsOfOrbit modu (Order expo) x =
   let orb = (1:) $ takeWhile (1/=) $ iterate (\y -> mod (x*y) modu) x
       (Order orbitSize) = countOrder orb
   in  (if expo==0
          then []
          else
            {-
            size = length orb
            Search for m and k with 0<k and 0<m and m<size
            and expo*m = size*k
            such that for all l with 0<l and l<k
            m does not divide size*l.
            To this end we ask for every m
            for the smallest r such that size divides r*m.
            If r=expo then x^m is a primitive root of order expo.
            If r<expo then x^m has order smaller than expo.
            The searched r is div size (gcd size m).
            However expo = div size (gcd size m) implies,
            that expo is a divisor of size.
                expo = div size (gcd size m)
             => div size expo = gcd size m
                s = gcd size m
            We have to consider for m
            only the multiples of s.
            Then divide both sides of the equation by s, yielding
                1 = gcd expo m'
            -}
            case divMod orbitSize expo of
               (s,0) ->
                  map snd $ filter (PID.coprime expo . fst) $
                  zip
                     [0 .. expo-1]
                     -- (ListHT.sieve s $ orb)
                     (map head $ iterate (genericDrop s) orb)
               _ -> [],
        orb)


hasPrimitiveRootOfUnityNaive ::
   (PrimitiveRoot a, Ord a) => a -> Order -> Bool
hasPrimitiveRootOfUnityNaive modu expo =
   any (dividesOrder expo . snd) $
   ordersOfPrimitiveRootsOfUnityTest modu

{-
This should be a maximum both with respect to magnitude and to divisibility.
-}
maximumOrderOfPrimitiveRootsOfUnityNaive ::
   (PrimitiveRoot a, Ord a) => a -> Order
maximumOrderOfPrimitiveRootsOfUnityNaive =
   foldl max (Order 1) . map snd . ordersOfPrimitiveRootsOfUnityTest

{- |
Computes a list of seeds and according maximum orders of roots of unity.
All divisors of those maximum orders are possible orders of roots of unity, too.
-}
ordersOfPrimitiveRootsOfUnityTest ::
   (PrimitiveRoot a, Ord a) => a -> [(a, Order)]
ordersOfPrimitiveRootsOfUnityTest modu =
   let search candidates =
          flip fmap (Set.minView candidates) $ \(x,rest) ->
          mapPair ((,) x,
                   Set.difference rest . Set.fromList) $
          orderOfOrbit modu x
   in  unfoldr search $ Set.fromList $
       filter (PID.coprime modu) $
       primitiveRootCandidates modu

{- |
modu and x must be coprime.
If they are not,
then none of the numbers in the orbit is a root of unity
and the function enters an infinite loop.
-}
orderOfOrbit ::
   (PrimitiveRoot a, Ord a) => a -> a -> (Order, [a])
orderOfOrbit modu x =
   let cyc = takeWhile (one/=) $ SigS.toList $ orbit modu x
   in  (succ $ countOrder cyc, cyc)


{-
This test speeds up 'hasPrimitiveRootOfUnityNaive' considerably
by considering the prime factors of modu.
If modu is a prime, then the ring has a multiplicative generator,
i.e. a primitive root of unity of order modu-1.
That is, all primitive roots of unity are of an order that divides modu-1.
It seems that if modu is a power of a prime,
then the according ring has also multiplicative generator.
I think this is the reason for generalising the Rader transform
to signals of prime power length.
-}
hasPrimitiveRootOfUnityInteger ::
   Integer -> Order -> Bool
hasPrimitiveRootOfUnityInteger modu expo =
   dividesOrder expo $
   maximumOrderOfPrimitiveRootsOfUnityInteger modu

{-
Carmichael theorem:
If the integer residue rings with coprime moduli m0 and m1
have primitive roots of maximum order o0 and o1, respectively,
then the integer ring with modulus m0*m1
has maximum order (lcm o0 o1).
-}

{-
This is the Carmichael function.
<http://oeis.org/A002322>
-}
maximumOrderOfPrimitiveRootsOfUnityInteger ::
   Integer -> Order
maximumOrderOfPrimitiveRootsOfUnityInteger =
   Order .
   lcmMulti .
   map
      (\(e,p) ->
         if p == 2 && e > 2
           then p^(e-2)
           else p^(e-1) * (p-1)) .
   map (mapFst fromIntegral) .
   primeFactors


{-
The sum of the sub-lists should equal the Euler totient function values
<http://oeis.org/A000010>.
-}
ordersOfPrimitiveRootsOfUnityInteger :: [[Int]]
ordersOfPrimitiveRootsOfUnityInteger =
   flip map [1..] $ \modu ->
   let maxOrder = maximumOrderOfPrimitiveRootsOfUnity (modu::Integer)
   in  map (length . primitiveRootsOfUnityPower modu) $
--       filter (flip divides maxOrder) $
       [Order 1 .. maxOrder]

ordersOfRootsOfUnityInteger :: [[Int]]
ordersOfRootsOfUnityInteger =
   flip map [1..] $ \modu ->
   map (length . rootsOfUnityPower (modu::Integer)) $
   [Order 1 ..]
{-
mapM_ print $ map (\n -> (n, maximumOrderOfPrimitiveRootsOfUnityInteger (fromIntegral n), take 30 $ ordersOfRootsOfUnityInteger !! (n-1))) [2..30]

mapM_ print $ map (\n -> (n, maximumOrderOfPrimitiveRootsOfUnityInteger (fromIntegral n), let row = ordersOfRootsOfUnityInteger !! (n-1) in map (row!!) $ map pred $ take 10 $ iterate (2*) 1)) [2..30]
-}

ordersOfRootsOfUnityIntegerCondensed :: [[Int]]
ordersOfRootsOfUnityIntegerCondensed =
   flip map [1..] $ \modu ->
   let maxOrder = maximumOrderOfPrimitiveRootsOfUnity (modu::Integer)
   in  map (length . rootsOfUnityPower modu) $
--       filter (flip divides maxOrder) $
       [Order 1 .. maxOrder]

rootsOfUnityPower ::
   (PrimitiveRoot a, Eq a) => a -> Order -> [a]
rootsOfUnityPower modu (Order expo) =
   filter
      (\n ->
         PID.coprime n modu
         &&
         RC.representative ((n /: modu) ^ expo) == one) $
   primitiveRootCandidates modu

{-
Corollary from the Carmichael function properties:
If in Z_n there is a primitive root r of unity of order o,
then for every Z_{m \cdot n} there is also a primitive root of unity
with the same order.

Collary:
If in Z_n1 you have a primitive root of order o1,
and so on for Z_{n_k} and order ok,
then Z_{lcm(n1,...,nk)} has primitive roots for every of the order o1,...,on.

Conjecture:
If Z_n has a total number of m primitive roots of unity of order o,
then Z_{k*n} has at least m primitive roots of unity of order o.

Conjecture:
Precondition: In Z_n there is a primitive root of prime order o.
Claims:
a) There are natural numbers m and k with n = m*(k*o+1) or n = m*o.
b) The smallest such n is of the form k*o+1 with k>1.

Counterexample for a) and non-prime o: o=15, n=77
Counterexample for b) and non-prime o:
   o=20, n=25; o=27, n=81; o=54, n=81; o=55, n=121

Corollary from definition of Carmichael function:
For n>1, Z_{2^{n+2}} has primitive root of unity of order 2^n.
-}

{- |
Given an order find integer residue rings
where roots of unity of this order exist.
The way they are constructed also warrants,
that 'order' is a unit (i.e. invertible) in those rings.

The list is not exhaustive
but computes suggestions quickly.
The first found modulus is often the smallest one that exist,
but not always (smallest counter-example: Order 80).
However, the first modulus is not the smallest one
among the ones that only have the wanted primitive root,
but where 'order' is not necessarily a unit.
E.g.

> ringsWithPrimitiveRootOfUnityAndUnit 840 == 2521 : 3361 : ...

but the smallest modulus is 1763.

Most of the numbers are primes.
For these the recursion property of the Carmichael function
immediately yields that there are primitive roots of unity of order 'order'.
-}
ringsWithPrimitiveRootOfUnityAndUnit :: Order -> [Integer]
ringsWithPrimitiveRootOfUnityAndUnit order@(Order k) =
   filter (flip hasPrimitiveRootOfUnityInteger order) $
   iterate (k+) 1


ringsWithPrimitiveRootsOfUnityAndUnitsNaive :: [Order] -> [Integer] -> [Integer]
ringsWithPrimitiveRootsOfUnityAndUnitsNaive rootOrders units =
   filter
      (\n ->
         all (hasPrimitiveRootOfUnityInteger n) rootOrders &&
         all (PID.coprime n) units)
      [1..]


{-
It would be nice to have the Omega monad here
in order to enumerate all rings.
-}
ringWithPrimitiveRootsOfUnityAndUnits :: [Order] -> [Integer] -> Integer
ringWithPrimitiveRootsOfUnityAndUnits rootOrders units =
   let p = lcmMulti units
   in  lcmMulti $
       map (head . filter (PID.coprime p) .
            ringsWithPrimitiveRootOfUnityAndUnit) $
       rootOrders

{-
Search for an appriopriate modulus
using the recursive definition of the Carmichael function.
We chose the prime factors of the Carmichael function argument
such that we get at least the prime factors in the function value that we need.

The modulus constructed this way is usually not the smallest possible
and it also does not provide that 'n' is a unit in the residue ring.
The simpler function 'ringsWithPrimitiveRootOfUnityAndUnit'
will usually produce a smaller modulus.
-}
ringWithPrimitiveRootsOfUnity :: Order -> Integer
ringWithPrimitiveRootsOfUnity (Order n) =
   case n of
      0 -> 2
      _ ->
         product $ map (uncurry ringPower) $ snd $
         mapAccumL
            (\factors (e,p) ->
               if Map.findWithDefault 0 p factors >= e
                 then (factors, (0,p))
                 else
                   if p==2
                     then
                       (factors,
                        case e of
                           0 -> (0,2)
                           1 -> (1,3)
                           2 -> (1,5)
                           _ -> (e+2, 2))
                     else
                       (Map.unionWith max factors $
                           Map.fromList $ map swap $ primeFactors $ p-1,
                        (e+1, p)))
            Map.empty $
         reverse $ primeFactors $ lcmMulti $
         n : map (subtract 1) (partialPrimes n)

lcmMulti :: (PID.C a) => [a] -> a
lcmMulti = foldl lcm one


{- |
List all numbers that only contain prime factors 2 and 3 in ascending order.
<http://oeis.org/A003586>
-}
numbers3Smooth :: [Integer]
numbers3Smooth = numbers3SmoothCorec

numbers3SmoothCorec :: [Integer]
numbers3SmoothCorec = mergePowers 3 $ iterate (2*) 1

mergePowers :: (Ord a, Ring.C a) => a -> [a] -> [a]
mergePowers _ [] = []
mergePowers p (x:xs) =
   let ys = x : ListHT.mergeBy (<=) xs (map (p*) ys)
   in  ys

numbers3SmoothFoldr :: [Integer]
numbers3SmoothFoldr =
   foldr
      (\(x0:x1:xs) ys -> x0 : x1 : ListHT.mergeBy (<=) xs ys)
      (error "numbers3SmoothFoldr: infinite list should not have an end") $
   iterate (map (3*)) $
   iterate (2*) 1

numbers3SmoothSet :: [Integer]
numbers3SmoothSet =
   unfoldr
      (fmap (\(m,rest) -> (m, Set.union rest $ Set.fromAscList [2*m,3*m])) .
       Set.minView) $
   Set.singleton 1


{-
Hamming sequence
<http://oeis.org/A051037>
-}
numbers5Smooth :: [Integer]
numbers5Smooth = numbers5SmoothCorec

numbers5SmoothCorec :: [Integer]
numbers5SmoothCorec =
   if False
     then -- causes permanent storage of numbers3SmoothCorec
          mergePowers 5 $ numbers3SmoothCorec
     else mergePowers 5 $ mergePowers 3 $ iterate (2*) 1

numbers5SmoothFoldr :: [Integer]
numbers5SmoothFoldr =
   foldr
      (\(x0:x1:x2:xs) ys -> x0 : x1 : x2 : ListHT.mergeBy (<=) xs ys)
      (error "numbers5SmoothFoldr: infinite list should not have an end") $
   iterate (map (5*)) $
   numbers3SmoothFoldr

numbers5SmoothSet :: [Integer]
numbers5SmoothSet =
   unfoldr
      (fmap (\(m,rest) -> (m, Set.union rest $ Set.fromAscList [2*m,3*m,5*m])) .
       Set.minView) $
   Set.singleton 1

ceilingPowerOfTwo :: (Ring.C a, Bits a) => a -> a
ceilingPowerOfTwo 0 = 1
ceilingPowerOfTwo n =
   (1+) $ fst $ head $
   dropWhile (uncurry (/=)) $
   ListHT.mapAdjacent (,) $
   scanl (\m d -> shiftR m d .|. m) (n-1) $
   iterate (2*) 1

{- |
It's not awfully efficient, but ok for our uses.
-}
ceilingPower :: (Integral.C a, Ord a) => a -> a -> a
ceilingPower base n = base ^ fromIntegral (ceilingLog base n)

ceilingLog :: (Integral.C a, Ord a) => a -> a -> Int
ceilingLog base =
   length . takeWhile (>0) . iterate (flip div base) . subtract 1

divideByMaximumPower ::
   (Integral.C a, ZeroTestable.C a) => a -> a -> a
divideByMaximumPower b n =
   last $
   n : unfoldr (\m -> case divMod m b of (q,r) -> toMaybe (isZero r) (q,q)) n

divideByMaximumPowerRecursive ::
   (Integral.C a, Eq a, ZeroTestable.C a) => a -> a -> a
divideByMaximumPowerRecursive b =
   let recourse n =
          case divMod n b of
             (q,0) -> recourse q
             _ -> n
   in  recourse

getMaximumExponent ::
   (Integral.C a, ZeroTestable.C a) =>
   a -> a -> (Int,a)
getMaximumExponent b n =
   last $ (0,n) :
   unfoldr
      (\(e,m) ->
         let (q,r) = divMod m b
             eq = (e+1,q)
         in  toMaybe (isZero r) (eq,eq))
      (0,n)

is3Smooth :: Integer -> Bool
is3Smooth =
   (1==) .
   divideByMaximumPower 3 .
   divideByMaximumPower 2

is5Smooth :: Integer -> Bool
is5Smooth =
   (1==) .
   divideByMaximumPower 5 .
   divideByMaximumPower 3 .
   divideByMaximumPower 2


ceiling3Smooth :: Integer -> Integer
ceiling3Smooth = ceiling3SmoothTrace

ceiling5Smooth :: Integer -> Integer
ceiling5Smooth = ceiling5SmoothTrace

{- |
Compute the smallest composite of 2 and 3 that is as least as large as the input.
This can be interpreted as solving an integer linear programming problem with
min (\(a,b) -> a * log 2 + b * log 3)
over the domain {(a,b) : a>=0, b>=0, a * log 2 + b * log 3 >= log n}
-}
{-
This implementation looks stupid,
but it is drastically faster for large numbers than ceiling3SmoothNaive.
The reason is that the smooth numbers are logarithmically equally distributed.
That is, from @n@ to the next smooth number
it may be only 1% deviation from @n@,
but for huge @n@ the absolute difference @0.01*n@ is still huge.

@ceiling3Smooth (10^400+1)@ can be computed in about 0.1 seconds.
(Surprisingly, @ceiling3Smooth (10^500+1)@ needs almost 30 seconds.)
-}
ceiling3SmoothScan :: Integer -> Integer
ceiling3SmoothScan n =
   head $ dropWhile (<n) numbers3Smooth

ceiling5SmoothScan :: Integer -> Integer
ceiling5SmoothScan n =
   head $ dropWhile (<n) numbers5Smooth

ceiling3SmoothNaive :: Integer -> Integer
ceiling3SmoothNaive =
   head . dropWhile (not . is3Smooth) . iterate (1+)

ceiling5SmoothNaive :: Integer -> Integer
ceiling5SmoothNaive =
   head . dropWhile (not . is5Smooth) . iterate (1+)


{-
Problem: We cannot just start with the ceilingPowerOfTwo
and then multiply with 3/4 until we fall below n,
since the 3/4 decreases too fast.
27/32 is closer to one,
and higher powers of 3 and 2 in the ratio make the ratio even closer to one.

This implementation is different:
It always moves and tests above @n@.
For every power of 3 it computes the least power of 2,
such that their product is above @n@.
-}
ceiling3SmoothTrace :: Integer -> Integer
ceiling3SmoothTrace n =
   minimum $ ceilingSmoothsTrace 2 3 n $ ceilingPowerOfTwo n

{-
We must be careful not to skip combinations that are optimal.

E.g.:
> _ceiling5SmoothTraceWrong (10^70+1)
10002658207445093206727527411583349735126415100956607165326185795158016
> ceiling5Smooth (10^70+1)
10001329015408448808646079907338649600000000000000000000000000000000000
-}
_ceiling5SmoothTraceWrong :: Integer -> Integer
_ceiling5SmoothTraceWrong n =
   minimum $ map (minimum . ceilingSmoothsTrace 3 5 n) $
   ceilingSmoothsTrace 2 3 n $ ceilingPowerOfTwo n

{-
For every reasonable pair of powers of 3 and 5
it computes the least power of 2,
such that their product is above @n@.
-}
ceiling5SmoothTrace :: Integer -> Integer
ceiling5SmoothTrace n =
   minimum $ map (minimum . ceilingSmoothsTrace 2 5 n) $
   ceilingSmoothsTrace 2 3 n $ ceilingPowerOfTwo n

{- |
@ceilingSmoothsTrace a b n m@
replaces successively @a@ factors in @m@ by @b@ factors
while keeping the product above @n@.
-}
ceilingSmoothsTrace :: Integer -> Integer -> Integer -> Integer -> [Integer]
ceilingSmoothsTrace a b n =
   let divMany k =
          case divMod k a of
             (q,r) -> if r==0 && q>=n then divMany q else k
       go m  =  m : if mod m a == 0 then go $ divMany $ m*b else []
   in  go


{- |
Compute all primes that occur in the course of dividing
a Fourier transform into sub-transforms.
-}
partialPrimes :: Integer -> [Integer]
partialPrimes =
   let primeFactorSet = Set.fromAscList . uniquePrimeFactors
   in  unfoldr
         (fmap
             (\(p,set) ->
                (p, Set.union (primeFactorSet (p-1)) set)) .
          Set.maxView)
       .
       primeFactorSet

-- cf. htam:NumberTheory
uniquePrimeFactors ::
   (Integral.C a, Bits a, ZeroTestable.C a, Ord a) =>
   a -> [a]
--   map snd . primeFactors
uniquePrimeFactors n =
   let oddFactors =
          foldr
             (\p go m ->
                let (q,r) = divMod m p
                in  if r==0
                      then p : go (divideByMaximumPower p q)
                      else
                        if q >= p
                          then go m
                          else if m==1 then [] else m : [])
             (error "uniquePrimeFactors: end of infinite list")
             (iterate (2+) 3)
   in  case powerOfTwoFactors n of
          (1,m) -> oddFactors m
          (_,m) -> 2 : oddFactors m

{- |
Prime factors and their exponents in ascending order.
-}
primeFactors ::
   (PrimitiveRoot a, Ord a) => a -> [(Int, a)]
primeFactors n =
   let oddFactors =
          foldr
             (\p go m ->
                let (q0,r) = divMod m p
                in  if r==0
                      then
                        case getMaximumExponent p q0 of
                          (e,q1) -> (e+1,p) : go q1
                      else
                        if q0 >= p
                          then go m
                          else if m==1 then [] else (1,m) : [])
             (const [])
             (filter (not . Units.isUnit) $
              primitiveRootCandidates n)
   in  case getMaximumExponent 2 n of
          (0,m) -> oddFactors m
          (e,m) -> (e,2) : oddFactors m

{-
cf. htam:NumberTheory

Shall this be moved to NumericPrelude?

It should be replaced by a more sophisticated prime test.
-}
isPrime :: Integer -> Bool
isPrime n =
   case primeFactors n of
      [] -> False
      (e,m):_ -> e==1 && m==n

{- |
Find lengths of signals that require many interim Rader transforms
and end with the given length.

> raderWorstCases 2  =  <http://oeis.org/A061092>
> raderWorstCases 5  =  tail <http://oeis.org/A059411>

Smallest raderWorstCase numbers are 2,5,13,17,19,31,37,41,43,61,...
This matches the definition of <http://oeis.org/A061303>.
-}
raderWorstCases :: Integer -> [Integer]
raderWorstCases =
   iterate
      (\n ->
         head $ dropWhile (not . isPrime) $
         tail $ iterate (n+) 1)

{- |
This is usually faster than 'fastFourierRing'
since it does not need to factor large numbers.
However, the generated modulus is usually much greater.
-}
{-
I see the following opportunities for optimization:

1. Speedup 'fastFourierRing' by
   faster primality test (Miller-Rabin) and
   faster prime factorization (Pollard-Rho-method).
   These are also needed for
   maximumOrderOfPrimitiveRootsOfUnityInteger
   that is used by Fourier.Element.primitiveRoot
   in order to compute a root with maximum order.

2. Reduce the moduli produced by '_fastFourierRingAlt'
   by merging some orders that are passed to
   ringWithPrimitiveRootsOfUnityAndUnits,
   such that an LCM of a group of orders can still be handled.
   This is a kind of knapsack problem.
   Maybe we could collect the factors in a way
   such that (lcm orderGroup + 1) is prime.

3. Avoid to compute factorizations of numbers
   where we already know the factors
   or where we do not need the factors at all.
   Use the factors returned by partialPrimes
   in order to compute a prime factorization
   of lcmMulti (map pred (partialPrimes n)).
   Call this (product ps).
   Now search for rings of moduli (1 + k * product ps),
   where there are (small) primitive roots of order (product ps).
   We only need to check whether there are small numbers
   such as 2, 3, 5, 6, 7 that have a (product ps)-th power that is 1,
   using fast exponentiation.
   If there is a power being 1 then check for primitivity
   by computing (k * product ps / p)-th powers
   for all prime factors p of (k * product ps).
   If there is no primitive root <= 7,
   there may still be a primitive root of wanted order,
   but it is then cheaper to seek for larger moduli.

   If we finally have a nice modulus
   it is still stupid to factorize (modulus-1)
   and search for a primitive root
   in each invocation of Fourier.Element.primitiveRoot.
   We could define a special datatype analogously to ResidueClass,
   that stores the primitive root and its order
   additional to the ResidueClass modulus.
-}
_fastFourierRingAlt :: Int -> Integer
_fastFourierRingAlt n =
   case n of
      0 -> 2
      1 -> 2
      _ ->
         let ni = fromIntegral n
             ps = filter (>1) (map (subtract 1) (partialPrimes ni))
         in  ringWithPrimitiveRootsOfUnityAndUnits (map Order $ ni : ps) ps

{- |
Determine an integer residue ring
in which a Fast Fourier transform of size n can be performed.
It must contain certain primitive roots.
If we choose a non-primitive root,
then some off-diagonal values in F^-1·F are non-zero.
-}
{-
When we need roots of orders o1,...,ok and according inverse elements
we can also ask for a ring, where there is a root of order lcm(o1,...,ok).
The answer to both questions is the same set of rings.
This can be proven using the statement,
that the order of any primitive root
divides the carmichael value of the modulus.

Since ringWithPrimitiveRootsOfUnityAndUnits
multiplies the moduli of rings for o1,...,ok,
it will produce large moduli.
-}
fastFourierRing :: Int -> Integer
fastFourierRing n =
   case n of
      0 -> 2
      1 -> 2
      _ ->
         let ni = fromIntegral n
         in  {-
             We cannot use ringsWithPrimitiveRootOfUnityAndUnit
             since for 359 we already get an Int overflow.
             For 719, 1439, 2879 we also get a very large value.
             -}
             head $ filter isPrime $
             (\order -> iterate (order +) 1) $
             lcmMulti $
             ni : map (subtract 1) (partialPrimes ni)
