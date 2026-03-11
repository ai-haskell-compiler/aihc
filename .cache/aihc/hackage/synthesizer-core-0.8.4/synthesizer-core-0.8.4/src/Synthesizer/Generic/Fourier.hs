{- |
Complete implementation for Fast Fourier Transform for any signal length.
Although defined for all kinds of signal storage,
we need fast access to arbitrary indices.
-}
{-
further thoughts
 - Test the algorithms using remainder polynomials
     with respect to [-1,0,...,0,1]
     Problem: We would need large polynomial degrees,
     namely LCM of the size of all sub-transforms.
     Those numbers are in the same magnitude
     as the integers we use for our integer residue class arithmetic.
 - a z-transform by convolving with a chirp would be nice,
     however we need a square of the primitive root of unity
     in order to compute cis((i/n)^2/2)
 - Can we write the Fourier transforms for lengths larger than the input signal length
     with implicit zero padding?
     This would be useful for Fourier based convolution.
     Our frequent use of 'rechunk' would be a problem, though.
     transformCoprime also needs explicit zero padding.
 - a type class could unify all Level generators
     and thus they would allow for a generic way to call a certain sub-transform
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Generic.Fourier (
   Element(..),
   -- * conversion between time and frequency domain (spectrum)
   transformForward,
   transformBackward,
   cacheForward,
   cacheBackward,
   cacheDuplex,
   transformWithCache,
   -- * convolution based on Fourier transform
   convolveCyclic,
   Window,
   window,
   convolveWithWindow,
   ) where

import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Generic.Cyclic as Cyclic
import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNRG

import qualified Synthesizer.Generic.Permutation as Permutation
import qualified Synthesizer.Basic.NumberTheory as NumberTheory

import qualified Synthesizer.State.Analysis as Ana
import qualified Synthesizer.State.Signal as SigS

import qualified Control.Monad.Trans.State as State
import Control.Monad (liftM2, )
import Control.Applicative ((<$>), )

import qualified Data.Map as Map
import Data.Tuple.HT (mapPair, )

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Ring as Ring
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.IntegralDomain as Integral

import qualified Number.ResidueClass.Check as RC
import Number.ResidueClass.Check ((/:), )

import qualified Number.Complex as Complex
import Number.Complex ((+:))

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (head, )



class Ring.C y => Element y where
   recipInteger :: (SigG.Read sig y) => sig y -> y
   addId :: (SigG.Read sig y) => sig y -> y
   multId :: (SigG.Read sig y) => sig y -> y
   {- |
   It must hold:

   > uncurry (*) (conjugatePrimitiveRootsOfUnity n) = 1

   > mapPair ((^m), (^m)) (conjugatePrimitiveRootsOfUnity (n*m) y)
   >    == conjugatePrimitiveRootsOfUnity n y@

   since we need for caching that the cache is uniquely determined
   by singal length and transform direction.
   -}
   conjugatePrimitiveRootsOfUnity :: (SigG.Read sig y) => sig y -> (y,y)

instance Trans.C a => Element (Complex.T a) where
   recipInteger sig = recip (fromIntegral (SigG.length sig)) +: zero
   addId _sig = zero
   multId _sig = one
   conjugatePrimitiveRootsOfUnity sig =
      (\x -> (x, Complex.conjugate x)) $
      case SigG.length sig of
         1 -> one
         2 -> negate one
         3 -> (negate one +: sqrt 3) / 2
         4 -> zero +: one
         5 ->
            let sqrt5 = sqrt 5
            in  ((sqrt5 - 1) +: sqrt 2 * sqrt(5 + sqrt5)) / 4
         6 -> (one +: sqrt 3) / 2
         8 -> Complex.scale (sqrt 2 / 2) (one +: one)
         12 -> (sqrt 3 +: one) / 2
         n -> Complex.cis (2*pi / fromIntegral n)

instance (NumberTheory.PrimitiveRoot a, PID.C a, Eq a) => Element (RC.T a) where
   recipInteger sig =
      recip (fromIntegral (SigG.length sig) /: RC.modulus (head sig))
   addId sig = zero /: RC.modulus (head sig)
   multId sig = one /: RC.modulus (head sig)
   {-
   We cannot simply compute
     NumberTheory.primitiveRootsOfUnity modu (SigG.length sig)
   since we have to fulfill the laws.
   In order to fulfill them,
   we choose a root with maximum order,
   this will always be the same,
   and it is a root of all primitive roots
   of any possible order in that ring.
   -}
   conjugatePrimitiveRootsOfUnity sig =
      let modu = RC.modulus (head sig)
          order@(NumberTheory.Order expo) =
             NumberTheory.maximumOrderOfPrimitiveRootsOfUnity modu
          r:_ = NumberTheory.primitiveRootsOfUnity modu order
          n = Integral.divChecked expo (fromIntegral (SigG.length sig))
          z = (r /: modu) ^ n
      in  (z, recip z)


head :: (SigG.Read sig y) => sig y -> y
head =
   SigG.switchL (error "Generic.Signal.head: empty signal") const .
   SigG.toState


directionPrimitiveRootsOfUnity ::
   (Element y, SigG.Read sig y) =>
   sig y -> ((Direction,y), (Direction,y))
directionPrimitiveRootsOfUnity x =
   let (z,zInv) = conjugatePrimitiveRootsOfUnity x
   in  ((Forward,z), (Backward,zInv))

transformForward ::
   (Element y, SigG.Transform sig y) =>
   sig y -> sig y
transformForward xs =
   transformWithCache (cacheForward xs) xs

{- |
Shall we divide the result values by the length of the signal?
Our dimensional wrapper around the Fourier transform does not expect this.
-}
transformBackward ::
   (Element y, SigG.Transform sig y) =>
   sig y -> sig y
transformBackward xs =
   transformWithCache (cacheBackward xs) xs

{- |
The size of the signal must match the size, that the plan was generated for.
-}
_transformPlan ::
   (Element y, SigG.Transform sig y) =>
   Plan -> (Direction,y) -> sig y -> sig y
_transformPlan p z xs =
   transformWithCache (cacheFromPlan p z xs) xs

{- |
The size and type of the signal must match the parameters,
that the cache was generated for.
-}
transformWithCache ::
   (Element y, SigG.Transform sig y) =>
   Cache sig y -> sig y -> sig y
transformWithCache cache xs =
   case cache of
      CacheIdentity -> xs
      CacheSmall size ->
         case size of
            LevelCache2 zs -> transform2 zs xs
            LevelCache3 zs -> transform3 zs xs
            LevelCache4 zs -> transform4 zs xs
            LevelCache5 zs -> transform5 zs xs
      CacheNaive level ->
         transformNaive level xs
      CacheRadix2 level subCache ->
         transformRadix2InterleavedFrequency level subCache xs
      CachePrime level subCaches ->
         transformPrime level subCaches xs
      CacheCoprime level subCaches ->
         transformCoprime level subCaches xs
      CacheComposite level subCaches ->
         transformComposite level subCaches xs


{- |
Memorize factorizations of the data size and permutation vectors.
-}
data Plan =
     PlanIdentity
   | PlanSmall LevelSmall
   | PlanNaive  -- mainly for debugging
   | PlanRadix2 LevelRadix2 Plan
   | PlanPrime LevelPrime Plan
   | PlanCoprime LevelCoprime (Plan, Plan)
   | PlanComposite LevelComposite (Plan, Plan)
   deriving (Show)

{-
efficient swallow comparison
only correct for Plans generated by 'plan'.
-}
instance Eq Plan where
   p0 == p1  =  compare p0 p1 == EQ

{-
Needed for keys in CacheMap
-}
instance Ord Plan where
   compare p0 p1  =
      case (p0,p1) of
         (PlanIdentity, PlanIdentity) -> EQ
         (PlanIdentity, _) -> LT
         (_, PlanIdentity) -> GT
         (PlanSmall l0, PlanSmall l1) -> compare l0 l1
         (PlanSmall _, _) -> LT
         (_, PlanSmall _) -> GT
         (PlanNaive, PlanNaive) -> EQ
         (PlanNaive, _) -> LT
         (_, PlanNaive) -> GT
         (PlanRadix2 l0 _, PlanRadix2 l1 _) -> compare l0 l1
         (PlanRadix2 _ _, _) -> LT
         (_, PlanRadix2 _ _) -> GT
         (PlanPrime l0 _, PlanPrime l1 _) -> compare l0 l1
         (PlanPrime _ _, _) -> LT
         (_, PlanPrime _ _) -> GT
         (PlanCoprime l0 _, PlanCoprime l1 _) -> compare l0 l1
         (PlanCoprime _ _, _) -> LT
         (_, PlanCoprime _ _) -> GT
         (PlanComposite l0 _, PlanComposite l1 _) -> compare l0 l1


plan :: Integer -> Plan
plan n =
   State.evalState (planWithMapUpdate n) smallPlanMap

type PlanMap = Map.Map Integer Plan

smallPlanMap :: PlanMap
smallPlanMap =
   Map.fromAscList $ zip [0..] $
   PlanIdentity :
   PlanIdentity :
   PlanSmall Level2 :
   PlanSmall Level3 :
   PlanSmall Level4 :
   PlanSmall Level5 :
   []

{- |
Detect and re-use common sub-plans.
-}
planWithMap :: Integer -> State.State PlanMap Plan
planWithMap n =
   case divMod n 2 of
      (n2,0) -> PlanRadix2 (levelRadix2 n2) <$> planWithMapUpdate n2
      _ ->
         let facs = NumberTheory.fermatFactors n
         in  -- find unitary divisors
             case filter (\(a,b) -> a>1 && gcd a b == 1) facs of
                q2 : _ ->
                   PlanCoprime (levelCoprime q2) <$>
                   planWithMapUpdate2 q2
                _ ->
                   let (q2 : _) = facs
                   in  if fst q2 == 1
                         then PlanPrime (levelPrime $ snd q2) <$>
                              planWithMapUpdate (n-1)
                         else PlanComposite (levelComposite q2) <$>
                              planWithMapUpdate2 q2

planWithMapUpdate :: Integer -> State.State PlanMap Plan
planWithMapUpdate n = do
   item <- State.gets (Map.lookup n)
   case item of
      Just p -> return p
      Nothing ->
         planWithMap n >>= \m -> State.modify (Map.insert n m) >> return m

planWithMapUpdate2 :: (Integer, Integer) -> State.State PlanMap (Plan, Plan)
planWithMapUpdate2 =
   uncurry (liftM2 (,)) .
   mapPair (planWithMapUpdate,planWithMapUpdate)


{- |
Cache powers of the primitive root of unity
in a storage compatible to the processed signal.
-}
data Cache sig y =
     CacheIdentity
   | CacheSmall (LevelCacheSmall y)
   | CacheNaive (LevelCacheNaive y)
   | CacheRadix2 (LevelCacheRadix2 sig y) (Cache sig y)
   | CachePrime (LevelCachePrime sig y) (Cache sig y, Cache sig y)
   | CacheCoprime LevelCoprime (Cache sig y, Cache sig y)
   | CacheComposite (LevelCacheComposite sig y) (Cache sig y, Cache sig y)
   deriving (Show)

{- |
The expression @cacheForward prototype@
precomputes all data that is needed for forward Fourier transforms
for signals of the type and length @prototype@.
You can use this cache in 'transformWithCache'.
-}
cacheForward ::
   (Element y, SigG.Transform sig y) =>
   sig y -> Cache sig y
cacheForward xs =
   cacheFromPlan
      (plan $ fromIntegral $ SigG.length xs)
      (fst $ directionPrimitiveRootsOfUnity xs)
      xs

{- |
See 'cacheForward'.
-}
cacheBackward ::
   (Element y, SigG.Transform sig y) =>
   sig y -> Cache sig y
cacheBackward xs =
   cacheFromPlan
      (plan $ fromIntegral $ SigG.length xs)
      (snd $ directionPrimitiveRootsOfUnity xs)
      xs

{- |
It is @(cacheForward x, cacheBackward x) = cacheDuplex x@
but 'cacheDuplex' shared common data of both caches.
-}
cacheDuplex ::
   (Element y, SigG.Transform sig y) =>
   sig y -> (Cache sig y, Cache sig y)
cacheDuplex xs =
   let p = plan $ fromIntegral $ SigG.length xs
       (z,zInv) = directionPrimitiveRootsOfUnity xs
   in  State.evalState
          (cacheFromPlanWithMapUpdate2 (p,p) (z,zInv) (xs,xs)) $
       Map.empty


data Direction = Forward | Backward
   deriving (Show, Eq, Ord)

type CacheMap sig y = Map.Map (Plan,Direction) (Cache sig y)

cacheFromPlan ::
   (Element y, SigG.Transform sig y) =>
   Plan -> (Direction, y) -> sig y -> Cache sig y
cacheFromPlan p z xs =
   State.evalState (cacheFromPlanWithMapUpdate p z xs) $
   Map.empty

{- |
Detect and re-use common sub-caches.
-}
cacheFromPlanWithMap ::
   (Element y, SigG.Transform sig y) =>
   Plan -> (Direction,y) -> sig y ->
   State.State (CacheMap sig y) (Cache sig y)
cacheFromPlanWithMap p (d,z) xs =
   case p of
      PlanIdentity -> return $ CacheIdentity
      PlanSmall size -> return $ CacheSmall $
         case size of
            Level2 -> LevelCache2 $ cache2 z
            Level3 -> LevelCache3 $ cache3 z
            Level4 -> LevelCache4 $ cache4 z
            Level5 -> LevelCache5 $ cache5 z
      PlanNaive ->
         return $ CacheNaive $ LevelCacheNaive z
      PlanRadix2 level@(LevelRadix2 n2) subPlan ->
         let subxs = CutG.take n2 xs
         in  CacheRadix2 (levelCacheRadix2 level z subxs) <$>
             cacheFromPlanWithMapUpdate subPlan (d,z*z) subxs
      PlanPrime level@(LevelPrime (perm,_,_)) subPlan ->
         (\subCaches ->
            CachePrime
               (levelCachePrime level (fst subCaches) z xs) subCaches)
         <$>
         let subxs = CutG.take (Permutation.size perm) xs
         in  cacheFromPlanWithMapUpdate2 (subPlan,subPlan)
                (directionPrimitiveRootsOfUnity subxs)
                (subxs,subxs)
      PlanCoprime level@(LevelCoprime (n,m) _) subPlans ->
         CacheCoprime level <$>
         cacheFromPlanWithMapUpdate2 subPlans ((d,z^m), (d,z^n))
            (CutG.take (fromInteger n) xs, CutG.take (fromInteger m) xs)
      PlanComposite level@(LevelComposite (n,m) _) subPlans ->
         CacheComposite (levelCacheComposite level z xs) <$>
         cacheFromPlanWithMapUpdate2 subPlans ((d,z^m), (d,z^n))
            (CutG.take (fromInteger n) xs, CutG.take (fromInteger m) xs)

cacheFromPlanWithMapUpdate ::
   (Element y, SigG.Transform sig y) =>
   Plan -> (Direction,y) -> sig y ->
   State.State (CacheMap sig y) (Cache sig y)
cacheFromPlanWithMapUpdate p z xs = do
   let key = (p, fst z)
   item <- State.gets (Map.lookup key)
   case item of
      Just c -> return c
      Nothing ->
         cacheFromPlanWithMap p z xs >>= \m ->
         State.modify (Map.insert key m) >>
         return m

cacheFromPlanWithMapUpdate2 ::
   (Element y, SigG.Transform sig y) =>
   (Plan, Plan) -> ((Direction,y),(Direction,y)) -> (sig y, sig y) ->
   State.State (CacheMap sig y) (Cache sig y, Cache sig y)
cacheFromPlanWithMapUpdate2 (p0,p1) (z0,z1) (xs0,xs1) =
   liftM2 (,)
      (cacheFromPlanWithMapUpdate p0 z0 xs0)
      (cacheFromPlanWithMapUpdate p1 z1 xs1)


newtype LevelCacheNaive y =
      LevelCacheNaive y
   deriving (Show)

transformNaive ::
   (Element y, SigG.Transform sig y) =>
   LevelCacheNaive y -> sig y -> sig y
transformNaive (LevelCacheNaive z) sig =
   SigG.takeStateMatch sig $
   SigS.map
      (scalarProduct1 (SigG.toState sig) . powers sig)
      (powers sig z)

scalarProduct1 ::
   (Ring.C a) =>
   SigS.T a -> SigS.T a -> a
scalarProduct1 xs ys =
   SigS.foldL1 (+) $ SigS.zipWith (*) xs ys

_transformRing ::
   (Ring.C y, SigG.Transform sig y) =>
   y -> sig y -> sig y
_transformRing z sig =
   SigG.takeStateMatch sig $
   Ana.chirpTransform z $ SigG.toState sig

powers ::
   (Element y, SigG.Read sig y) =>
   sig y -> y -> SigS.T y
powers sig c = SigS.iterate (c*) $ multId sig


data LevelSmall = Level2 | Level3 | Level4 | Level5
   deriving (Show, Eq, Ord, Enum)

data LevelCacheSmall y =
     LevelCache2 y
   | LevelCache3 (y,y)
   | LevelCache4 (y,y,y)
   | LevelCache5 (y,y,y,y)
   deriving (Show)

cache2 :: (Ring.C y) => y -> y
cache3 :: (Ring.C y) => y -> (y,y)
cache4 :: (Ring.C y) => y -> (y,y,y)
cache5 :: (Ring.C y) => y -> (y,y,y,y)

cache2 z = z
cache3 z = (z, z*z)
cache4 z = let z2=z*z in (z,z2,z*z2)
cache5 z = let z2=z*z in (z,z2,z*z2,z2*z2)


transform2 ::
   (Ring.C y, SigG.Transform sig y) =>
   y -> sig y -> sig y
transform2 z sig =
   let x0:x1:_ = SigG.toList sig
   in  SigG.takeStateMatch sig $
       SigS.fromList [x0+x1, x0+z*x1]

transform3 ::
   (Ring.C y, SigG.Transform sig y) =>
   (y,y) -> sig y -> sig y
transform3 (z,z2) sig =
   let x0:x1:x2:_ = SigG.toList sig
{- Rader's algorithm with convolution by 2-size-Fourier-transform
       xf1 = x1+x2
       xf2 = x1-x2
       zf1 = z+z2
       zf2 = z-z2
       xzf1 = xf1*zf1
       xzf2 = xf2*zf2
       xz1 = (xzf1+xzf2)/2
       xz2 = (xzf1-xzf2)/2
-}
{- naive
       [x0+x1+x2, x0+z*x1+z2*x2, x0+z2*x1+z*x2]
-}
       ((s,_), (zx1,zx2)) = Cyclic.sumAndConvolvePair (x1,x2) (z,z2)
   in  SigG.takeStateMatch sig $
       SigS.fromList [x0+s, x0+zx1, x0+zx2]

transform4 ::
   (Ring.C y, SigG.Transform sig y) =>
   (y,y,y) -> sig y -> sig y
transform4 (z,z2,z3) sig =
   let x0:x1:x2:x3:_ = SigG.toList sig
       x02a = x0+x2; x02b = x0+z2*x2
       x13a = x1+x3; x13b = x1+z2*x3
   in  SigG.takeStateMatch sig $
       SigS.fromList [x02a+   x13a, x02b+z *x13b,
                      x02a+z2*x13a, x02b+z3*x13b]
{-
This needs also five multiplications,
but in complex numbers it is z=i, and thus multiplications are cheap
and we should better make use of distributive law in order to save additions.

       x02a = x0+x2; x02b = x0+z2*x2
       x1_2 = z2*x1; x3_2 = z2*x3
   in  SigG.takeStateMatch sig $
       SigS.fromList [x02a + x1   + x3  , x02b+z*(x1   + x3_2),
                      x02a + x1_2 + x3_2, x02b+z*(x1_2 + x3  )]
-}

{-
Use Rader's trick for mapping the transform to a convolution
and apply Karatsuba's trick at two levels (i.e. total three times)
to that convolution.

0 0 0 0 0
0 1 2 3 4
0 2 4 1 3
0 3 1 4 2
0 4 3 2 1

Permutation.T: 0 1 2 4 3

0 0 0 0 0
0 1 2 4 3
0 2 4 3 1
0 4 3 1 2
0 3 1 2 4
-}
transform5 ::
   (Ring.C y, SigG.Transform sig y) =>
   (y,y,y,y) -> sig y -> sig y
transform5 (z1,z2,z3,z4) sig =
   let x0:x1:x2:x3:x4:_ = SigG.toList sig
       ((s,_), (d1,d2,d4,d3)) =
          Cyclic.sumAndConvolveQuadruple (x1,x3,x4,x2) (z1,z2,z4,z3)
   in  SigG.takeStateMatch sig $
       SigS.fromList [x0+s, x0+d1, x0+d2, x0+d3, x0+d4]

{-
transform7

Toom-3-multiplication at the highest level and Karatsuba below?
Toom-2.5-multiplication with manual addition of the missing parts?

Toom-3-multiplication with complex interpolation nodes?
Still requires division by 4 and then complex multiplication in the frequency domain.
A:=matrix(5,5,[1,0,0,0,0,1,1,1,1,1,1,-1,1,-1,1,1,I,-1,-I,1,0,0,0,0,1]);
A:=matrix(5,5,[1,0,0,0,0,1,1,1,1,1,1,-1,1,-1,1,1,I,-1,-I,1,1,-I,-1,I,1]);

Karatsuba at three levels for convolution of signal of size 8 with zero padding?

Modify the 3x3 Fourier matrix by multiplying a regular matrix
to make it more convenient to work with?
We will hardly get rid of the irrational numbers.
-}

newtype LevelRadix2 = LevelRadix2 Int
   deriving (Show, Eq, Ord)

levelRadix2 :: Integer -> LevelRadix2
levelRadix2 =
   LevelRadix2 . fromIntegral


data LevelCacheRadix2 sig y =
   LevelCacheRadix2 Int (sig y)
   deriving (Show)

levelCacheRadix2 ::
   (Element y, SigG.Transform sig y) =>
   LevelRadix2 -> y -> sig y -> LevelCacheRadix2 sig y
levelCacheRadix2 (LevelRadix2 n2) z sig =
   LevelCacheRadix2 n2
      (SigG.takeStateMatch sig $ powers sig z)


{- |
Cooley-Tukey specialised to one factor of the size being 2.

Size of the input signal must be even.
-}
transformRadix2InterleavedFrequency ::
   (Element y, SigG.Transform sig y) =>
   LevelCacheRadix2 sig y -> Cache sig y -> sig y -> sig y
transformRadix2InterleavedFrequency
      (LevelCacheRadix2 n2 twiddle) subCache sig =
   let (xs0,xs1) = SigG.splitAt n2 sig
       fs0 = transformWithCache subCache $ SigG.zipWith (+) xs0 xs1
       fs1 = transformWithCache subCache $
                SigG.zipWith3
                   (\w x0 x1 -> w*(x0-x1))
                   twiddle xs0 xs1
   in  SigG.takeStateMatch sig $
       SigS.interleave (SigG.toState fs0) (SigG.toState fs1)


data LevelComposite =
   LevelComposite
      (Integer, Integer)
      (Permutation.T, Permutation.T)
   deriving (Show)

instance Eq LevelComposite where
   a == b  =  compare a b == EQ

instance Ord LevelComposite where
   compare (LevelComposite a _) (LevelComposite b _)  =
      compare a b

levelComposite :: (Integer, Integer) -> LevelComposite
levelComposite (n,m) =
   let ni = fromInteger n
       mi = fromInteger m
   in  LevelComposite (n,m)
          (Permutation.transposition ni mi,
           Permutation.transposition mi ni)


data LevelCacheComposite sig y =
   LevelCacheComposite
      (Integer, Integer)
      (Permutation.T, Permutation.T)
      (sig y)
   deriving (Show)

levelCacheComposite ::
   (Element y, SigG.Transform sig y) =>
   LevelComposite -> y -> sig y -> LevelCacheComposite sig y
levelCacheComposite (LevelComposite (n,m) transpose) z sig =
   LevelCacheComposite (n,m) transpose $
   SigG.takeStateMatch sig $
   flip SigS.generateInfinite (n, multId sig, multId sig) $ \(i,zi,zij) ->
   (zij,
    case pred i of
      0 -> (n, zi*z, multId sig)
      i1 -> (i1, zi, zij*zi))
{-
   {-# SCC "levelCacheComposite:rechunk" #-}
   concatRechunk sig $
   {-# SCC "levelCacheComposite:subpowers" #-}
   SigS.map
      (SigG.takeStateMatch (SigG.take (fromIntegral n) sig) . powers sig)
      ({-# SCC "levelCacheComposite:powers" #-}
       powers sig z)
-}
{-
   SigS.map
      (SigG.takeStateMatch sig . SigS.take (fromIntegral n) . powers sig)
      ({-# SCC "levelCacheComposite:powers" #-}
       powers sig z)
-}
{- suffers from big inefficiency of repeated 'append'
   SigG.takeStateMatch sig $
   SigS.fold $
   SigS.map (SigS.take (fromIntegral n) . powers sig) $
   SigS.take (fromIntegral m) $ -- necessary for strict storable vectors
   powers sig z
-}

{- |
For @transformComposite z (n,m) sig@,
it must hold @n*m == length sig@ and @z ^ length sig == 1@.

Cooley-Tukey-algorithm
-}
transformComposite ::
   (Element y, SigG.Transform sig y) =>
   LevelCacheComposite sig y -> (Cache sig y, Cache sig y) -> sig y -> sig y
transformComposite
      (LevelCacheComposite (n,m) (transposeNM, transposeMN) twiddle)
      (subCacheN,subCacheM) sig =
   Permutation.apply transposeMN .
       concatRechunk sig .
       SigS.map (transformWithCache subCacheM) .
       SigG.sliceVertical (fromInteger m) .
       Permutation.apply transposeNM .
--       concatRechunk sig .
       SigG.zipWith (*) twiddle .
       SigS.fold .
       SigS.map (transformWithCache subCacheN) .
       SigG.sliceVertical (fromInteger n) .
       Permutation.apply transposeMN $
       sig


data LevelCoprime =
   LevelCoprime
      (Integer, Integer)
      (Permutation.T, Permutation.T, Permutation.T)
   deriving (Show)

instance Eq LevelCoprime where
   a == b  =  compare a b == EQ

instance Ord LevelCoprime where
   compare (LevelCoprime a _) (LevelCoprime b _)  =
      compare a b

{-
Fourier exponent matrix of a signal of size 6.

0 0 0 0 0 0     0               0   0   0       0     0
0 1 2 3 4 5               0       2   0   4       3     0
0 2 4 0 2 4  =          0    *  0   2   4    *      0     0
0 3 0 3 0 3           0           0   0   0     0     3
0 4 2 0 4 2         0           0   4   2         0     0
0 5 4 3 2 1       0               4   0   2         0     3
-}
levelCoprime :: (Integer, Integer) -> LevelCoprime
levelCoprime (n,m) =
   let ni = fromInteger n
       mi = fromInteger m
   in  LevelCoprime (n,m)
          (Permutation.skewGrid mi ni,
           Permutation.transposition ni mi,
           Permutation.skewGridCRTInv ni mi)


{- |
For @transformCoprime z (n,m) sig@,
the parameters @n@ and @m@ must be relatively prime
and @n*m == length sig@ and @z ^ length sig == 1@.

Good-Thomas algorithm
-}
{-
A very elegant way would be to divide the signal into chunks of size n,
define ring operations on these chunks
and perform one (length/n)-size-sub-transform in this chunk-ring.
This way we would also only have to plan the sub-transform once.
On StorableVectors the chunking could be performed in-place
in terms of a virtual reshape operation.
In the general case the performance can become very bad
if the chunks are very small, say 2 or 3 elements.
-}
transformCoprime ::
   (Element y, SigG.Transform sig y) =>
   LevelCoprime -> (Cache sig y, Cache sig y) -> sig y -> sig y
transformCoprime
      (LevelCoprime (n,m) (grid, transpose, gridInv)) (subCacheN,subCacheM) =
   let subTransform cache j sig =
          concatRechunk sig .
          SigS.map (transformWithCache cache) .
          SigG.sliceVertical (fromIntegral j) $ sig
   in  Permutation.apply gridInv .
       subTransform subCacheM m .
       Permutation.apply transpose .
       subTransform subCacheN n .
       Permutation.apply grid


-- concatenate and reorganize for faster indexing
concatRechunk ::
   (SigG.Transform sig y) =>
   sig y -> SigS.T (sig y) -> sig y
concatRechunk pattern =
   SigG.takeStateMatch pattern .
   SigG.toState .
   SigS.fold


data LevelPrime =
   LevelPrime (Permutation.T, Permutation.T, Permutation.T)
      deriving (Show)

instance Eq LevelPrime where
   a == b  =  compare a b == EQ

instance Ord LevelPrime where
   compare (LevelPrime (a,_,_)) (LevelPrime (b,_,_))  =
      compare (Permutation.size a) (Permutation.size b)

{-
Fourier exponent matrix of a signal of size 7.

0 0 0 0 0 0 0
0 1 2 3 4 5 6
0 2 4 6 1 3 5
0 3 6 2 5 1 4
0 4 1 5 2 6 3
0 5 3 1 6 4 2
0 6 5 4 3 2 1

multiplicative generator in Z7: 3
permutation of rows and columns by powers of 3: 1 3 2 6 4 5

0 0 0 0 0 0 0
0 1 3 2 6 4 5
0 3 2 6 4 5 1
0 2 6 4 5 1 3
0 6 4 5 1 3 2
0 4 5 1 3 2 6
0 5 1 3 2 6 4

Inverse permutation: 1 3 2 5 6 4
The inverse permutations seems not to be generated by a multiplication.
-}
levelPrime :: Integer -> LevelPrime
levelPrime n =
   let perm = Permutation.multiplicative $ fromIntegral n
   in  LevelPrime
          (perm, Permutation.reverse perm, Permutation.inverse perm)


data LevelCachePrime sig y =
   LevelCachePrime (Permutation.T, Permutation.T) (sig y)
      deriving (Show)

levelCachePrime ::
   (Element y, SigG.Transform sig y) =>
   LevelPrime -> Cache sig y -> y -> sig y -> LevelCachePrime sig y
levelCachePrime (LevelPrime (perm, rev, inv)) subCache z sig =
   LevelCachePrime (rev, inv)
      ((\zs -> FiltNRG.amplify (recipInteger zs) zs) $
       transformWithCache subCache $
       Permutation.apply perm $
       SigG.takeStateMatch sig $
       SigS.iterate (z*) z)

{- |
Rader's algorithm for prime length signals.
-}
transformPrime ::
   (Element y, SigG.Transform sig y) =>
   LevelCachePrime sig y -> (Cache sig y, Cache sig y) -> sig y -> sig y
transformPrime (LevelCachePrime (rev, inv) zs) subCaches =
   SigG.switchL (error "transformPrime: empty signal") $
   \x0 rest ->
      SigG.cons (SigG.foldL (+) x0 rest) $
      SigG.map (x0+) $
      Permutation.apply inv $
      convolveSpectrumCyclicCache subCaches zs $
      Permutation.apply rev rest

{-
Cyclic.reverse xs = shiftR 1 (reverse xs)
Cyclic.reverse (xs <*> ys) = Cyclic.reverse xs <*> Cyclic.reverse ys
Cyclic.reverse (Cyclic.reverse xs) = xs

We could move the 'Cyclic.reverse' over to the z-vector,
but then we would have to reverse again after convolution.

zs <*> Cyclic.reverse rest
 = Cyclic.reverse (Cyclic.reverse zs <*> rest)
-}

{-
This uses Cyclic.filter instead of Cyclic.convolve.
This is simpler, but Fourier.convolveCyclic is a bit simpler than Fourier.filterCyclic,
since it does not need to reverse an operand.
-}
_transformPrimeAlt ::
   (Ring.C y, SigG.Transform sig y) =>
   LevelPrime -> y -> sig y -> sig y
_transformPrimeAlt (LevelPrime (perm, _, inv)) z =
   SigG.switchL (error "transformPrime: empty signal") $
   \x0 rest ->
      SigG.cons (SigG.foldL (+) x0 rest) $
      SigG.map (x0+) $
      Permutation.apply inv $
      Cyclic.filterNaive
         (Permutation.apply perm rest)
         (Permutation.apply perm (SigG.takeStateMatch rest (SigS.iterate (z*) z)))



{- |
Filter window stored as spectrum
such that it can be applied efficiently to long signals.
-}
data Window sig y =
   Window Int (Cache sig y, Cache sig y) (sig y)
   deriving (Show)


window ::
   (Element y, SigG.Transform sig y) =>
   sig y -> Window sig y
window x =
   if CutG.null x
     then Window 0 (CacheIdentity, CacheIdentity) CutG.empty
     else
       let size  = CutG.length x
           size2 = 2 * NumberTheory.ceilingPowerOfTwo size
           padded =
              SigG.take size2 $
              CutG.append x $
                 let pad = SigG.takeStateMatch x $ SigS.repeat $ addId x
                 in  CutG.append pad (SigG.append pad pad)
           caches@(cache, _cacheInv) =
              cacheDuplex padded
       in  Window
              (size2-size+1)
              caches
              (transformWithCache cache $
               FiltNRG.amplify (recipInteger padded) padded)

{- |
Efficient convolution of a large filter window
with a probably infinite signal.
-}
convolveWithWindow ::
   (Element y, SigG.Transform sig y) =>
   Window sig y -> sig y -> sig y
convolveWithWindow (Window blockSize caches spectrum) b =
   if blockSize==zero
     then CutG.empty
     else
       let windowSize = SigG.length spectrum - blockSize
       in  SigS.foldR (FiltNRG.addShiftedSimple blockSize) CutG.empty $
           SigS.map
              (\block ->
                 SigG.take (windowSize + SigG.length block) $
                 convolveSpectrumCyclicCache caches spectrum $
                 flip CutG.append
                    {-
                    The last block may be shorter than blockSize
                    and thus needs more padding.
                    -}
                    (SigG.takeStateMatch spectrum $ SigS.repeat $ addId b) $
                 block) $
           SigG.sliceVertical blockSize b


{- |
Signal must have equal size and must not be empty.
-}
convolveCyclic ::
   (Element y, SigG.Transform sig y) =>
   sig y -> sig y -> sig y
convolveCyclic x =
   let len = fromIntegral $ SigG.length x
       (z,zInv) =
          directionPrimitiveRootsOfUnity x
   in  convolveCyclicCache
          (cacheFromPlan (plan len) z x,
           cacheFromPlan (plan len) zInv x)
          x

convolveCyclicCache ::
   (Element y, SigG.Transform sig y) =>
   (Cache sig y, Cache sig y) -> sig y -> sig y -> sig y
convolveCyclicCache caches x =
   convolveSpectrumCyclicCache caches $
   FiltNRG.amplify (recipInteger x) $ transformWithCache (fst caches) x

{- |
This function does not apply scaling.
That is you have to scale the spectrum by @recip (length x)@
if you want a plain convolution.
-}
convolveSpectrumCyclicCache ::
   (Element y, SigG.Transform sig y) =>
   (Cache sig y, Cache sig y) -> sig y -> sig y -> sig y
convolveSpectrumCyclicCache (cache,cacheInv) x y =
   transformWithCache cacheInv $
   SigG.zipWith (*) x $
   transformWithCache cache y

{-
Test:

let xs = [0,1,0,0,0,0 :: Complex.T Double]; z = fst $ conjugatePrimitiveRootsOfUnity xs in print (transformNaive z xs) >> print (transformCoprime z (2,3) xs)
-}
