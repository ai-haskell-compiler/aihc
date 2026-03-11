{-# OPTIONS_GHC -funbox-strict-fields -O #-}
{-# LANGUAGE ExistentialQuantification #-}
{-  -ddump-simpl -ddump-asm -}
{-  -dverbose-core2core -ddump-simpl-stats -}
-- I use the dump options only in the main module and not in Cabal
-- in order to get only code for the main module and not all modules
{-
This module demonstrates the following:
mainMonolithic1Generator performs the same computation as mainMonolithic1Compose
but the former is more than two times slower than latter.
This is serious since in more complex signal processing programs
this factor seems to multiply.
I assume that the problem is that 'mixGen' is not inlined.
Instead GHC seems to have decided to specialise mixGen.
In contrast to mainMonolithic1Compose,
mainMonolithic1Generator uses a data type with existential quantification.
But this alone is not the problem,
since mainMonolithic0 and mainMonolithic0Generator run with the same speed.

The program can be compiled using
> ghc -package storablevector-0.2.5 -O speedtest/SpeedTestChorus.hs


Exporting only 'main' causes warnings about unused functions,
but it also reduces the core output to a third.
-}
module Main (main) where

import qualified Data.StorableVector.Lazy.Builder as Builder
import qualified Data.StorableVector.ST.Strict as SVSTS
import qualified Data.StorableVector.ST.Lazy   as SVSTL
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Lazy as SVL
-- import qualified Data.StorableVector.Private as SVP

import qualified Control.Monad.ST.Strict as StrictST
import Control.Monad.ST.Lazy (strictToLazyST, )

import Foreign.Storable (Storable, )
import GHC.Float (float2Int, int2Float, double2Int, int2Double, )

import qualified Sound.Frame.Stereo as Stereo

-- import qualified Data.Strictness.HT as Strict

import Control.Monad (guard, zipWithM, )
import Data.Monoid (mempty, mappend, )


{-
GHC-6.10.4:
I started with Storable instance for pairs from storable-tuple,
that was implemented using the storable-record framework at this time.
I got run-time around 5 seconds.
When I used inlining then the computation time increased to 8s!
Then I switch to sample-frame:Sound.Frame.Stereo
computation time dropped to 1.4 seconds.
At this time I already switched back
from the storable-record based implementation to a custom one
of the Storable Stereo instance.
With this implementation inlining doesn't change the run-time.
But then I noted that the generated file
contained only one saw wave tone.
This problem disappeared by not using -O2 option, but only -O.
Monolithic and chunky require about 2.6 seconds,
whereas monolithicStrict needs 3.8 seconds.
After inlining monolithicStrict needs 1.8 seconds.
-}

type Phase = (Float, Float, Float)

{-# INLINE saw #-}
saw :: Num a => a -> a
saw t = 1-2*t

{-# INLINE sawChorus #-}
sawChorus :: Phase -> Float
sawChorus (pl0,pl1,pl2) =
   0.3 * (saw pl0 + saw pl1 + saw pl2)

{-
Much faster than @snd . properFraction@ but fails for large numbers.
-}
class (Num a, Ord a) => Fraction a where
   fraction :: a -> a

instance Fraction Float where
   {-# INLINE fraction #-}
   fraction x = x - int2Float (float2Int x)

instance Fraction Double where
   {-# INLINE fraction #-}
   fraction x = x - int2Double (double2Int x)

{-
fraction = Strict.arguments1 $ \x ->
   let y = x - int2Float (float2Int x)
   in  y
-}
{-
   in  if y<0
         then y+1
         else y
-}
{-
   if x==0
     then 0
     else x - int2Float (float2Int x)
-}
--   rnf x `seq` x - int2Float (float2Int x)


{-# INLINE generator0Freq #-}
generator0Freq ::
   Fraction a => a -> a -> Maybe (a, a)
generator0Freq freq =
   \p -> Just (saw p, fraction (p+freq))

{-# INLINE generator0 #-}
generator0 ::
   Float -> Maybe (Float, Float)
generator0 = generator0Freq 0.01


{-# INLINE tone0 #-}
tone0 :: Float -> Float -> SVL.Vector Float
tone0 freq phase =
   SVL.unfoldr SVL.defaultChunkSize (generator0Freq freq) phase


{-
Here we let storablevector functions check
whether we reached the end of the vector.
However, 'SVSTS.maybeWrite' also checks
whether the index is non-negative,
which is unnecessary in our case.
-}
{-# INLINE runLoopSTStrictSafe #-}
runLoopSTStrictSafe ::
   (Storable a) =>
   Int -> (s -> Maybe (a, s)) -> s -> SV.Vector a
runLoopSTStrictSafe n f s =
   SVSTS.runSTVector
      (do v <- SVSTS.new_ n
          let go i s0 =
                case f s0 of
                   Nothing -> return v
                   Just (a,s1) ->
                      SVSTS.maybeWrite v i a >>= \cont ->
                      if cont
                        then go (succ i) s1
                        else return v
          go 0 s)

{-# INLINE runLoopSTStrict #-}
runLoopSTStrict ::
   (Storable a) =>
   Int -> (s -> Maybe (a, s)) -> s -> SV.Vector a
runLoopSTStrict n f s =
   SVSTS.runSTVector
      (do v <- SVSTS.new_ n
          let go i s0 =
                case guard (i<n) >> f s0 of
                   Nothing -> return v
                   Just (a,s1) ->
                      SVSTS.unsafeWrite v i a >> go (succ i) s1
          go 0 s)

{-# INLINE runLoopSTLazy #-}
runLoopSTLazy ::
   (Storable a) =>
   Int -> (s -> Maybe (a, s)) -> s -> SV.Vector a
runLoopSTLazy n f s =
   SVSTL.runSTVector
      (do v <- SVSTL.new_ n
          let go s0 i =
                case guard (i<n) >> f s0 of
                   Nothing -> return v
                   Just (a,s1) ->
                      strictToLazyST (SVSTS.unsafeWrite v i a >> return (succ i))
                       >>= go s1
                      {-
                      Strict pattern matching on () is necessary
                      in order to avoid a memory leak.
                      Working in ST.Lazy is still
                      three times slower than ST.Strict
                      -}
--                      SVSTL.unsafeWrite v i a >>= \() -> go s1 (succ i)
--                      SVSTL.unsafeWrite v i a >> go s1 (succ i)
          go s 0)


{-# INLINE mixST #-}
mixST :: (Storable a, Num a) =>
   SVSTS.Vector s a -> (st -> Maybe (a, st)) -> st -> StrictST.ST s Int
mixST v f s =
   let go i s0 =
         if i < SVSTS.length v
           then
             case f s0 of
                Nothing -> return i
                Just (a,s1) ->
                   SVSTS.unsafeModify v i (a+) >> go (succ i) s1
           else return i
   in  go 0 s

{-# INLINE mixSTGuard #-}
mixSTGuard :: (Storable a, Num a) =>
   SVSTS.Vector s a -> (st -> Maybe (a, st)) -> st -> StrictST.ST s Int
mixSTGuard v f s =
   let go i s0 =
          case guard (i < SVSTS.length v) >> f s0 of
             Nothing -> return i
             Just (a,s1) ->
                SVSTS.unsafeModify v i (a+) >> go (succ i) s1
   in  go 0 s

{-
It seems that mixSTVectorFoldr is essentially slower than mixSTVectorIndex.
The former one should be faster,
since 'foldr' uses direct pointer into the source vector.
-}
{-# INLINE mixSTVectorIndex #-}
mixSTVectorIndex :: (Storable a, Num a) =>
   SVSTS.Vector s a -> SV.Vector a -> StrictST.ST s Int
mixSTVectorIndex dst src =
   let end = min (SVSTS.length dst) (SV.length src)
       go i =
          if i >= end
            then return i
            else
              SVSTS.unsafeModify dst i (SV.index src i +) >>
              go (succ i)
   in  go 0

{-# INLINE mixSTVectorFoldr #-}
mixSTVectorFoldr :: (Storable a, Num a) =>
   SVSTS.Vector s a -> SV.Vector a -> StrictST.ST s Int
mixSTVectorFoldr dst src =
   SV.foldr
      (\x go i ->
         if i >= SVSTS.length dst
           then return i
           else
             SVSTS.unsafeModify dst i (x +) >>
             go (succ i))
      return src 0



{-# INLINE runBuilder #-}
runBuilder ::
   (Storable a) =>
   SVL.ChunkSize -> (s -> Maybe (a, s)) -> s -> SVL.Vector a
runBuilder chunkSize f s =
   Builder.toLazyStorableVector chunkSize
      (let go s0 =
              case f s0 of
                 Nothing -> mempty
                 Just (a,s1) ->
                    mappend (Builder.put a) (go s1)
       in  go s)


infixl 6 `mix`, `mixGen`, `mixVec`

{- |
Build a generator from two other generators
by handling their state in parallel and mix their results.
-}
{-# INLINE mix #-}
mix ::
   (Num y) =>
   (s -> Maybe (y, s)) ->
   (t -> Maybe (y, t)) ->
   ((s,t) -> Maybe (y, (s,t)))
mix f g (s0,t0) =
   do (a,s1) <- f s0
      (b,t1) <- g t0
      return ((a+b), (s1,t1))


{- |
This is like a list without storage.
It is like stream-fusion:Data.Stream
but without Skip constructor.
-}
data Generator a =
   forall s.
      Generator (s -> Maybe (a, s)) s

{-# INLINE runGeneratorMonolithic #-}
runGeneratorMonolithic :: Storable a => Int -> Generator a -> SV.Vector a
runGeneratorMonolithic n (Generator f s) = fst $ SV.unfoldrN n f s

{- SPECIALISE INLINE generator0Gen :: Float -> Float -> Generator Float -}
{-# INLINE generator0Gen #-}
generator0Gen ::
   Fraction a => a -> a -> Generator a
generator0Gen freq phase =
   Generator (\p -> Just (saw p, fraction (p+freq))) phase

{- SPECIALISE INLINE mixGen :: Generator Float -> Generator Float -> Generator Float -}
{-# INLINE mixGen #-}
mixGen ::
   (Num y) =>
   Generator y ->
   Generator y ->
   Generator y
mixGen (Generator f s) (Generator g t) =
   Generator (\(s0,t0) ->
      do (a,s1) <- f s0
         (b,t1) <- g t0
         return ((a+b), (s1,t1))) (s,t)



{-# INLINE incPhase #-}
incPhase :: Phase -> Phase -> Phase
incPhase (d0,d1,d2) (p0,p1,p2) =
   (fraction (p0+d0), fraction (p1+d1), fraction (p2+d2))

{-# INLINE generator1 #-}
generator1 ::
   Phase -> Maybe (Float, Phase)
generator1 =
   \p -> Just (sawChorus p, incPhase dl p)


{-# SPECIALISE mixVec :: SVL.Vector Float -> SVL.Vector Float -> SVL.Vector Float #-}
{- disabled INLINE mixVec -}
mixVec ::
   (Num y, Storable y) =>
   SVL.Vector y ->
   SVL.Vector y ->
   SVL.Vector y
mixVec xs0 ys0 =
   let recourse xt@(x:_) yt@(y:_) =
          let z = SV.zipWith (+) x y
              n = SV.length z
          in  z : recourse
                     (SVL.chunks $ SVL.drop n $ SVL.fromChunks xt)
                     (SVL.chunks $ SVL.drop n $ SVL.fromChunks yt)
       recourse xs [] = xs
       recourse [] ys = ys
   in  SVL.fromChunks $
       recourse (SVL.chunks xs0) (SVL.chunks ys0)


{-# INLINE generator2 #-}
generator2 ::
   (Phase, Phase) -> Maybe (Stereo.T Float, (Phase, Phase))
generator2 =
   \(pl, pr) ->
      Just (Stereo.cons (sawChorus pl) (sawChorus pr),
         (incPhase dl pl, incPhase dr pr))

{-# INLINE dl #-}
{-# INLINE dr #-}
dl, dr :: Phase
(dl,dr) =
   ((0.01008, 0.01003, 0.00990),
    (0.00992, 0.00997, 0.01010))

{-# INLINE initPhase2 #-}
initPhase2 :: (Phase, Phase)
initPhase2 =
   ((0,0.7,0.1), (0.3,0.4,0.6))


size :: Int
size = 10000000


mainSumFoldl :: IO ()
mainSumFoldl =
   print $ SV.foldl (\s x -> s+x+13) 23 (SV.replicate size (42::Int))
{-
stack overflow
-}

mainSumFoldl' :: IO ()
mainSumFoldl' =
   print $ SV.foldl' (\s x -> s+x+13) 23 (SV.replicate size (42::Int))
{-
GHC-6.12.1:

real    0m0.171s
user    0m0.112s
sys     0m0.056s

GHC-7.6.1 (64bit, Toshiba):

real    0m0.100s
user    0m0.080s
sys     0m0.016s
-}

mainSumFoldr :: IO ()
mainSumFoldr =
   print $ SV.foldr (\x go s -> go $! s+x+13) id (SV.replicate size (42::Int)) $! 23
{-
GHC-6.12.1:

real    0m0.503s
user    0m0.464s
sys     0m0.036s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.253s
user    0m0.204s
sys     0m0.044s
-}

mainMonolithic0 :: IO ()
mainMonolithic0 =
   SV.writeFile "speed.f32"
      (fst $ SV.unfoldrN size generator0 0)
{-
GHC-6.10.4:

real    0m0.423s
user    0m0.256s
sys     0m0.152s


GHC-6.12.1:

real    0m0.392s
user    0m0.252s
sys     0m0.140s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.157s
user    0m0.080s
sys     0m0.076s
-}

mainMonolithic0Generator :: IO ()
mainMonolithic0Generator =
   SV.writeFile "speed.f32"
      (runGeneratorMonolithic size
          (generator0Gen (0.01::Float) 0))
{-
GHC-6.12.1:

real    0m0.413s
user    0m0.240s
sys     0m0.172s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.164s
user    0m0.076s
sys     0m0.080s
-}

mainMonolithic0STStrict :: IO ()
mainMonolithic0STStrict =
   SV.writeFile "speed.f32"
      (runLoopSTStrict size (generator0Freq (0.01::Float)) 0)
{-
GHC-6.10.4:

real    0m0.430s
user    0m0.288s
sys     0m0.132s


GHC-6.12.1:

real    0m0.447s
user    0m0.276s
sys     0m0.168s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.164s
user    0m0.064s
sys     0m0.096s
-}

mainMonolithic0STLazy :: IO ()
mainMonolithic0STLazy =
   SV.writeFile "speed.f32"
      (runLoopSTLazy size (generator0Freq (0.01::Float)) 0)
{-
GHC-6.10.4:

real    0m0.886s
user    0m0.752s
sys     0m0.128s

GHC-6.12.1:

real    0m0.763s
user    0m0.620s
sys     0m0.144s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.166s
user    0m0.068s
sys     0m0.096s
-}

mainMonolithic0STMix :: IO ()
mainMonolithic0STMix =
   SV.writeFile "speed.f32" $
   StrictST.runST
      (do v <- SVSTS.new size 0
          l <- mixSTGuard v (generator0Freq (0.01::Float)) 0
          fmap (SV.take l) (SVSTS.unsafeFreeze v))
{-
GHC-6.10.4:

real    0m0.505s
user    0m0.344s
sys     0m0.156s


GHC-6.12.1:

real    0m0.475s
user    0m0.344s
sys     0m0.128s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.196s
user    0m0.100s
sys     0m0.092s
-}

mainMonolithic1 :: IO ()
mainMonolithic1 =
   SV.writeFile "speed.f32"
      (fst $ SV.unfoldrN size generator1 (fst initPhase2))
{-
GHC-6.12.1:

real    0m0.973s
user    0m0.824s
sys     0m0.140s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.252s
user    0m0.160s
sys     0m0.088s
-}

mainMonolithic1Composed :: IO ()
mainMonolithic1Composed =
   SV.writeFile "speed.f32"
      (fst $ SV.unfoldrN size
          (let (f0,f1,f2) = dl
           in  generator0Freq f0 `mix`
               generator0Freq f1 `mix`
               generator0Freq f2)
          (let (p0,p1,p2) = fst initPhase2
           in  ((p0,p1),p2)))
{-
GHC-6.10.4:

real    0m0.974s
user    0m0.812s
sys     0m0.160s


GHC-6.12.1:

real    0m0.940s
user    0m0.800s
sys     0m0.132s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.258s
user    0m0.180s
sys     0m0.076s
-}

mainMonolithic1Generator :: IO ()
mainMonolithic1Generator =
   SV.writeFile "speed.f32"
      (runGeneratorMonolithic size
          (let (f0,f1,f2) = dl
               (p0,p1,p2) = fst initPhase2
           in  generator0Gen f0 p0 `mixGen`
               generator0Gen f1 p1 `mixGen`
               generator0Gen f2 p2))
{-
GHC-6.10.4:

real    0m2.244s
user    0m2.084s
sys     0m0.152s


GHC-6.12.1:

real    0m2.256s
user    0m2.084s
sys     0m0.172s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.262s
user    0m0.172s
sys     0m0.088s
-}

mainMonolithic1GeneratorFold :: IO ()
mainMonolithic1GeneratorFold =
   SV.writeFile "speed.f32"
      (runGeneratorMonolithic size
          (let (f0,f1,f2) = dl
               (p0,p1,p2) = fst initPhase2
           in  foldl1 mixGen $
               map (uncurry generator0Gen) $
               [(f0,p0), (f1,p1), (f2,p2)]))
{-
GHC-6.10.4:

real    0m3.006s
user    0m2.816s
sys     0m0.180s


GHC-6.12.1:

real    0m3.050s
user    0m2.884s
sys     0m0.160s


GHC-7.6.1 (64bit, Toshiba):

real    0m1.057s
user    0m0.940s
sys     0m0.112s
-}

mainMonolithic1STMix :: IO ()
mainMonolithic1STMix =
   SV.writeFile "speed.f32" $
   StrictST.runST
      (do v <- SVSTS.new size 0
          let (f0,f1,f2) = dl
              (p0,p1,p2) = fst initPhase2
          l0 <- mixSTGuard v (generator0Freq f0) p0
          l1 <- mixSTGuard v (generator0Freq f1) p1
          l2 <- mixSTGuard v (generator0Freq f2) p2
          fmap (SV.take (l0 `min` l1 `min` l2)) (SVSTS.unsafeFreeze v))
{-
GHC-6.10.4:

real    0m1.895s
user    0m1.684s
sys     0m0.180s


GHC-6.12.1:

real    0m1.932s
user    0m1.764s
sys     0m0.168s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.350s
user    0m0.256s
sys     0m0.092s
-}

mainMonolithic1STMixZip :: IO ()
mainMonolithic1STMixZip =
   SV.writeFile "speed.f32" $
   StrictST.runST
      (do v <- SVSTS.new size 0
          let (f0,f1,f2) = dl
              (p0,p1,p2) = fst initPhase2
          ls <- zipWithM (mixSTGuard v . generator0Freq)
                   [f0,f1,f2] [p0,p1,p2]
          fmap (SV.take (minimum ls)) (SVSTS.unsafeFreeze v))
{-
GHC-6.10.4:

real    0m1.391s
user    0m1.232s
sys     0m0.160s


GHC-6.12.1:

real    0m1.560s
user    0m1.404s
sys     0m0.136s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.493s
user    0m0.396s
sys     0m0.092s

-}


mainMonolithic1STMixVector :: IO ()
mainMonolithic1STMixVector =
   SV.writeFile "speed.f32" $
   StrictST.runST
      (do v <- SVSTS.new size 0
          let (f0,f1,f2) = dl
              (p0,p1,p2) = fst initPhase2
              osci f p = fst $ SV.unfoldrN size (generator0Freq f) p
          l0 <- mixSTVectorIndex v (osci f0 p0)
          l1 <- mixSTVectorIndex v (osci f1 p1)
          l2 <- mixSTVectorIndex v (osci f2 p2)
          fmap (SV.take (l0 `min` l1 `min` l2)) (SVSTS.unsafeFreeze v))
{-
GHC-6.12.1:

real    0m1.751s
user    0m1.544s
sys     0m0.208s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.615s
user    0m0.468s
sys     0m0.140s
-}

mainMonolithic1STMixVectorZipFoldr :: IO ()
mainMonolithic1STMixVectorZipFoldr =
   SV.writeFile "speed.f32" $
   StrictST.runST
      (do v <- SVSTS.new size 0
          let (f0,f1,f2) = dl
              (p0,p1,p2) = fst initPhase2
              vs = zipWith
                 (\f p -> fst $ SV.unfoldrN size (generator0Freq f) p)
                 [f0,f1,f2] [p0,p1,p2]
          ls <- mapM (mixSTVectorFoldr v) vs
          fmap (SV.take (minimum ls)) (SVSTS.unsafeFreeze v))
{-
GHC-6.12.1:

real    0m3.046s
user    0m2.828s
sys     0m0.216s


GHC-7.6.1 (64bit, Toshiba):

real    0m1.449s
user    0m1.324s
sys     0m0.120s
-}

mainMonolithic1STMixVectorZipIndex :: IO ()
mainMonolithic1STMixVectorZipIndex =
   SV.writeFile "speed.f32" $
   StrictST.runST
      (do v <- SVSTS.new size 0
          let (f0,f1,f2) = dl
              (p0,p1,p2) = fst initPhase2
              vs = zipWith
                 (\f p -> fst $ SV.unfoldrN size (generator0Freq f) p)
                 [f0,f1,f2] [p0,p1,p2]
          ls <- mapM (mixSTVectorIndex v) vs
          fmap (SV.take (minimum ls)) (SVSTS.unsafeFreeze v))
{-
GHC-6.12.1:

real    0m1.782s
user    0m1.532s
sys     0m0.220s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.595s
user    0m0.484s
sys     0m0.108s
-}


mainMonolithic2 :: IO ()
mainMonolithic2 =
   SV.writeFile "speed.f32"
      (fst $ SV.unfoldrN size generator2 initPhase2)
{-
GHC-6.12.1:

real    0m1.852s
user    0m1.588s
sys     0m0.252s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.561s
user    0m0.404s
sys     0m0.152s
-}

{-
mainMonolithicStrict2 :: IO ()
mainMonolithicStrict2 =
   SV.writeFile "speed.f32"
      (fst $ SVP.unfoldrStrictN size generator2 initPhase2)

mainMonolithicTransition2 :: IO ()
mainMonolithicTransition2 =
   SV.writeFile "speed.f32"
      (fst $ SVP.unfoldrTransitionN size
          (\(pl,pr) -> (incPhase dl pl, incPhase dr pr))
          (\(pl,pr) ->
              Just (Stereo.cons (sawChorus pl) (sawChorus pr)))
          initPhase2)
-}


mainChunky0 :: IO ()
mainChunky0 =
   SVL.writeFile "speed.f32"
      (SVL.take size $
       SVL.unfoldr SVL.defaultChunkSize generator0 0)
{-
GHC-6.10.4:

real    0m0.428s
user    0m0.292s
sys     0m0.132s


GHC-6.12.1:

real    0m0.424s
user    0m0.252s
sys     0m0.168s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.186s
user    0m0.076s
sys     0m0.108s
-}

mainChunky0Builder :: IO ()
mainChunky0Builder =
   SVL.writeFile "speed.f32"
      (SVL.take size $
       runBuilder SVL.defaultChunkSize generator0 0)
{-
GHC-6.10.4:

real    0m1.107s
user    0m0.968s
sys     0m0.140s


GHC-6.12.1:

real    0m1.079s
user    0m0.936s
sys     0m0.136s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.139s
user    0m0.048s
sys     0m0.088s
-}

mainChunky1 :: IO ()
mainChunky1 =
   SVL.writeFile "speed.f32"
      (SVL.take size $
       SVL.unfoldr SVL.defaultChunkSize generator1 (fst initPhase2))
{-
GHC-6.10.4:

real    0m0.938s
user    0m0.812s
sys     0m0.116s


GHC-6.12.1:

real    0m0.945s
user    0m0.788s
sys     0m0.152s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.272s
user    0m0.172s
sys     0m0.096s
-}

mainChunky1MixFlat :: IO ()
mainChunky1MixFlat =
   SVL.writeFile "speed.f32"
      (let (f0,f1,f2) = dl
           (p0,p1,p2) = fst initPhase2
       in  SVL.take size $
           tone0 f0 p0 `mixVec`
           tone0 f1 p1 `mixVec`
           tone0 f2 p2)
{-
GHC-6.10.4:

real    0m3.932s
user    0m2.112s
sys     0m0.156s


GHC-6.12.1:

real    0m2.264s
user    0m2.144s
sys     0m0.116s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.708s
user    0m0.424s
sys     0m0.124s
-}

mainChunky1MixFold :: IO ()
mainChunky1MixFold =
   SVL.writeFile "speed.f32"
      (let (f0,f1,f2) = dl
           (p0,p1,p2) = fst initPhase2
       in  SVL.take size $
           foldl1 mixVec $
           map (uncurry tone0) $
           [(f0,p0), (f1,p1), (f2,p2)])
{-
GHC-6.10.4:

real    0m1.611s
user    0m1.476s
sys     0m0.108s


GHC-6.12.1:

real    0m1.555s
user    0m1.416s
sys     0m0.136s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.730s
user    0m0.596s
sys     0m0.132s
-}

mainChunky2 :: IO ()
mainChunky2 =
   SVL.writeFile "speed.f32"
      (SVL.take size $
       SVL.unfoldr SVL.defaultChunkSize generator2 initPhase2)
{-
GHC-6.10.4:

real    0m2.220s
user    0m1.400s
sys     0m0.192s


GHC-6.12.1:

real    0m1.877s
user    0m1.652s
sys     0m0.216s


GHC-7.6.1 (64bit, Toshiba):

real    0m0.591s
user    0m0.420s
sys     0m0.168s
-}

main :: IO ()
main =
   mainSumFoldl'
