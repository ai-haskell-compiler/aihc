{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{- |
Type classes that give a uniform interface to
storable signals, stateful signals, lists, fusable lists.
Some of the signal types require constraints on the element type.
Storable signals require Storable elements.
Thus we need multiparameter type classes.
In this module we collect functions
where the element type is not altered by the function.
-}
module Synthesizer.Generic.Signal (
   module Synthesizer.Generic.Signal,
   Cut.null,
   Cut.length,
   Cut.empty,
   Cut.cycle,
   Cut.append,
   Cut.concat,
   Cut.take,
   Cut.drop,
   Cut.dropMarginRem,
   Cut.splitAt,
   Cut.reverse,
   Cut.lengthAtLeast,
   Cut.lengthAtMost,
   Cut.sliceVertical,
   ) where

import qualified Synthesizer.Generic.Cut as Cut
import Synthesizer.Generic.Cut (append, )

import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.State.Signal as SigS
import qualified Synthesizer.Storable.Signal as SigSt
import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV

import qualified Synthesizer.Plain.Modifier as Modifier

import Foreign.Storable (Storable)

import Control.Monad.Trans.State (runState, runStateT, )

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Function (fix, )
import Data.Tuple.HT (mapPair, mapFst, fst3, snd3, thd3, )
import Data.Monoid (Monoid, mappend, mempty, )
import Data.Semigroup (Semigroup, (<>), )

import qualified Algebra.ToInteger    as ToInteger
import qualified Algebra.ToRational   as ToRational
import qualified Algebra.Absolute     as Absolute
import qualified Algebra.RealIntegral as RealIntegral
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.NonNegative  as NonNeg
import qualified Algebra.ZeroTestable as ZeroTestable

import qualified Algebra.Module   as Module
import qualified Algebra.Ring     as Ring
import qualified Algebra.Additive as Additive
import qualified Algebra.Monoid   as Monoid
import Algebra.Additive ((+), (-), )

import qualified Data.EventList.Relative.BodyTime as EventList

import qualified Numeric.NonNegative.Class as NonNeg98

import qualified Test.QuickCheck as QC

import qualified Prelude as P
import Prelude
   (Bool, Int, Maybe(Just), maybe, fst, snd,
    (==), (<), (>), (<=), (>=), compare, Ordering(..),
    flip, uncurry, const, (.), ($), (&&), id, (++),
    fmap, return, error, show,
    Eq, Ord, Show, min, max, )



class Storage signal where

   data Constraints signal :: *

   constraints :: signal -> Constraints signal


class Read0 sig where
   toList :: Storage (sig y) => sig y -> [y]
   toState :: Storage (sig y) => sig y -> SigS.T y
--   toState :: Storage (sig y) => StateT (sig y) Maybe y
   foldL :: Storage (sig y) => (s -> y -> s) -> s -> sig y -> s
   foldR :: Storage (sig y) => (y -> s -> s) -> s -> sig y -> s
   index :: Storage (sig y) => sig y -> Int -> y

class (Cut.Read (sig y), Read0 sig, Storage (sig y)) => Read sig y where

class (Read0 sig) => Transform0 sig where
   cons :: Storage (sig y) => y -> sig y -> sig y
   takeWhile :: Storage (sig y) => (y -> Bool) -> sig y -> sig y
   dropWhile :: Storage (sig y) => (y -> Bool) -> sig y -> sig y
   span :: Storage (sig y) => (y -> Bool) -> sig y -> (sig y, sig y)

   {- |
   When using 'viewL' for traversing a signal,
   it is certainly better to convert to State signal first,
   since this might involve optimized traversing
   like in case of Storable signals.
   -}
   viewL :: Storage (sig y) => sig y -> Maybe (y, sig y)
   viewR :: Storage (sig y) => sig y -> Maybe (sig y, y)

   zipWithAppend :: Storage (sig y) => (y -> y -> y) -> sig y -> sig y -> sig y

   -- functions from Transform2 that are oftenly used with only one type variable
   map ::
      (Storage (sig y0), Storage (sig y1)) =>
      (y0 -> y1) -> (sig y0 -> sig y1)
   scanL ::
      (Storage (sig y0), Storage (sig y1)) =>
      (y1 -> y0 -> y1) -> y1 -> sig y0 -> sig y1
   crochetL ::
      (Storage (sig y0), Storage (sig y1)) =>
      (y0 -> s -> Maybe (y1, s)) -> s -> sig y0 -> sig y1

class (Cut.Transform (sig y), Transform0 sig, Read sig y) => Transform sig y where


{- |
This type is used for specification of the maximum size of strict packets.
Packets can be smaller, can have different sizes in one signal.
In some kinds of streams, like lists and stateful generators,
the packet size is always 1.
The packet size is not just a burden caused by efficiency,
but we need control over packet size in applications with feedback.

ToDo: Make the element type of the corresponding signal a type parameter.
This helps to distinguish chunk sizes of scalar and vectorised signals.
-}
newtype LazySize = LazySize Int
   deriving (Eq, Ord, Show,
             Additive.C, Ring.C, ZeroTestable.C,
             ToInteger.C, ToRational.C, Absolute.C,
             RealIntegral.C, Integral.C)

instance Semigroup LazySize where
   LazySize a <> LazySize b = LazySize (a + b)

instance Monoid LazySize where
   mempty = LazySize 0
   mappend = (<>)

instance Monoid.C LazySize where
   idt = LazySize 0
   LazySize a <*> LazySize b = LazySize (a + b)

instance NonNeg.C LazySize where
   split = NonNeg.splitDefault (\(LazySize n) -> n) LazySize

instance QC.Arbitrary LazySize where
   arbitrary =
      case defaultLazySize of
         LazySize n -> fmap LazySize (QC.choose (1, 2 P.* n))

instance Cut.Read LazySize where
   null (LazySize n) = n==0
   length (LazySize n) = n

instance Cut.Transform LazySize where
   {-# INLINE take #-}
   take m (LazySize n) = LazySize $ min (max 0 m) n
   {-# INLINE drop #-}
   drop m (LazySize n) = LazySize $ max 0 $ n - max 0 m
   {-# INLINE splitAt #-}
   splitAt m x =
      let y = Cut.take m x
      in  (y, x-y)
   {-# INLINE dropMarginRem #-}
   dropMarginRem n m x@(LazySize xs) =
      let d = min m $ max 0 $ xs - n
      in  (m-d, Cut.drop d x)
   {-# INLINE reverse #-}
   reverse = id


{- |
This can be used for internal signals
that have no observable effect on laziness.
E.g. when you construct a list
by @repeat defaultLazySize zero@
we assume that 'zero' is defined for all Additive types.
-}
defaultLazySize :: LazySize
defaultLazySize =
   let (SVL.ChunkSize size) = SVL.defaultChunkSize
   in  LazySize size

{- |
We could provide the 'LazySize' by a Reader monad,
but we don't do that because we expect that the choice of the lazy size
is more local than say the choice of the sample rate.
E.g. there is no need to have the same laziness coarseness
for multiple signal processors.
-}
class Transform0 sig => Write0 sig where
   fromList :: Storage (sig y) => LazySize -> [y] -> sig y
--   fromState :: Storage (sig y) => LazySize -> SigS.T y -> sig y
--   fromState :: Storage (sig y) => LazySize -> StateT s Maybe y -> s -> sig y
   repeat :: Storage (sig y) => LazySize -> y -> sig y
   replicate :: Storage (sig y) => LazySize -> Int -> y -> sig y
   iterate :: Storage (sig y) => LazySize -> (y -> y) -> y -> sig y
   iterateAssociative :: Storage (sig y) => LazySize -> (y -> y -> y) -> y -> sig y
   unfoldR :: Storage (sig y) => LazySize -> (s -> Maybe (y,s)) -> s -> sig y

class (Write0 sig, Transform sig y) => Write sig y where


instance (Storable y) => Storage (SVL.Vector y) where
   data Constraints (SVL.Vector y) = Storable y => StorableLazyConstraints
   constraints _ = StorableLazyConstraints


readSVL ::
   (Storable a => SVL.Vector a -> b) ->
   (Storage (SVL.Vector a) => SVL.Vector a -> b)
readSVL f x = case constraints x of StorableLazyConstraints -> f x

writeSVL ::
   (Storable a => SVL.Vector a) ->
   (Storage (SVL.Vector a) => SVL.Vector a)
writeSVL x =
   let z = case constraints z of StorableLazyConstraints -> x
   in  z

{-
getSVL ::
   Storable a =>
   (Storage SVL.Vector a => SVL.Vector a) ->
   (SVL.Vector a)
getSVL x = case constraints x of StorableLazyConstraints -> x
-}

instance Storable y => Read SVL.Vector y where

-- instance Storable y => Read SigSt.T y where
instance Read0 SVL.Vector where
   {-# INLINE toList #-}
   toList = readSVL SVL.unpack
   {-# INLINE toState #-}
   toState = readSVL SigS.fromStorableSignal
   {-# INLINE foldL #-}
   foldL f x = readSVL (SVL.foldl f x)
   {-# INLINE foldR #-}
   foldR f x = readSVL (SVL.foldr f x)
   {-# INLINE index #-}
   index = readSVL SVL.index


instance Storable y => Transform SVL.Vector y where

instance Transform0 SVL.Vector where
   {-# INLINE cons #-}
   cons x = readSVL (SVL.cons x)
   {-# INLINE takeWhile #-}
   takeWhile p = readSVL (SVL.takeWhile p)
   {-# INLINE dropWhile #-}
   dropWhile p = readSVL (SVL.dropWhile p)
   {-# INLINE span #-}
   span p = readSVL (SVL.span p)

   {-# INLINE viewL #-}
   viewL = readSVL SVL.viewL
   {-# INLINE viewR #-}
   viewR = readSVL SVL.viewR

   {-# INLINE map #-}
   map f x = writeSVL (readSVL (SVL.map f) x)
   {-# INLINE scanL #-}
   scanL f a x = writeSVL (readSVL (SVL.scanl f a) x)
   {-# INLINE crochetL #-}
   crochetL f a x = writeSVL (readSVL (SVL.crochetL f a) x)
   {-# INLINE zipWithAppend #-}
   zipWithAppend f = readSVL (SigSt.zipWithAppend f)



withStorableContext ::
   (SVL.ChunkSize -> a) -> (LazySize -> a)
withStorableContext f =
   \(LazySize size) -> f (SVL.ChunkSize size)

instance Storable y => Write SVL.Vector y where

instance Write0 SVL.Vector where
   {-# INLINE fromList #-}
   fromList = withStorableContext $ \size x -> writeSVL (SVL.pack size x)
   {-# INLINE repeat #-}
   repeat = withStorableContext $ \size x -> writeSVL (SVL.repeat size x)
   {-# INLINE replicate #-}
   replicate = withStorableContext $ \size n x -> writeSVL (SVL.replicate size n x)
   {-# INLINE iterate #-}
   iterate = withStorableContext $ \size f x -> writeSVL (SVL.iterate size f x)
   {-# INLINE unfoldR #-}
   unfoldR = withStorableContext $ \size f x -> writeSVL (SVL.unfoldr size f x)
   {-# INLINE iterateAssociative #-}
   iterateAssociative = withStorableContext $ \size op x -> writeSVL (SVL.iterate size (op x) x) -- should be optimized



instance (Storable y) => Storage (SV.Vector y) where
   data Constraints (SV.Vector y) = Storable y => StorableConstraints
   constraints _ = StorableConstraints

readSV ::
   (Storable a => SV.Vector a -> b) ->
   (Storage (SV.Vector a) => SV.Vector a -> b)
readSV f x = case constraints x of StorableConstraints -> f x

writeSV ::
   (Storable a => SV.Vector a) ->
   (Storage (SV.Vector a) => SV.Vector a)
writeSV x =
   let z = case constraints z of StorableConstraints -> x
   in  z


instance Storable y => Read SV.Vector y where

instance Read0 SV.Vector where
   {-# INLINE toList #-}
   toList = readSV SV.unpack
   {-# INLINE toState #-}
   toState = readSV SigS.fromStrictStorableSignal
   {-# INLINE foldL #-}
   foldL f x = readSV (SV.foldl f x)
   {-# INLINE foldR #-}
   foldR f x = readSV (SV.foldr f x)
   {-# INLINE index #-}
   index = readSV SV.index

instance Storable y => Transform SV.Vector y where

instance Transform0 SV.Vector where
   {-# INLINE cons #-}
   cons x = readSV (SV.cons x)
   {-# INLINE takeWhile #-}
   takeWhile p = readSV (SV.takeWhile p)
   {-# INLINE dropWhile #-}
   dropWhile p = readSV (SV.dropWhile p)
   {-# INLINE span #-}
   span p = readSV (SV.span p)

   {-# INLINE viewL #-}
   viewL = readSV SV.viewL
   {-# INLINE viewR #-}
   viewR = readSV SV.viewR

   {-# INLINE map #-}
   map f x = writeSV (readSV (SV.map f) x)
   {-# INLINE scanL #-}
   scanL f a x = writeSV (readSV (SV.scanl f a) x)
   {-# INLINE crochetL #-}
   crochetL f a x =
      writeSV (fst (readSV (SV.crochetLResult f a) x))
      -- fst . SV.crochetContL f acc
   {-# INLINE zipWithAppend #-}
   zipWithAppend f =
      readSV (\xs ys ->
         case compare (SV.length xs) (SV.length ys) of
            EQ -> SV.zipWith f xs ys
            LT -> SV.append (SV.zipWith f xs ys) (SV.drop (SV.length xs) ys)
            GT -> SV.append (SV.zipWith f xs ys) (SV.drop (SV.length ys) xs))



instance Storage [y] where
   data Constraints [y] = ListConstraints
   constraints _ = ListConstraints

instance Read [] y where

instance Read0 [] where
   {-# INLINE toList #-}
   toList = id
   {-# INLINE toState #-}
   toState = SigS.fromList
   {-# INLINE foldL #-}
   foldL = List.foldl
   {-# INLINE foldR #-}
   foldR = List.foldr
   {-# INLINE index #-}
   index = (List.!!)


instance Transform [] y where

instance Transform0 [] where
   {-# INLINE cons #-}
   cons = (:)
   {-# INLINE takeWhile #-}
   takeWhile = List.takeWhile
   {-# INLINE dropWhile #-}
   dropWhile = List.dropWhile
   {-# INLINE span #-}
   span = List.span

   {-# INLINE viewL #-}
   viewL = ListHT.viewL
   {-# INLINE viewR #-}
   viewR = ListHT.viewR

   {-# INLINE map #-}
   map = List.map
   {-# INLINE scanL #-}
   scanL = List.scanl
   {-# INLINE crochetL #-}
   crochetL = Sig.crochetL
   {-# INLINE zipWithAppend #-}
   zipWithAppend = Sig.zipWithAppend


instance Write [] y where

instance Write0 [] where
   {-# INLINE fromList #-}
   fromList _ = id
   {-# INLINE repeat #-}
   repeat _ = List.repeat
   {-# INLINE replicate #-}
   replicate _ = List.replicate
   {-# INLINE iterate #-}
   iterate _ = List.iterate
   {-# INLINE unfoldR #-}
   unfoldR _ = List.unfoldr
   {-# INLINE iterateAssociative #-}
   iterateAssociative _ = ListHT.iterateAssociative



instance Storage (SigS.T y) where
   data Constraints (SigS.T y) = StateConstraints
   constraints _ = StateConstraints

instance Read SigS.T y

instance Read0 SigS.T where
   {-# INLINE toList #-}
   toList = SigS.toList
   {-# INLINE toState #-}
   toState = id
   {-# INLINE foldL #-}
   foldL = SigS.foldL
   {-# INLINE foldR #-}
   foldR = SigS.foldR
   {-# INLINE index #-}
   index = indexByDrop


instance Transform SigS.T y

instance Transform0 SigS.T where
   {-# INLINE cons #-}
   cons = SigS.cons
   {-# INLINE takeWhile #-}
   takeWhile = SigS.takeWhile
   {-# INLINE dropWhile #-}
   dropWhile = SigS.dropWhile
   {-# INLINE span #-}
   span p =
      -- This implementation is slow. Better leave it unimplemented?
      mapPair (SigS.fromList, SigS.fromList) .
      List.span p . SigS.toList

   {-# INLINE viewL #-}
   viewL = SigS.viewL
   {-# INLINE viewR #-}
   viewR =
      -- This implementation is slow. Better leave it unimplemented?
      fmap (mapFst SigS.fromList) .
      ListHT.viewR . SigS.toList

   {-# INLINE map #-}
   map = SigS.map
   {-# INLINE scanL #-}
   scanL = SigS.scanL
   {-# INLINE crochetL #-}
   crochetL = SigS.crochetL
   {-# INLINE zipWithAppend #-}
   zipWithAppend = SigS.zipWithAppend


instance Write SigS.T y

instance Write0 SigS.T where
   {-# INLINE fromList #-}
   fromList _ = SigS.fromList
   {-# INLINE repeat #-}
   repeat _ = SigS.repeat
   {-# INLINE replicate #-}
   replicate _ = SigS.replicate
   {-# INLINE iterate #-}
   iterate _ = SigS.iterate
   {-# INLINE unfoldR #-}
   unfoldR _ = SigS.unfoldR
   {-# INLINE iterateAssociative #-}
   iterateAssociative _ = SigS.iterateAssociative


instance Storage (EventList.T time y) where
   data Constraints (EventList.T time y) = EventListConstraints
   constraints _ = EventListConstraints

instance (NonNeg98.C time, P.Integral time) =>
      Read (EventList.T time) y where

instance (NonNeg98.C time, P.Integral time) =>
      Read0 (EventList.T time) where
   {-# INLINE toList #-}
   toList =
      List.concatMap (uncurry (flip List.genericReplicate)) .
      EventList.toPairList
   {-# INLINE toState #-}
   toState = SigS.fromPiecewiseConstant
   {-# INLINE foldL #-}
   foldL f x = SigS.foldL f x . toState
   {-# INLINE foldR #-}
   foldR f x = SigS.foldR f x . toState
   {-# INLINE index #-}
   index sig n =
      EventList.foldrPair
         (\b t go k ->
            if k < t
              then b
              else go (t NonNeg98.-| k))
         (error $ "EventList.index: positions " ++ show n ++ " out of range")
         sig
         (P.fromIntegral n)

instance (NonNeg98.C time, P.Integral time) =>
      Transform (EventList.T time) y where

instance (NonNeg98.C time, P.Integral time) =>
      Transform0 (EventList.T time) where
   {-# INLINE cons #-}
   cons b = EventList.cons b (P.fromInteger 1)
   {-# INLINE takeWhile #-}
   takeWhile p =
      EventList.foldrPair
         (\b t rest ->
            if p b
              then EventList.cons b t rest
              else EventList.empty)
         EventList.empty
   {-# INLINE dropWhile #-}
   dropWhile p =
      let recourse xs =
             flip (EventList.switchL EventList.empty) xs $ \b _t rest ->
             if p b
               then recourse rest
               else xs
      in  recourse
   {-# INLINE span #-}
   span p =
      let recourse xs =
             flip (EventList.switchL (EventList.empty,EventList.empty)) xs $ \b t rest ->
             if p b
               then mapFst (EventList.cons b t) $ recourse rest
               else (EventList.empty, xs)
      in  recourse

   {-# INLINE viewL #-}
   viewL xs = do
      ((b,t),ys) <- EventList.viewL xs
      if t>0
        then Just (b, if t==1 then ys else EventList.cons b (t NonNeg98.-|1) ys)
        else viewL ys
   {-# INLINE viewR #-}
   viewR =
      let dropTrailingZeros =
             EventList.foldrPair
                (\b t rest ->
                   if t==0 && EventList.null rest
                     then EventList.empty
                     else EventList.cons b t rest)
                EventList.empty
          recourse (b,t) =
             EventList.switchL
                (if t<=1
                   then EventList.empty
                   else EventList.singleton b (t NonNeg98.-| 1),
                 b)
                (\b0 t0 xs0 ->
                   mapFst (EventList.cons b t) $ recourse (b0,t0) xs0)
      in  fmap (uncurry recourse) . EventList.viewL . dropTrailingZeros

   {-# INLINE map #-}
   map = fmap
   {-# INLINE scanL #-}
   scanL f x =
      fromState (LazySize 1) . SigS.scanL f x . toState
   {-# INLINE crochetL #-}
   crochetL f x =
      fromState (LazySize 1) . SigS.crochetL f x . toState
   {-# INLINE zipWithAppend #-}
   zipWithAppend f =
      let recourse xs ys =
             flip (EventList.switchL ys) xs $ \x xn xs0 ->
             flip (EventList.switchL xs) ys $ \y yn ys0 ->
             let n = min xn yn
                 drop_ a an as0 =
                    if n>=an
                      then as0
                      else EventList.cons a (an NonNeg98.-| n) as0
             in  EventList.cons (f x y) n $
                 recourse
                    (drop_ x xn xs0)
                    (drop_ y yn ys0)
      in  recourse



instance (NonNeg98.C time, P.Integral time) => Write (EventList.T time) y where

instance (NonNeg98.C time, P.Integral time) => Write0 (EventList.T time) where
   {-# INLINE fromList #-}
   fromList _ =
      EventList.fromPairList .
      List.map (flip (,) (P.fromInteger 1))
   {-# INLINE repeat #-}
   repeat (LazySize n) a =
      let xs = EventList.cons a (P.fromIntegral n) xs
      in  xs
   {-# INLINE replicate #-}
   replicate size m a =
      Cut.take m (repeat size a)
   {-# INLINE iterate #-}
   iterate size f =
      fromState size . SigS.iterate f
   {-# INLINE unfoldR #-}
   unfoldR _size f =
      let recourse =
             maybe EventList.empty
                (\(x,s) -> EventList.cons x
                   (P.fromInteger 1) (recourse s)) . f
      in  recourse
   {-# INLINE iterateAssociative #-}
   iterateAssociative size f x = iterate size (f x) x


{-# INLINE switchL #-}
switchL :: (Transform sig y) =>
   a -> (y -> sig y -> a) -> sig y -> a
switchL nothing just =
   maybe nothing (uncurry just) . viewL

{-# INLINE switchR #-}
switchR :: (Transform sig y) =>
   a -> (sig y -> y -> a) -> sig y -> a
switchR nothing just =
   maybe nothing (uncurry just) . viewR

{-# INLINE runViewL #-}
runViewL ::
   (Read sig y) =>
   sig y ->
   (forall s. (s -> Maybe (y, s)) -> s -> x) ->
   x
runViewL xs =
   SigS.runViewL (toState xs)

{-# INLINE runSwitchL #-}
runSwitchL ::
   (Read sig y) =>
   sig y ->
   (forall s. (forall z. z -> (y -> s -> z) -> s -> z) -> s -> x) ->
   x
runSwitchL xs =
   SigS.runSwitchL (toState xs)


{-# INLINE singleton #-}
singleton :: (Transform sig y) => y -> sig y
singleton x = cons x mempty

{-# INLINE mix #-}
mix :: (Additive.C y, Transform sig y) =>
   sig y -> sig y -> sig y
mix = zipWithAppend (Additive.+)

{-# INLINE zip #-}
zip :: (Read sig a, Transform sig b, Transform sig (a,b)) =>
   sig a -> sig b -> sig (a,b)
zip = zipWith (,)

{-# INLINE zipWith #-}
zipWith :: (Read sig a, Transform sig b, Transform sig c) =>
   (a -> b -> c) -> (sig a -> sig b -> sig c)
zipWith h = zipWithState h . toState

{-# INLINE zipWith3 #-}
zipWith3 :: (Read sig a, Read sig b, Transform sig c) =>
   (a -> b -> c -> c) -> (sig a -> sig b -> sig c -> sig c)
zipWith3 h as bs = zipWithState3 h (toState as) (toState bs)

{-# INLINE zipWithState #-}
zipWithState :: (Transform sig b, Transform sig c) =>
   (a -> b -> c) -> SigS.T a -> sig b -> sig c
zipWithState f sig =
   SigS.runViewL sig (\next ->
   crochetL (\b as0 ->
      do (a,as1) <- next as0
         Just (f a b, as1)))

{-# INLINE zipWithState3 #-}
zipWithState3 :: (Transform sig c, Transform sig d) =>
   (a -> b -> c -> d) -> (SigS.T a -> SigS.T b -> sig c -> sig d)
zipWithState3 h a b =
   zipWithState ($) (SigS.zipWith h a b)



{-# INLINE unzip #-}
unzip :: (Transform sig (a,b), Transform sig a, Transform sig b) =>
   sig (a,b) -> (sig a, sig b)
unzip xs =
   (map fst xs, map snd xs)

{-# INLINE unzip3 #-}
unzip3 :: (Transform sig (a,b,c), Transform sig a, Transform sig b, Transform sig c) =>
   sig (a,b,c) -> (sig a, sig b, sig c)
unzip3 xs =
   (map fst3 xs, map snd3 xs, map thd3 xs)



{- |
@takeStateMatch len xs@
keeps a prefix of @xs@ of the same length and block structure as @len@
and stores it in the same type of container as @len@.
-}
{-# INLINE takeStateMatch #-}
takeStateMatch :: (Transform sig a, Transform sig b) =>
   sig a -> SigS.T b -> sig b
takeStateMatch x y =
   zipWithState const y x


{-# INLINE delay #-}
delay :: (Write sig y) =>
   LazySize -> y -> Int -> sig y -> sig y
delay size z n =
   append (replicate size n z)

{-# INLINE delayLoop #-}
delayLoop ::
   (Transform sig y) =>
      (sig y -> sig y)
            -- ^ processor that shall be run in a feedback loop
   -> sig y -- ^ prefix of the output, its length determines the delay
   -> sig y
delayLoop proc prefix =
   fix (append prefix . proc)


{-# INLINE delayLoopOverlap #-}
delayLoopOverlap ::
   (Additive.C y, Write sig y) =>
      Int
   -> (sig y -> sig y)
            {- ^ Processor that shall be run in a feedback loop.
                 It's absolutely necessary that this function preserves the chunk structure
                 and that it does not look a chunk ahead.
                 That's guaranteed for processes that do not look ahead at all,
                 like 'SVL.map', 'SVL.crochetL' and
                 all of type @Causal.Process@. -}
   -> sig y -- ^ input
   -> sig y -- ^ output has the same length as the input
delayLoopOverlap time proc xs =
   fix (zipWith (Additive.+) xs .
        delay defaultLazySize Additive.zero time . proc)



{-# INLINE sum #-}
sum :: (Additive.C a, Read sig a) => sig a -> a
sum = foldL (Additive.+) Additive.zero

{-# INLINE sum1 #-}
sum1 :: (Additive.C a, Read sig a) => sig a -> a
sum1 = SigS.foldL1 (Additive.+) . toState
{-
sum1 :: (Additive.C a, Transform sig a) => sig a -> a
sum1 =
   switchL
      (error "Generic.Signal.sum1: signal must be non-empty in order to avoid to use a non-existing zero")
      (foldL (Additive.+))
-}


{-# INLINE foldMap #-}
foldMap :: (Read sig a, Monoid m) => (a -> m) -> sig a -> m
foldMap f = foldR (mappend . f) mempty

{-# DEPRECATED monoidConcatMap "Use foldMap instead." #-}
{-# INLINE monoidConcatMap #-}
monoidConcatMap :: (Read sig a, Monoid m) => (a -> m) -> sig a -> m
monoidConcatMap = foldMap


{-# INLINE tails #-}
tails :: (Transform sig y) => sig y -> SigS.T (sig y)
tails =
   SigS.unfoldR (fmap (\x -> (x, fmap snd (viewL x)))) . Just

{- |
Like 'tail', but for an empty signal it simply returns an empty signal.
-}
{-# INLINE laxTail #-}
laxTail :: (Transform sig y) => sig y -> sig y
laxTail xs =
   switchL xs (flip const) xs

{-# INLINE mapAdjacent #-}
mapAdjacent :: (Read sig a, Transform sig a) =>
   (a -> a -> a) -> sig a -> sig a
mapAdjacent f xs0 =
   let xs1 = maybe xs0 snd (viewL xs0)
   in  zipWith f xs0 xs1

{-# INLINE modifyStatic #-}
modifyStatic :: (Transform sig a) =>
   Modifier.Simple s ctrl a a -> ctrl -> sig a -> sig a
modifyStatic (Modifier.Simple state proc) control =
   crochetL (\a acc -> Just (runState (proc control a) acc)) state

{-| Here the control may vary over the time. -}
{-# INLINE modifyModulated #-}
modifyModulated :: (Transform sig a, Transform sig b, Read sig ctrl) =>
   Modifier.Simple s ctrl a b -> sig ctrl -> sig a -> sig b
modifyModulated (Modifier.Simple state proc) control =
   runViewL control (\next c0 ->
   crochetL
      (\x (acc0,cs0) ->
         do (c,cs1) <- next cs0
            let (y,acc1) = runState (proc c x) acc0
            return (y,(acc1,cs1)))
      (state, c0))
{-
modifyModulated (Modifier.Simple state proc) control x =
   crochetL
      (\ca acc -> Just (runState (uncurry proc ca) acc))
      state (zip control x)
-}

-- cf. Module.linearComb
{-# INLINE linearComb #-}
linearComb ::
   (Module.C t y, Read sig t, Read sig y) =>
   sig t -> sig y -> y
linearComb ts ys =
   SigS.sum (SigS.zipWith (Module.*>) (toState ts) (toState ys))


fromState :: (Write sig y) =>
   LazySize -> SigS.T y -> sig y
fromState size (SigS.Cons f x) =
   unfoldR size (runStateT f) x

{-# INLINE extendConstant #-}
extendConstant :: (Write sig y) =>
   LazySize -> sig y -> sig y
extendConstant size xt =
   maybe
      xt
      (append xt . repeat size . snd)
      (viewR xt)

snoc :: (Transform sig y) => sig y -> y -> sig y
snoc xs x = append xs $ singleton x


-- comonadic 'bind'
-- only non-empty suffixes are processed
{-# INLINE mapTails #-}
mapTails :: (Transform sig a) =>
   (sig a -> a) -> sig a -> sig a
mapTails f x =
   crochetL (\_ xs0 ->
      do (_,xs1) <- viewL xs0
         Just (f xs0, xs1))
      x x
{-
Implementation with unfoldR is more natural,
but it could not preserve the chunk structure of the input signal.
Thus we prefer crochetL, although we do not consume single elements of the input signal.
-}
mapTailsAlt ::
   (Transform sig a, Write sig b) =>
   LazySize -> (sig a -> b) -> sig a -> sig b
mapTailsAlt size f =
   unfoldR size (\xs ->
      do (_,ys) <- viewL xs
         Just (f xs, ys))

{- |
Only non-empty suffixes are processed.
More oftenly we might need

> zipWithTails :: (Read sig b, Transform2 sig a) =>
>    (b -> sig a -> a) -> sig b -> sig a -> sig a

this would preserve the chunk structure of @sig a@,
but it is a bit more hassle to implement that.
-}
{-# INLINE zipWithTails #-}
zipWithTails :: (Transform sig a, Transform sig b, Transform sig c) =>
   (a -> sig b -> c) -> sig a -> sig b -> sig c
zipWithTails f =
   flip (crochetL (\x ys0 ->
      do (_,ys) <- viewL ys0
         Just (f x ys0, ys)))

{-
instance (Additive.C y, Sample.C y, C sig) => Additive.C (sig y) where
   (+) = mix
   negate = map Additive.negate
-}


indexByDrop :: (Transform sig a) => sig a -> Int -> a
indexByDrop xs n =
   if n<0
     then error $ "Generic.index: negative index " ++ show n
     else switchL
             (error $ "Generic.index: index too large " ++ show n)
             const
             (Cut.drop n xs)


{-
This does not work, because we can constrain only the instances of Data
but this is not checked when implementing methods of C.

class Data sig y where

class C sig where
   add :: (Data sig y, Additive.C y) => sig y -> sig y -> sig y
   map :: (Data sig a, Data sig b) => (a -> b) -> (sig a -> sig b)
   zipWith :: (Data sig a, Data sig b, Data sig c) =>
                  (a -> b -> c) -> (sig a -> sig b -> sig c)
-}

{-
This does not work, because we would need type parameters for all occuring element types.

class C sig y where
   add :: (Additive.C y) => sig y -> sig y -> sig y
   map :: C sig a => (a -> y) -> (sig a -> sig y)
   zipWith :: (a -> b -> y) -> (sig a -> sig b -> sig y)
-}
