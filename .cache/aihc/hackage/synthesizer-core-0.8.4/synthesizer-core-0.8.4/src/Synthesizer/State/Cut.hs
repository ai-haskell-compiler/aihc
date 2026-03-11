{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2006
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.State.Cut (
   {- * dissection -}
   takeUntilPause,
   takeUntilInterval,
   chopStorable,
   chopChunkySize,

   {- * glueing -}
   selectBool,
   select,
   arrange,
   arrangeList,
   ) where

import qualified Synthesizer.State.Signal as Sig

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Generic.CutChunky as CutChunky
import qualified Synthesizer.Generic.Cut as Cut
import Foreign.Storable (Storable)

import qualified Data.EventList.Relative.TimeBody as EventList

import qualified MathObj.LaurentPolynomial as Laurent
import qualified Algebra.RealRing     as RealRing
import qualified Algebra.Additive as Additive

import Control.Applicative (Applicative, )

import qualified Data.List.HT as ListHT
import qualified Data.Array as Array
import Data.Traversable (sequenceA, )
import Data.Tuple.HT (mapFst, mapSnd, )
import Data.Array (Array, Ix, (!), )
import Data.Maybe (fromMaybe, )

import qualified Synthesizer.ChunkySize as ChunkySize
import qualified Number.NonNegative as NonNegW

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
Take signal until it falls short of a certain amplitude for a given time.
-}
{-# INLINE takeUntilPause #-}
takeUntilPause :: (RealRing.C a) => a -> Int -> Sig.T a -> Sig.T a
takeUntilPause y =
   takeUntilInterval ((<=y) . abs)

{- |
Take values until the predicate p holds for n successive values.
The list is truncated at the beginning of the interval of matching values.
-}
{-# INLINE takeUntilInterval #-}
takeUntilInterval :: (a -> Bool) -> Int -> Sig.T a -> Sig.T a
takeUntilInterval p n xs =
   Sig.map fst $
   Sig.takeWhile ((<n) . snd) $
   Sig.zip xs $
   Sig.drop n $
   Sig.append (Sig.scanL (\acc x -> if p x then succ acc else 0) 0 xs) $
   Sig.repeat 0



{-# INLINE selectBool #-}
selectBool :: (Sig.T a, Sig.T a) -> Sig.T Bool -> Sig.T a
selectBool =
   Sig.zipWith (\(xf,xt) c -> if c then xt else xf) .
   uncurry Sig.zip


{-# INLINE select #-}
select :: Ix i => Array i (Sig.T a) -> Sig.T i -> Sig.T a
select =
   Sig.crochetL
      (\xi arr ->
           do arr0 <- sequenceArray (fmap Sig.viewL arr)
              return (fst (arr0!xi), fmap snd arr0))

{-# INLINE sequenceArray #-}
sequenceArray ::
   (Applicative f, Ix i) =>
   Array i (f a) -> f (Array i a)
sequenceArray arr =
   fmap (Array.listArray (Array.bounds arr)) $
   sequenceA (Array.elems arr)


{- |
Given a list of signals with time stamps,
mix them into one signal as they occur in time.
Ideally for composing music.

Cf. 'MathObj.LaurentPolynomial.series'
-}
{-# INLINE arrangeList #-}
arrangeList :: (Additive.C v) =>
       EventList.T NonNegW.Int (Sig.T v)
            {-^ A list of pairs: (relative start time, signal part),
                The start time is relative to the start time
                of the previous event. -}
    -> Sig.T v
            {-^ The mixed signal. -}
arrangeList evs =
   let xs = map Sig.toList (EventList.getBodies evs)
   in  case map NonNegW.toNumber (EventList.getTimes evs) of
          t:ts -> Sig.replicate t zero `Sig.append`
                  Sig.fromList (Laurent.addShiftedMany ts xs)
          []   -> Sig.empty




{-# INLINE arrange #-}
arrange :: (Additive.C v) =>
       EventList.T NonNegW.Int (Sig.T v)
            {-^ A list of pairs: (relative start time, signal part),
                The start time is relative to the start time
                of the previous event. -}
    -> Sig.T v
            {-^ The mixed signal. -}
arrange evs =
   let xs = EventList.getBodies evs
   in  case map NonNegW.toNumber (EventList.getTimes evs) of
          t:ts -> Sig.replicate t zero `Sig.append`
                  addShiftedMany ts xs
          []   -> Sig.empty


{-# INLINE addShiftedMany #-}
addShiftedMany :: (Additive.C a) => [Int] -> [Sig.T a] -> Sig.T a
addShiftedMany ds xss =
   foldr (uncurry addShifted) Sig.empty (zip (ds++[zero]) xss)



{-# INLINE addShifted #-}
addShifted :: Additive.C a => Int -> Sig.T a -> Sig.T a -> Sig.T a
addShifted del xs ys =
   if del < 0
     then error "State.Signal.addShifted: negative shift"
     else
       Sig.runViewL xs (\nextX xs2 ->
       Sig.runViewL ys (\nextY ys2 ->
          Sig.unfoldR
             (\((d,ys0),xs0) ->
                 -- d<0 cannot happen
                 if d==zero
                   then
                     fmap
                        (mapSnd (\(xs1,ys1) -> ((zero,ys1),xs1)))
                        (Sig.zipStep nextX nextY (+) (xs0, ys0))
                   else
                     Just $ mapSnd ((,) (pred d, ys0)) $
                     fromMaybe (zero, xs0) $ nextX xs0)
             ((del,ys2),xs2)
       ))


{- |
Split a storable signal into a sequence of signals.
A new piece is started whenever the Boolean signal contains a 'True'.
The first piece in the result is the part from the beginning until the first 'True'.
That is, if the signal 'Bool' starts with a 'True',
then the first result piece is empty.

When the control signal is at least as long as the storable signal
and if we neglect the chunking structure, then it holds

> concat (chopStorable bs xs) == xs
-}
chopStorable :: Storable a => Sig.T Bool -> SigSt.T a -> [SigSt.T a]
chopStorable = chop

chopChunkySize :: Sig.T Bool -> ChunkySize.T -> [ChunkySize.T]
chopChunkySize = chop


chop :: CutChunky.C chunky => Sig.T Bool -> chunky -> [chunky]
chop bs =
   Sig.runViewL bs $ \f s ->
      let go _ [] = (Cut.empty, [])
          go s0 (chunk:chunks) =
             case chopChunk f chunk s0 of
                (split, ms) ->
                   prependChunks split $
                   case ms of
                      Nothing -> (CutChunky.fromChunks chunks, [])
                      Just s1 -> go s1 chunks
      in  uncurry (:) . go s . CutChunky.toChunks

prependChunks ::
   CutChunky.C chunky =>
   [CutChunky.Chunk chunky] ->
   (chunky, [chunky]) ->
   (chunky, [chunky])
prependChunks [] xs = xs
prependChunks (chunk:chunks) xs =
   let go c0 css =
          mapFst
             (\y ->
                if Cut.null c0
                  then y
                  else CutChunky.fromChunks $ c0 : CutChunky.toChunks y)
             (case css of
                [] -> xs
                (c1:cs) -> (Cut.empty, uncurry (:) (go c1 cs)))
   in  go chunk chunks

chopChunk ::
   Cut.Transform chunk =>
   (s -> Maybe (Bool, s)) ->
   chunk -> s -> ([chunk], Maybe s)
chopChunk f vs =
   let go j s0 =
          if j >= Cut.length vs
            then ([j], Just s0)
            else
              case f s0 of
                 Nothing -> ([j, Cut.length vs], Nothing)
                 Just (b,s1) ->
                    (if b
                       then mapFst (j:)
                       else id) $
                    go (succ j) s1
   in  mapFst
          (ListHT.mapAdjacent (\from to -> Cut.drop from $ Cut.take to vs) . (0:)) .
       go 0
