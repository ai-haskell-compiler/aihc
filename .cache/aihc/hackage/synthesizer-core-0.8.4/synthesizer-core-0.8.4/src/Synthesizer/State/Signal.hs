{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{- |
ToDo:
Better name for the module is certainly
  Synthesizer.Generator.Signal
-}
module Synthesizer.State.Signal where

import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Data.List as List

import qualified Algebra.Module   as Module
import qualified Algebra.Additive as Additive
import Algebra.Module ((*>))
import Algebra.Additive (zero)

import qualified Synthesizer.Format as Format

import qualified Data.EventList.Relative.BodyTime as EventList
import qualified Numeric.NonNegative.Class as NonNeg98
import Numeric.NonNegative.Class ((-|), )

import Control.Monad.Trans.State
          (runState, StateT(StateT), runStateT, )
import Control.Monad (Monad, mplus, msum,
           (>>), (>>=), fail, return, (=<<),
           liftM2,
           Functor, fmap, )

import qualified Control.Applicative as App

import Data.Foldable (Foldable, foldr, )
import Data.Monoid (Monoid, mappend, mempty, )
import Data.Semigroup (Semigroup, (<>), )

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Data.StorableVector.Lazy.Pattern as SVL
import qualified Data.StorableVector.Lazy.Pointer as PtrSt
import qualified Data.StorableVector as V
import Foreign.Storable (Storable)

import qualified Data.List.HT as ListHT
import Data.Tuple.HT (mapFst, mapSnd, mapPair, fst3, snd3, thd3, )
import Data.Function.HT (nest, )
import Data.Maybe.HT (toMaybe, )
import Data.Bool.HT (if', )
import NumericPrelude.Numeric (Float, Double, fromInteger, )

import Text.Show (Show(showsPrec), show, showParen, showString, )
import Data.Maybe (Maybe(Just, Nothing), maybe, fromMaybe, )
import qualified Prelude as P
import Prelude
   ((.), ($), id, const, flip, curry, uncurry, fst, snd, error,
    (>), (>=), max, Ord, (==), Eq,
    succ, pred, Bool(True,False), (&&), not, Int,
--    fromInteger,
    (++),
    seq,
    )


-- | Cf. StreamFusion  Data.Stream
data T a =
   forall s. -- Seq s =>
      Cons !(StateT s Maybe a)  -- compute next value
           !s                   -- initial state


instance (Show y) => Show (T y) where
   showsPrec p x =
      showParen (p >= 10)
         (showString "StateSignal.fromList " . showsPrec 11 (toList x))

instance (Eq y) => Eq (T y) where
   (==) = equal

instance Format.C T where
   format = showsPrec

instance Functor T where
   fmap g (Cons f s) = Cons (fmap g f) s

instance Foldable T where
   foldr = foldR

instance App.Applicative T where
   pure = singleton
   x <*> y = liftA2 ($) x y

instance Monad T where
   return = singleton
   x >>= k =
      runViewL x $ \f s0 ->
      flip generate (fmap (mapFst k) $ f s0) $ \m ->
      m >>=
      let go (y,s) =
             mplus
                (fmap (\(y1,ys) -> (y1, Just (ys,s))) (viewL y))
                (fmap (mapFst k) (f s) >>= go)
      in  go


{- |
It is a common pattern to use @switchL@ or @viewL@ in a loop
in order to traverse a signal.
However this needs repeated packing and unpacking
of the 'viewL' function and the state.
It seems that GHC is not clever enough to detect,
that the 'view' function does not change.
With 'runViewL' you can unpack a stream once
and use an efficient 'viewL' in the loop.
-}
{-# INLINE runViewL #-}
runViewL ::
   T y ->
   (forall s. (s -> Maybe (y, s)) -> s -> x) ->
   x
runViewL (Cons f s) cont =
   cont (runStateT f) s

{-# INLINE runSwitchL #-}
runSwitchL ::
   T y ->
   (forall s. (forall z. z -> (y -> s -> z) -> s -> z) -> s -> x) ->
   x
runSwitchL sig cont =
   runViewL sig (\next ->
      cont (\n j -> maybe n (uncurry j) . next))

{-# INLINE generate #-}
generate :: (acc -> Maybe (y, acc)) -> acc -> T y
generate f = Cons (StateT f)

{-# INLINE unfoldR #-}
unfoldR :: (acc -> Maybe (y, acc)) -> acc -> T y
unfoldR = generate

{-# INLINE generateInfinite #-}
generateInfinite :: (acc -> (y, acc)) -> acc -> T y
generateInfinite f = generate (Just . f)

{-# INLINE fromList #-}
fromList :: [y] -> T y
fromList = generate ListHT.viewL

{-# INLINE toList #-}
toList :: T y -> [y]
toList (Cons f x0) =
   List.unfoldr (runStateT f) x0


{-# INLINE fromStorableSignal #-}
fromStorableSignal ::
   (Storable a) =>
   SigSt.T a -> T a
fromStorableSignal =
   generate PtrSt.viewL .
   PtrSt.cons

{-# INLINE fromStrictStorableSignal #-}
fromStrictStorableSignal ::
   (Storable a) =>
   V.Vector a -> T a
fromStrictStorableSignal xs =
   map (V.index xs) $ take (V.length xs) $ iterate succ zero

{-# INLINE toStorableSignal #-}
toStorableSignal ::
   (Storable a) =>
   SigSt.ChunkSize -> T a -> SigSt.T a
toStorableSignal size (Cons f a) =
   SigSt.unfoldr size (runStateT f) a

{-# INLINE toStrictStorableSignal #-}
toStrictStorableSignal ::
   (Storable a) =>
   Int -> T a -> V.Vector a
toStrictStorableSignal size (Cons f a) =
   fst $ V.unfoldrN size (runStateT f) a

-- needed in synthesizer-alsa
{-# INLINE toStorableSignalVary #-}
toStorableSignalVary ::
   (Storable a) =>
   SVL.LazySize -> T a -> SigSt.T a
toStorableSignalVary size (Cons f a) =
   fst $ SVL.unfoldrN size (runStateT f) a

fromPiecewiseConstant ::
   (NonNeg98.C time, P.Integral time) =>
   EventList.T time a -> T a
fromPiecewiseConstant xs0 =
   generate
      (let go ((x,n),xs) =
             if' (n == P.fromInteger 0)
               (go =<< EventList.viewL xs)
               (Just (x, ((x, n -| P.fromInteger 1), xs)))
       in  go)
      ((error "if counter is zero, the sample value is invalid", P.fromInteger 0), xs0)


{-# INLINE iterate #-}
iterate :: (a -> a) -> a -> T a
iterate f = generateInfinite (\x -> (x, f x))

{-# INLINE iterateAssociative #-}
iterateAssociative :: (a -> a -> a) -> a -> T a
iterateAssociative op x = iterate (op x) x -- should be optimized

{-# INLINE repeat #-}
repeat :: a -> T a
repeat = iterate id




{-# INLINE crochetL #-}
crochetL :: (x -> acc -> Maybe (y, acc)) -> acc -> T x -> T y
crochetL g b (Cons f a) =
   Cons
      (StateT (\(a0,b0) ->
          do (x0,a1) <- runStateT f a0
             (y0,b1) <- g x0 b0
             Just (y0, (a1,b1))))
      (a,b)


{-# INLINE scanL #-}
scanL :: (acc -> x -> acc) -> acc -> T x -> T acc
scanL f start =
   cons start .
   crochetL (\x acc -> let y = f acc x in Just (y, y)) start


{-# INLINE scanLClip #-}
-- | input and output have equal length, that's better for fusion
scanLClip :: (acc -> x -> acc) -> acc -> T x -> T acc
scanLClip f start =
   crochetL (\x acc -> Just (acc, f acc x)) start

{-# INLINE map #-}
map :: (a -> b) -> (T a -> T b)
map = fmap
-- map f = crochetL (\x _ -> Just (f x, ())) ()


{- |
This function will recompute the input lists
and is thus probably not what you want.
If you want to avoid recomputation please consider Causal.Process.
-}
{-# INLINE unzip #-}
unzip :: T (a,b) -> (T a, T b)
unzip x = (map fst x, map snd x)

{-# INLINE unzip3 #-}
unzip3 :: T (a,b,c) -> (T a, T b, T c)
unzip3 xs = (map fst3 xs, map snd3 xs, map thd3 xs)


{-# INLINE delay1 #-}
{- |
This is a fusion friendly implementation of delay.
However, in order to be a 'crochetL'
the output has the same length as the input,
that is, the last element is removed - at least for finite input.
-}
delay1 :: a -> T a -> T a
delay1 = crochetL (flip (curry Just))

{-# INLINE delay #-}
delay :: y -> Int -> T y -> T y
delay z n = append (replicate n z)

{-# INLINE take #-}
take :: Int -> T a -> T a
take n =
   map snd . takeWhile ((>0) . fst) . zip (iterate pred n)
   -- crochetL (\x n -> toMaybe (n>zero) (x, pred n))

{-# INLINE takeWhile #-}
takeWhile :: (a -> Bool) -> T a -> T a
takeWhile p = crochetL (\x _ -> toMaybe (p x) (x, ())) ()

{-# INLINE replicate #-}
replicate :: Int -> a -> T a
replicate n = take n . repeat


{- * functions consuming multiple lists -}

{-# INLINE zipWith #-}
zipWith :: (a -> b -> c) -> (T a -> T b -> T c)
zipWith h (Cons f a) =
   crochetL
      (\x0 a0 ->
          do (y0,a1) <- runStateT f a0
             Just (h y0 x0, a1))
      a

{-# INLINE zipWithStorable #-}
zipWithStorable :: (Storable b, Storable c) =>
   (a -> b -> c) -> (T a -> SigSt.T b -> SigSt.T c)
zipWithStorable h (Cons f a) =
   SigSt.crochetL
      (\x0 a0 ->
          do (y0,a1) <- runStateT f a0
             Just (h y0 x0, a1))
      a

{-# INLINE zipWith3 #-}
zipWith3 :: (a -> b -> c -> d) -> (T a -> T b -> T c -> T d)
zipWith3 f s0 s1 =
   zipWith (uncurry f) (zip s0 s1)

{-# INLINE zipWith4 #-}
zipWith4 :: (a -> b -> c -> d -> e) -> (T a -> T b -> T c -> T d -> T e)
zipWith4 f s0 s1 =
   zipWith3 (uncurry f) (zip s0 s1)


{-# INLINE zip #-}
zip :: T a -> T b -> T (a,b)
zip = zipWith (,)

{-# INLINE zip3 #-}
zip3 :: T a -> T b -> T c -> T (a,b,c)
zip3 = zipWith3 (,,)

{-# INLINE zip4 #-}
zip4 :: T a -> T b -> T c -> T d -> T (a,b,c,d)
zip4 = zipWith4 (,,,)


{- * functions based on 'foldL' -}

{-# INLINE foldL' #-}
foldL' :: (x -> acc -> acc) -> acc -> T x -> acc
foldL' g b0 sig =
   runSwitchL sig (\next s0 ->
      let recurse b s =
             seq b (next b (\x -> recurse (g x b)) s)
      in  recurse b0 s0)
{-
foldL' g b =
   seq b . switchL b (\ x xs -> foldL' g (g x b) xs)
-}

{-# INLINE foldL #-}
foldL :: (acc -> x -> acc) -> acc -> T x -> acc
foldL f = foldL' (flip f)

{-# INLINE foldL1 #-}
foldL1 :: (x -> x -> x) -> T x -> x
foldL1 f =
   switchL
      (error "State.Signal.foldL1: empty signal")
      (foldL f)

{-# INLINE length #-}
length :: T a -> Int
length = foldL' (const succ) zero

{-# INLINE equal #-}
equal :: (Eq a) => T a -> T a -> Bool
equal xs ys =
   runViewL xs (\nextX sx ->
   runViewL ys (\nextY sy ->
      let go px py =
             case (nextX px, nextY py) of
                (Nothing, Nothing) -> True
                (Just (x,xr), Just (y,yr)) ->
                   x==y && go xr yr
                _ -> False
      in  go sx sy
   ))


{- * functions based on 'foldR' -}

foldR :: (x -> acc -> acc) -> acc -> T x -> acc
foldR g b sig =
   runSwitchL sig (\next s0 ->
      let recurse =
             next b (\ x xs -> g x (recurse xs))
      in  recurse s0)
{-
foldR g b =
   switchL b (\ x xs -> g x (foldR g b xs))
-}


{- * Other functions -}

{-# INLINE null #-}
null :: T a -> Bool
null =
   switchL True (const (const False))
   -- foldR (const (const False)) True

{-# INLINE empty #-}
empty :: T a
empty = generate (const Nothing) ()

{-# INLINE singleton #-}
singleton :: a -> T a
singleton =
   generate (fmap (\x -> (x, Nothing))) . Just

{-# INLINE cons #-}
{- |
This is expensive and should not be used to construct lists iteratively!
-}
cons :: a -> T a -> T a
cons x xs =
   generate
      (\(mx0,xs0) ->
          fmap (mapSnd ((,) Nothing)) $
          maybe
             (viewL xs0)
             (\x0 -> Just (x0, xs0))
             mx0) $
   (Just x, xs)

{-# INLINE viewL #-}
viewL :: T a -> Maybe (a, T a)
viewL (Cons f a0) =
   fmap
      (mapSnd (Cons f))
      (runStateT f a0)

{- iterated 'cons' is very inefficient
viewR :: T a -> Maybe (T a, a)
viewR =
   foldR (\x mxs -> Just (maybe (empty,x) (mapFst (cons x)) mxs)) Nothing
-}

{-# INLINE viewR #-}
viewR :: Storable a => T a -> Maybe (T a, a)
viewR = viewRSize SigSt.defaultChunkSize

{-# INLINE viewRSize #-}
viewRSize :: Storable a => SigSt.ChunkSize -> T a -> Maybe (T a, a)
viewRSize size =
   fmap (mapFst fromStorableSignal) .
   SigSt.viewR .
   toStorableSignal size


{-# INLINE switchL #-}
switchL :: b -> (a -> T a -> b) -> T a -> b
switchL n j =
   maybe n (uncurry j) . viewL

{-# INLINE switchR #-}
switchR :: Storable a => b -> (T a -> a -> b) -> T a -> b
switchR n j =
   maybe n (uncurry j) . viewR


{- |
This implementation requires
that the input generator has to check repeatedly whether it is finished.
-}
{-# INLINE extendConstant #-}
extendConstant :: T a -> T a
extendConstant sig =
   runSwitchL sig (\switch s0 ->
   switch
      empty
      (\ x0 _ ->
          generate
             (\xt1@(x1,s1) ->
                 Just $ switch
                    (x1,xt1)
                    (\x s2 -> (x, (x,s2)))
                    s1)
             (x0,s0)) $
      s0)


{-
{-# INLINE tail #-}
tail :: T a -> T a
tail = Cons . List.tail . decons

{-# INLINE head #-}
head :: T a -> a
head = List.head . decons
-}

{-# INLINE drop #-}
drop :: Int -> T a -> T a
drop n =
   fromMaybe empty .
   nest n (fmap snd . viewL =<<) .
   Just

{-# INLINE dropMarginRem #-}
{- |
This implementation expects that looking ahead is cheap.
-}
dropMarginRem :: Int -> Int -> T a -> (Int, T a)
dropMarginRem n m =
   switchL (error $ "StateSignal.dropMaringRem: length xs < " ++ show n) const .
   dropMargin (succ n) m .
   zipWithTails1 (,) (iterate (max 0 . pred) m)

{-# INLINE dropMargin #-}
dropMargin :: Int -> Int -> T a -> T a
dropMargin n m xs =
   dropMatch (take m (drop n xs)) xs


dropMatch :: T b -> T a -> T a
dropMatch xs ys =
   fromMaybe ys $
   liftM2 dropMatch
      (fmap snd $ viewL xs)
      (fmap snd $ viewL ys)


index :: Int -> T a -> a
index n =
   switchL (error $ "State.Signal: index " ++ show n ++ " too large") const . drop n


{-
splitAt :: Int -> T a -> (T a, T a)
splitAt n = mapPair (Cons, Cons) . List.splitAt n . decons
-}

{-# INLINE splitAt #-}
splitAt :: Storable a =>
   Int -> T a -> (T a, T a)
splitAt = splitAtSize SigSt.defaultChunkSize

{-# INLINE splitAtSize #-}
splitAtSize :: Storable a =>
   SigSt.ChunkSize -> Int -> T a -> (T a, T a)
splitAtSize size n =
   mapPair (fromStorableSignal, fromStorableSignal) .
   SigSt.splitAt n .
   toStorableSignal size


{-# INLINE dropWhile #-}
dropWhile :: (a -> Bool) -> T a -> T a
dropWhile p (Cons f s0) =
   let recurse s =
          maybe empty (\(x,s1) -> if' (p x) (recurse s1) (Cons f s)) $
          runStateT f s
   in  recurse s0
{-
dropWhile p xt =
   switchL empty (\ x xs -> if p x then dropWhile p xs else xt) xt
-}

{-
span :: (a -> Bool) -> T a -> (T a, T a)
span p = mapPair (Cons, Cons) . List.span p . decons
-}

{-# INLINE span #-}
span :: Storable a =>
   (a -> Bool) -> T a -> (T a, T a)
span = spanSize SigSt.defaultChunkSize

{-# INLINE spanSize #-}
spanSize :: Storable a =>
   SigSt.ChunkSize -> (a -> Bool) -> T a -> (T a, T a)
spanSize size p =
   mapPair (fromStorableSignal, fromStorableSignal) .
   SigSt.span p .
   toStorableSignal size


{-# INLINE cycle #-}
cycle :: T a -> T a
cycle sig =
   runViewL sig
      (\next s ->
          maybe
             (error "StateSignal.cycle: empty input")
             (\yt -> generate (Just . fromMaybe yt . next) s) $
             next s)
{-
cycle xs =
   maybe
      (error "StateSignal.cycle: empty input")
      (\yt -> generate (Just . fromMaybe yt . viewL) xs) $
      viewL xs
-}


{-# SPECIALISE INLINE mix :: T Float -> T Float -> T Float #-}
{-# SPECIALISE INLINE mix :: T Double -> T Double -> T Double #-}
{-# INLINE mix #-}
mix :: Additive.C a => T a -> T a -> T a
mix = zipWithAppend (Additive.+)


{-# INLINE sub #-}
sub :: Additive.C a => T a -> T a -> T a
sub xs ys =  mix xs (neg ys)

{-# INLINE neg #-}
neg :: Additive.C a => T a -> T a
neg = map Additive.negate

instance Additive.C y => Additive.C (T y) where
   zero = empty
   (+) = mix
   (-) = sub
   negate = neg

instance Module.C y yv => Module.C y (T yv) where
   (*>) x y = map (x*>) y


infixr 5 `append`

{-# INLINE append #-}
append :: T a -> T a -> T a
append xs ys =
   generate
      (\(b,xys) ->
          mplus
             (fmap (mapSnd ((,) b)) $ viewL xys)
             (if' b Nothing
                (fmap (mapSnd ((,) True)) $ viewL ys)))
      (False,xs)

{-# INLINE appendStored #-}
appendStored :: Storable a =>
   T a -> T a -> T a
appendStored = appendStoredSize SigSt.defaultChunkSize

{-# INLINE appendStoredSize #-}
appendStoredSize :: Storable a =>
   SigSt.ChunkSize -> T a -> T a -> T a
appendStoredSize size xs ys =
   fromStorableSignal $
   SigSt.append
      (toStorableSignal size xs)
      (toStorableSignal size ys)

{-# INLINE concat #-}
-- | certainly inefficient because of frequent list deconstruction
concat :: [T a] -> T a
concat =
   generate
      (msum .
       List.map
          (\ x -> ListHT.viewL x >>=
           \(y,ys) -> viewL y >>=
           \(z,zs) -> Just (z,zs:ys)) .
       List.init . List.tails)


{-# INLINE concatStored #-}
concatStored :: Storable a =>
   [T a] -> T a
concatStored = concatStoredSize SigSt.defaultChunkSize

{-# INLINE concatStoredSize #-}
concatStoredSize :: Storable a =>
   SigSt.ChunkSize -> [T a] -> T a
concatStoredSize size =
   fromStorableSignal .
   SigSt.concat .
   List.map (toStorableSignal size)

{-
This should be faster than Monad.ap
if an empty signal as second operand is detected.
In this case an empty signal is returned without running a loop.
-}
liftA2 :: (a -> b -> c) -> (T a -> T b -> T c)
liftA2 p x y =
   runViewL x $ \f s0 ->
   runViewL y $ \g t0 ->
   flip generate (App.liftA2 (,) (f s0) (g t0)) $ \m ->
   flip fmap m $ \(as@(a,s), (b,t)) ->
   (p a b,
    fmap ((,) as) (g t) `mplus`
    App.liftA2 (,) (f s) (g t0))

{-# INLINE reverse #-}
reverse ::
   T a -> T a
reverse =
   fromList . List.reverse . toList

{-# INLINE reverseStored #-}
reverseStored :: Storable a =>
   T a -> T a
reverseStored = reverseStoredSize SigSt.defaultChunkSize

{-# INLINE reverseStoredSize #-}
reverseStoredSize :: Storable a =>
   SigSt.ChunkSize -> T a -> T a
reverseStoredSize size =
   fromStorableSignal .
   SigSt.reverse .
   toStorableSignal size


{-# INLINE sum #-}
sum :: (Additive.C a) => T a -> a
sum = foldL' (Additive.+) Additive.zero

{-# INLINE maximum #-}
maximum :: (Ord a) => T a -> a
maximum =
   switchL
      (error "StateSignal.maximum: empty list")
      (foldL' max)

{-
{-# INLINE tails #-}
tails :: T y -> [T y]
tails = List.map Cons . List.tails . decons
-}

{-# INLINE init #-}
init :: T y -> T y
init =
   switchL
      (error "StateSignal.init: empty list")
      (crochetL (\x acc -> Just (acc,x)))

{-# INLINE sliceVert #-}
-- inefficient since it computes some things twice
sliceVert :: Int -> T y -> [T y]
sliceVert n =
--   map fromList . Sig.sliceVert n . toList
   List.map (take n) . List.takeWhile (not . null) . List.iterate (drop n)

{-# DEPRECATED zapWith, zapWithAlt "use mapAdjacent" #-}
{-# INLINE zapWith #-}
zapWith :: (a -> a -> b) -> T a -> T b
zapWith = mapAdjacent

zapWithAlt :: (a -> a -> b) -> T a -> T b
zapWithAlt f xs =
   zipWith f xs (switchL empty (curry snd) xs)

{-# INLINE mapAdjacent #-}
mapAdjacent :: (a -> a -> b) -> T a -> T b
mapAdjacent f =
   switchL empty
      (crochetL (\y x -> Just (f x y, y)))

{-# INLINE modifyStatic #-}
modifyStatic :: Modifier.Simple s ctrl a b -> ctrl -> T a -> T b
modifyStatic modif control x =
   crochetL
      (\a acc ->
         Just (runState (Modifier.step modif control a) acc))
      (Modifier.init modif) x

{-| Here the control may vary over the time. -}
{-# INLINE modifyModulated #-}
modifyModulated :: Modifier.Simple s ctrl a b -> T ctrl -> T a -> T b
modifyModulated modif control x =
   crochetL
      (\ca acc ->
         Just (runState (uncurry (Modifier.step modif) ca) acc))
      (Modifier.init modif)
      (zip control x)


-- cf. Module.linearComb
{-# INLINE linearComb #-}
linearComb ::
   (Module.C t y) =>
   T t -> T y -> y
linearComb ts ys =
   sum $ zipWith (*>) ts ys


-- comonadic 'bind'
-- only non-empty suffixes are processed
{-# INLINE mapTails #-}
mapTails ::
   (T y0 -> y1) -> T y0 -> T y1
mapTails f =
   generate (\xs ->
      do (_,ys) <- viewL xs
         return (f xs, ys))
{-
mapTails f xs0 =
   runViewL xs0 (\next ->
      generate (\xs ->
         do (_,ys) <- next xs
            return (f xs, ys)))
-}

-- | only non-empty suffixes are processed
{-# INLINE zipWithTails #-}
zipWithTails ::
   (y0 -> T y1 -> y2) -> T y0 -> T y1 -> T y2
zipWithTails f =
   curry $ generate (\(xs0,ys0) ->
      do (x,xs) <- viewL xs0
         (_,ys) <- viewL ys0
         return (f x ys0, (xs,ys)))
{-
zipWithTails f xs1 ys1 =
   runViewL xs1 (\nextX xs2 ->
   runViewL ys1 (\nextY ys2 ->
      generate (\(xs0,ys0) ->
         do (x,xs) <- nextX xs0
            (_,ys) <- nextY ys0
            return (f x ys0, (xs,ys)))
         (xs2,ys2)))
-}

-- | in contrast to 'zipWithTails' it also generates the empty suffix (once)
{-# INLINE zipWithTails1 #-}
zipWithTails1 ::
   (y0 -> T y1 -> y2) -> T y0 -> T y1 -> T y2
zipWithTails1 f xs ys =
   generate (\(xs0,ys0) ->
      do (x,xs1) <- viewL xs0
         ys1 <- ys0
         return (f x ys1, (xs1, fmap snd $ viewL ys1)))
      (xs, Just ys)

-- | in contrast to 'zipWithTails' it appends infinitely many empty suffixes
{-# INLINE zipWithTailsInf #-}
zipWithTailsInf ::
   (y0 -> T y1 -> y2) -> T y0 -> T y1 -> T y2
zipWithTailsInf f =
   curry $ generate (\(xs0,ys0) ->
      do (x,xs) <- viewL xs0
         return (f x ys0, (xs, switchL empty (flip const) ys0)))

{-
This can hardly be implemented in an efficient way.
But this means, we cannot implement the Generic.Transform class.

zipWithRest ::
   (y0 -> y0 -> y1) ->
   T y0 -> T y0 ->
   (T y1, (Bool, T y0))
zipWithRest f =
   curry $ generate (\(xs0,ys0) ->
      do (x,xs) <- viewL xs0
         (y,ys) <- viewL ys0
         return (f x y, (xs,ys)))
-}


{-# INLINE zipWithAppend #-}
zipWithAppend ::
   (y -> y -> y) ->
   T y -> T y -> T y
zipWithAppend f xs ys =
   runViewL xs (\nextX sx ->
   runViewL ys (\nextY sy ->
      unfoldR (zipStep nextX nextY f) (sx,sy)
   ))

{-# INLINE zipStep #-}
zipStep ::
   (s -> Maybe (a,s)) ->
   (t -> Maybe (a,t)) ->
   (a -> a -> a) -> (s, t) -> Maybe (a, (s, t))
zipStep nextX nextY f (xt,yt) =
   case (nextX xt, nextY yt) of
      (Just (x,xs), Just (y,ys)) -> Just (f x y, (xs,ys))
      (Nothing,     Just (y,ys)) -> Just (y,     (xt,ys))
      (Just (x,xs), Nothing)     -> Just (x,     (xs,yt))
      (Nothing,     Nothing)     -> Nothing



delayLoop ::
      (T y -> T y)
            -- ^ processor that shall be run in a feedback loop
   -> T y   -- ^ prefix of the output, its length determines the delay
   -> T y
delayLoop proc prefix =
   -- the temporary list is need for sharing the output
   let ys = fromList (toList prefix List.++ toList (proc ys))
   in  ys

delayLoopOverlap ::
   (Additive.C y) =>
      Int
   -> (T y -> T y)
            -- ^ processor that shall be run in a feedback loop
   -> T y   -- ^ input
   -> T y   -- ^ output has the same length as the input
delayLoopOverlap time proc xs =
   -- the temporary list is need for sharing the output
   let ys = zipWith (Additive.+) xs (delay zero time (proc (fromList (toList ys))))
   in  ys


{-
A traversable instance is hardly useful,
because 'cons' is so expensive.

instance Traversable T where
-}
{-# INLINE sequence_ #-}
sequence_ :: Monad m => T (m a) -> m ()
sequence_ =
   switchL (return ()) (\x xs -> x >> sequence_ xs)

{-# INLINE mapM_ #-}
mapM_ :: Monad m => (a -> m ()) -> T a -> m ()
mapM_ f = sequence_ . map f


{- |
Counterpart to 'Data.Monoid.mconcat'.
-}
fold :: Monoid m => T m -> m
fold = foldR mappend mempty

{-# DEPRECATED monoidConcat "Use foldMap instead." #-}
monoidConcat :: Monoid m => T m -> m
monoidConcat = fold


foldMap :: Monoid m => (a -> m) -> T a -> m
foldMap f = monoidConcat . map f

{-# DEPRECATED monoidConcatMap "Use foldMap instead." #-}
monoidConcatMap :: Monoid m => (a -> m) -> T a -> m
monoidConcatMap = foldMap


instance Semigroup (T y) where
   (<>) = append

instance Monoid (T y) where
   mempty = empty
   mappend = (<>)


catMaybes :: T (Maybe a) -> T a
catMaybes sig =
   runViewL sig (\next ->
   generate (
      let go s0 =
             next s0 >>= \(ma,s1) ->
             fmap (flip (,) s1) ma `mplus`
             go s1
      in  go))

flattenPairs :: T (a,a) -> T a
flattenPairs sig =
   runViewL sig (\next t ->
   generate
      (\(carry,s0) ->
         fmap (\b -> (b, (Nothing, s0))) carry `mplus`
         fmap (\((a,b),s1) -> (a, (Just b, s1))) (next s0))
      (Nothing,t))

interleave, interleaveAlt ::
   T y -> T y -> T y
interleave xs ys =
   runViewL xs (\nextX sx ->
   runViewL ys (\nextY sy ->
   unfoldR
      (\(select,(sx0,sy0)) ->
         case select of
            False -> fmap (mapSnd (\sx1 -> (True,  (sx1,sy0)))) $ nextX sx0
            True  -> fmap (mapSnd (\sy1 -> (False, (sx0,sy1)))) $ nextY sy0)
      (False, (sx,sy))))

interleaveAlt xs ys = flattenPairs $ zip xs ys
