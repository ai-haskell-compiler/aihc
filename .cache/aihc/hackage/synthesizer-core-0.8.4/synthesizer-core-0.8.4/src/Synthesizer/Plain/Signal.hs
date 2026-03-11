{- |
Copyright   :  (c) Henning Thielemann 2008-2011
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  portable
-}
module Synthesizer.Plain.Signal where

import qualified Number.Peano as Peano

import qualified Synthesizer.Plain.Modifier as Modifier

import qualified Data.List.Match as ListMatch
import qualified Data.List.HT    as ListHT
import qualified Data.List       as List

import Data.Tuple.HT (forcePair, mapFst, mapSnd, )


type T = []


{- * Generic routines that are useful for filters -}

type Modifier s ctrl a b = Modifier.Simple s ctrl a b

{-|
modif is a process controlled by values of type c
with an internal state of type s,
it converts an input value of type a into an output value of type b
while turning into a new state

ToDo:
Shall finite signals be padded with zeros?
-}
modifyStatic ::
   Modifier s ctrl a b -> ctrl -> T a -> T b
modifyStatic = Modifier.static

{-| Here the control may vary over the time. -}
modifyModulated ::
   Modifier s ctrl a b -> T ctrl -> T a -> T b
modifyModulated = Modifier.modulated


type ModifierInit s init ctrl a b = Modifier.Initialized s init ctrl a b


modifierInitialize ::
   ModifierInit s init ctrl a b -> init -> Modifier s ctrl a b
modifierInitialize = Modifier.initialize

modifyStaticInit ::
   ModifierInit s init ctrl a b -> init -> ctrl -> T a -> T b
modifyStaticInit = Modifier.staticInit

{-| Here the control may vary over the time. -}
modifyModulatedInit ::
   ModifierInit s init ctrl a b -> init -> T ctrl -> T a -> T b
modifyModulatedInit = Modifier.modulatedInit



unfoldR :: (acc -> Maybe (y, acc)) -> acc -> (acc, T y)
unfoldR f =
   let recourse acc0 =
          forcePair $
          maybe
             (acc0,[])
             (\(y,acc1) ->
                mapSnd (y:) $ recourse acc1)
             (f acc0)
   in  recourse

reduceL :: (x -> acc -> Maybe acc) -> acc -> T x -> acc
reduceL f =
   let recourse a xt =
          case xt of
             [] -> a
             (x:xs) ->
                maybe a
                   (\ a' -> seq a' (recourse a' xs))
                   (f x a)
   in  recourse

mapAccumL :: (x -> acc -> Maybe (y, acc)) -> acc -> T x -> (acc, T y)
mapAccumL f =
   let recourse acc0 xt =
          forcePair $
          case xt of
             [] -> (acc0,[])
             (x:xs) ->
                 maybe
                    (acc0,[])
                    (\(y,acc1) ->
                       mapSnd (y:) $ recourse acc1 xs)
                    (f x acc0)
   in  recourse

crochetL :: (x -> acc -> Maybe (y, acc)) -> acc -> T x -> T y
crochetL f a = snd . mapAccumL f a


{- |
Feed back signal into signal processor,
and apply a delay by one value.
'fix1' is a kind of 'Signal.generate'.
-}
fix1 :: y -> (T y -> T y) -> T y
fix1 pad f =
   let y = f (pad:y)
   in  y

{-# RULES
  "fix1/crochetL" forall f a b.
     fix1 a (crochetL f b) =
        snd $ unfoldR (\(a0,b0) ->
            do yb1@(y0,_) <- f a0 b0
               return (y0, yb1)) (a,b) ;
  #-}



{-
instance SigG.Data [] y where

instance SigG.C [] where
   add = (Additive.+)
   map = List.map
   zipWith = List.zipWith
-}


{- |
@dropMarginRem n m xs@
drops at most the first @m@ elements of @xs@
and ensures that @xs@ still contains @n@ elements.
Additionally returns the number of elements that could not be dropped
due to the margin constraint.
That is @dropMarginRem n m xs == (k,ys)@ implies @length xs - m == length ys - k@.
Requires @length xs >= n@.
-}
dropMarginRem :: Int -> Int -> T a -> (Int, T a)
dropMarginRem n m =
   head .
   dropMargin (1+n) m .
   zip (iterate (max 0 . pred) m) .
   ListHT.tails

dropMargin :: Int -> Int -> T a -> T a
dropMargin n m xs =
   ListMatch.drop (take m (drop n xs)) xs


{- |
Test whether a list has at least @n@ elements.
-}
lengthAtLeast :: Int -> T a -> Bool
lengthAtLeast n xs =
   n<=0 || not (null (drop (n-1) xs))


{- |
Can be implemented more efficiently
than just by 'zipWith' and 'List.tails'
for other data structures.
-}
zipWithTails ::
   (y0 -> T y1 -> y2) -> T y0 -> T y1 -> T y2
zipWithTails f xs =
   zipWith f xs . init . ListHT.tails

zipWithRest ::
   (y0 -> y0 -> y1) ->
   T y0 -> T y0 ->
   (T y1, (Bool, T y0))
zipWithRest f xs ys =
   let len = min (List.genericLength xs) (List.genericLength ys) :: Peano.T
       (prefixX,suffixX) = List.genericSplitAt len xs
       (prefixY,suffixY) = List.genericSplitAt len ys
       second = null suffixX
   in  (zipWith f prefixX prefixY,
        (second, if second then suffixY else suffixX))

zipWithRestRec ::
   (y0 -> y0 -> y1) ->
   T y0 -> T y0 ->
   (T y1, (Bool, T y0))
zipWithRestRec f =
   let recourse xt yt =
          forcePair $
          case (xt,yt) of
             (x:xs, y:ys) -> mapFst (f x y :) (recourse xs ys)
             ([], _) -> ([], (True,  yt))
             (_, []) -> ([], (False, xt))
   in  recourse
{-
Test.QuickCheck.test (\xs ys -> zipWithRest (,) xs ys == zipWithRest' (,) xs (ys::[Int]))
-}

zipWithAppend ::
   (y -> y -> y) ->
   T y -> T y -> T y
zipWithAppend f xs ys =
   uncurry (++) $ mapSnd snd $ zipWithRest f xs ys
