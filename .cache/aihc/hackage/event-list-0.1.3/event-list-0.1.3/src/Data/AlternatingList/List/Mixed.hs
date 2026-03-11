{- |
Copyright   :  (c) Henning Thielemann 2007-2009

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

Functions that combine both data types,
'Data.AlternatingList.List.Disparate.T' and
'Data.AlternatingList.List.Uniform.T'
-}
module Data.AlternatingList.List.Mixed (
    consFirst, consSecond, (./), (/.),
    snocFirst, snocSecond,
    viewL, viewFirstL, viewSecondL,
    viewR, viewFirstR, viewSecondR,
    switchL, switchFirstL, switchSecondL,
    switchR, switchFirstR, switchSecondR,
    mapFirstL,  mapFirstHead,  mapFirstTail,
    mapSecondL, mapSecondHead, mapSecondTail,
    mapFirstR,  mapFirstLast,  mapFirstInit,
    mapSecondR, mapSecondLast, mapSecondInit,
    appendUniformUniform, appendDisparateUniform, appendUniformDisparate,
    concatUniform, concatDisparate,
    reverseUniform, reverseDisparate,
    splitAtDisparateUniform, splitAtUniformDisparate, splitAtUniformUniform,
    takeDisparate, takeUniform, dropDisparate, dropUniform,
    {- spanFirst, spanSecond, spanDisparate, -}
   ) where


import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform

import Data.AlternatingList.List.Uniform (mapSecondHead)

import qualified Control.Monad as Monad

import Data.Tuple.HT (mapFst, mapSnd, mapPair, )

import Prelude hiding
   (null, foldr, map, concat, sequence, sequence_, )


infixr 5 ./, /.

(/.) :: a -> Uniform.T a b -> Disp.T a b
(/.) = consFirst

(./) :: b -> Disp.T a b -> Uniform.T a b
(./) = consSecond


consFirst :: a -> Uniform.T a b -> Disp.T a b
consFirst a ~(Uniform.Cons b xs) = Disp.cons a b xs

consSecond :: b -> Disp.T a b -> Uniform.T a b
consSecond = Uniform.Cons


snocFirst :: Uniform.T a b -> a -> Disp.T b a
snocFirst xs = appendUniformUniform xs . Uniform.singleton
-- snocFirst xs a = Uniform.foldr consSecond consFirst (Uniform.singleton a) xs

snocSecond :: Disp.T b a -> b -> Uniform.T a b
snocSecond xs = appendDisparateUniform xs . Uniform.singleton
-- snocSecond xs b = Disp.foldr consSecond consFirst (Uniform.singleton b) xs


viewL :: Uniform.T a b -> (b, Maybe (a, Uniform.T a b))
viewL = mapSnd viewFirstL . viewSecondL

viewFirstL :: Disp.T a b -> Maybe (a, Uniform.T a b)
viewFirstL =
   Monad.liftM (\((a,b), xs) -> (a, consSecond b xs)) . Disp.viewL

viewSecondL :: Uniform.T a b -> (b, Disp.T a b)
viewSecondL (Uniform.Cons b xs) = (b,xs)


viewR :: Uniform.T a b -> (Maybe (Uniform.T a b, a), b)
viewR (Uniform.Cons b0 xs0) =
   Disp.switchR
      (Nothing, b0)
      (\ xs a b -> (Just (consSecond b0 xs, a), b))
      xs0

viewFirstR :: Disp.T b a -> Maybe (Uniform.T a b, a)
viewFirstR =
   Monad.liftM (\ (xs, ~(a,b)) -> (snocSecond xs a, b)) .
   Disp.viewR

{-
TODO:
Must be more lazy in case of
@viewSecondR (2 /. 'a' ./ 3 /. 'b' ./ 4 /. undefined)@.
It must also return the @'b'@ but it does not.
-}
viewSecondR :: Uniform.T a b -> (Disp.T b a, b)
viewSecondR (Uniform.Cons b0 xs0) =
   Disp.switchR
      (Disp.empty, b0)
      (\ xs a b -> (consFirst b0 (snocSecond xs a), b))
      xs0


{-# INLINE switchL #-}
switchL :: (b -> c) -> (b -> a -> Uniform.T a b -> c) -> Uniform.T a b -> c
switchL f g =
   switchSecondL (\x -> switchFirstL (f x) (g x))

{-# INLINE switchFirstL #-}
switchFirstL :: c -> (a -> Uniform.T a b -> c) -> Disp.T a b -> c
switchFirstL f g =
   Disp.switchL f (\ a b xs -> g a (consSecond b xs))

{-# INLINE switchSecondL #-}
switchSecondL :: (b -> Disp.T a b -> c) -> Uniform.T a b -> c
switchSecondL f (Uniform.Cons b xs) = f b xs
{-
The lazy pattern match leads to a space leak in synthesizer-alsa:testArrangeSpaceLeak
I would like to reproduce this in a small test,
but I did not achieve this so far.
-}
-- switchSecondL f ~(Uniform.Cons b xs) = f b xs


{-# INLINE switchR #-}
switchR :: (b -> c) -> (Uniform.T a b -> a -> b -> c) -> Uniform.T a b -> c
switchR f g =
   switchSecondR (\xs b -> switchFirstR (f b) (\ys a -> g ys a b) xs)

{-# INLINE switchFirstR #-}
switchFirstR :: c -> (Uniform.T a b -> a -> c) -> Disp.T b a -> c
switchFirstR f g =
   maybe f (uncurry g) . viewFirstR

{-# INLINE switchSecondR #-}
switchSecondR :: (Disp.T b a -> b -> c) -> Uniform.T a b -> c
switchSecondR f = uncurry f . viewSecondR


-- could also be in ListDisparate
mapFirstL ::
   (a -> a, Uniform.T a b0 -> Uniform.T a b1) ->
   Disp.T a b0 -> Disp.T a b1
mapFirstL f =
   maybe Disp.empty (uncurry consFirst . mapPair f) . viewFirstL

mapFirstHead ::
   (a -> a) ->
   Disp.T a b -> Disp.T a b
mapFirstHead f = mapFirstL (f,id)

mapFirstTail ::
   (Uniform.T a b0 -> Uniform.T a b1) ->
   Disp.T a b0 -> Disp.T a b1
mapFirstTail f = mapFirstL (id,f)


mapSecondL ::
   (b -> b, Disp.T a0 b -> Disp.T a1 b) ->
   Uniform.T a0 b -> Uniform.T a1 b
mapSecondL f = uncurry consSecond . mapPair f . viewSecondL

{-
mapSecondHead ::
   (b -> b) ->
   Uniform.T a b -> Uniform.T a b
mapSecondHead f = mapSecondL (f,id)
-}

mapSecondTail ::
   (Disp.T a0 b -> Disp.T a1 b) ->
   Uniform.T a0 b -> Uniform.T a1 b
mapSecondTail f = mapSecondL (id,f)


mapFirstR ::
   (Uniform.T a b0 -> Uniform.T a b1, a -> a) ->
   Disp.T b0 a -> Disp.T b1 a
mapFirstR f =
   maybe Disp.empty (uncurry snocFirst . mapPair f) . viewFirstR

-- could also be in ListDisparate
mapFirstLast ::
   (a -> a) ->
   Disp.T b a -> Disp.T b a
mapFirstLast f = mapFirstR (id,f)

mapFirstInit ::
   (Uniform.T a b0 -> Uniform.T a b1) ->
   Disp.T b0 a -> Disp.T b1 a
mapFirstInit f = mapFirstR (f,id)


mapSecondR ::
   (Disp.T b a0 -> Disp.T b a1, b -> b) ->
   Uniform.T a0 b -> Uniform.T a1 b
mapSecondR f = uncurry snocSecond . mapPair f . viewSecondR

mapSecondLast ::
   (b -> b) ->
   Uniform.T a b -> Uniform.T a b
mapSecondLast f = mapSecondR (id,f)

mapSecondInit ::
   (Disp.T b a0 -> Disp.T b a1) ->
   Uniform.T a0 b -> Uniform.T a1 b
mapSecondInit f = mapSecondR (f,id)



reverseUniform :: Uniform.T a b -> Uniform.T a b
reverseUniform =
   Uniform.foldl (flip consFirst) (flip consSecond) Disp.empty

reverseDisparate :: Disp.T a b -> Disp.T b a
reverseDisparate =
   Disp.foldl (flip consSecond) (flip consFirst) Disp.empty


appendUniformUniform :: Uniform.T a b -> Uniform.T b a -> Disp.T b a
appendUniformUniform xs ys =
   Uniform.foldr consSecond consFirst ys xs

appendDisparateUniform :: Disp.T b a -> Uniform.T a b -> Uniform.T a b
appendDisparateUniform xs ys =
   Disp.foldr consSecond consFirst ys xs

appendUniformDisparate :: Uniform.T a b -> Disp.T a b -> Uniform.T a b
appendUniformDisparate xs ys =
   mapSecondTail (flip Disp.append ys) xs


concatDisparate :: Disp.T (Uniform.T b a) (Uniform.T a b) -> Disp.T a b
concatDisparate =
   Disp.foldr appendUniformUniform appendUniformDisparate Disp.empty

concatUniform :: Uniform.T (Uniform.T b a) (Uniform.T a b) -> Uniform.T a b
concatUniform =
   switchSecondL
   (\ b xs -> appendUniformDisparate b (concatDisparate xs))



splitAtDisparateUniform :: Int -> Uniform.T a b -> (Disp.T b a, Uniform.T a b)
splitAtDisparateUniform 0 = (,) Disp.empty
splitAtDisparateUniform n =
   (\ ~(prefix,suffix) ->
       maybe
          (error "splitAtDisparateUniform: empty list")
          (mapFst (snocFirst prefix))
          (viewFirstL suffix)) .
   splitAtUniformDisparate (pred n)

splitAtUniformDisparate :: Int -> Uniform.T a b -> (Uniform.T a b, Disp.T a b)
splitAtUniformDisparate n (Uniform.Cons b xs) =
   mapFst (consSecond b) $ Disp.splitAt n xs


splitAtUniformUniform ::
   Int -> Disp.T b a -> Maybe (Uniform.T a b, Uniform.T b a)
splitAtUniformUniform n =
   (\ ~(xs,ys) ->
        fmap
           (mapFst (snocSecond xs))
           (viewFirstL ys)) .
   Disp.splitAt n


takeDisparate :: Int -> Uniform.T a b -> Disp.T b a
takeDisparate n =
   fst . viewSecondR . takeUniform n

takeUniform :: Int -> Uniform.T a b -> Uniform.T a b
takeUniform n (Uniform.Cons b xs) =
   consSecond b $ Disp.take n xs

dropDisparate :: Int -> Uniform.T a b -> Disp.T a b
dropDisparate n = Disp.drop n . snd . viewSecondL

dropUniform :: Int -> Uniform.T a b -> Uniform.T a b
dropUniform 0 = id
dropUniform n =
   switchFirstL (error "dropUniform: empty list") (flip const) .
   dropDisparate (pred n)


{-
breakDisparateFirst :: (a -> Bool) ->
   Disp.T a b -> (Disp.T a b, Disp.T a b)
breakDisparateFirst p = Disp.spanFirst (not . p)

breakUniformFirst :: (a -> Bool) ->
   Uniform.T a b -> (Uniform.T a b, Disp.T a b)
breakUniformFirst p =
   let recourse xs0 =
          (\(b,xs) ->
              if p b
                then (empty, xs0)
                else
                  maybe
                     (\(a,ys) ->)
                  let (as,) = recourse  xs
                  in  ) $
          viewSecondL xs0
-}

{-
spanSecond :: (b -> Bool) -> Uniform.T a b -> (Uniform.T a b, Disp.T b a)
spanSecond p (Uniform.Cons b xs) =
   mapFst (consSecond b) (Disp.span p xs)

spanDisparate :: (b -> Bool) -> Disp.T a b -> (Uniform.T b a, Uniform.T a b)
spanDisparate p =
   mapPair (consSecond, consSecond) . List.span (p . pairFirst) . toPairList
-}
