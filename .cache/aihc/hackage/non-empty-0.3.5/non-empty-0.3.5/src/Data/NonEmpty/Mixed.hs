{- |
Functions that cope both with plain and non-empty structures.

If there are two versions of a function,
where one works on fixed-length lists,
then place the fixed-length list variant in NonEmpty
and the other one here.
-}
module Data.NonEmpty.Mixed where

import qualified Data.NonEmpty.Foldable as FoldU
import qualified Data.NonEmpty.Class as C
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import qualified Data.List.HT as ListHT
import Data.Traversable (Traversable, mapAccumL, sequenceA, )
import Data.Foldable (Foldable, foldr, )
import Data.Tuple.HT (mapFst, mapSnd, )
import Data.Eq.HT (equating, )

import Prelude hiding (splitAt, take, foldr, scanl, scanr, )


groupBy ::
   (Foldable f) =>
   (a -> a -> Bool) -> f a -> [NonEmpty.T [] a]
groupBy p =
   foldr
      (\x0 yt ->
         let (xr,yr) =
               case yt of
                  NonEmpty.Cons x1 xs : ys ->
                     if p x0 x1
                       then (x1:xs,ys)
                       else ([],yt)
                  [] -> ([],yt)
         in  NonEmpty.Cons x0 xr : yr)
      []

groupPairs :: (Foldable f, Eq a) => f (a,b) -> [(a, NonEmpty.T [] b)]
groupPairs =
   map (\xs -> (fst $ NonEmpty.head xs, fmap snd xs)) .
   groupBy (equating fst)

groupKey :: (Foldable f, Eq a) => (b -> a) -> f b -> [(a, NonEmpty.T [] b)]
groupKey f = groupPairs . FoldU.Mapped (\b -> (f b, b))

groupEithers ::
   (Foldable f) =>
   f (Either a b) -> [Either (NonEmpty.T [] a) (NonEmpty.T [] b)]
groupEithers =
   foldr
      (\x xs ->
         case x of
            Left a ->
               uncurry (:) $ mapFst (Left . NonEmpty.Cons a) $
               case xs of
                  Left as : ys -> (NonEmpty.flatten as, ys)
                  ys -> ([], ys)
            Right b ->
               uncurry (:) $ mapFst (Right . NonEmpty.Cons b) $
               case xs of
                  Right bs : ys -> (NonEmpty.flatten bs, ys)
                  ys -> ([], ys))
      []


segmentAfter ::
   (Foldable f) =>
   (a -> Bool) -> f a -> ([NonEmpty.T [] a], [a])
segmentAfter p =
   foldr
      (\x ~(ys,zs) ->
         if p x
           then (NonEmpty.singleton x : ys, zs)
           else
              case ys of
                 [] -> (ys, x:zs)
                 w:ws -> (C.cons x w : ws, zs))
      ([],[])

segmentBefore ::
   (Foldable f) =>
   (a -> Bool) -> f a -> ([a], [NonEmpty.T [] a])
segmentBefore p =
   foldr
      (\ x ys ->
         if p x
           then ([], NonEmpty.Cons x (fst ys) : snd ys)
           else (x : fst ys, snd ys))
      ([],[])

filterToInfixes ::
   (Foldable f) =>
   (a -> Bool) -> f a -> [NonEmpty.T [] a]
filterToInfixes p =
   let cons = uncurry $ maybe id (:) . NonEmpty.fetch
   in  cons .
       foldr
          (\x yzs ->
             if p x
               then mapFst (x:) yzs
               else ([], cons yzs))
          ([], [])

mapAdjacent ::
   (C.Cons f, C.Zip f) => (a -> a -> b) -> NonEmpty.T f a -> f b
mapAdjacent f xs =
   C.zipWith f (NonEmpty.flatten xs) (NonEmpty.tail xs)


take ::
   (C.View g, C.Repeat f, Traversable f) =>
   g a -> Maybe (f a)
take = fst . splitAt

splitAt ::
   (C.View g, C.Repeat f, Traversable f) =>
   g a -> (Maybe (f a), g a)
splitAt xs0 =
   (\(xs1, mys) -> (mys, maybe xs0 (const xs1) mys)) $
   mapSnd sequenceA $
   mapAccumL
      (\xt () ->
         case C.viewL xt of
            Nothing -> (xt, Nothing)
            Just (x,xs) -> (xs, Just x))
      xs0 (C.repeat ())

sliceVertical ::
   (C.View g, C.Repeat f, Traversable f) =>
   g a -> ([f a], g a)
sliceVertical x0 =
   case splitAt x0 of
      (my,x1) ->
         case my of
            Nothing -> ([], x1)
            Just y -> mapFst (y:) $ sliceVertical x1



{- |
This implementation is more efficient for Sequence than 'NonEmpty.viewR'.
-}
viewR :: (C.ViewR f, C.Empty f, C.Cons f) => NonEmpty.T f a -> (f a, a)
viewR (NonEmpty.Cons x xs) =
   case C.viewR xs of
      Nothing -> (C.empty, x)
      Just (ys, y) -> (C.cons x ys, y)

init :: (C.ViewR f, C.Empty f, C.Cons f) => NonEmpty.T f a -> f a
init = fst . viewR

last :: (C.ViewR f) => NonEmpty.T f a -> a
last (NonEmpty.Cons x xs) =
   case C.viewR xs of
      Nothing -> x
      Just (_, y) -> y


tails ::
   (C.ViewL f, C.Empty f) =>
   f a -> NonEmpty.T [] (f a)
tails xt =
   NonEmpty.force $
   case C.viewL xt of
      Nothing -> NonEmpty.Cons C.empty []
      Just (_, xs) -> C.cons xt $ tails xs

inits ::
   (C.ViewL f, C.Cons f, C.Empty f) =>
   f a -> NonEmpty.T [] (f a)
inits xt =
   NonEmpty.Cons C.empty $
   case C.viewL xt of
      Nothing -> []
      Just (x,xs) -> map (C.cons x) $ NonEmpty.flatten $ inits xs


appendLeft :: (C.Cons f) => [a] -> f a -> f a
appendLeft = flip $ foldr C.cons


iterate :: (C.Repeat f, Traversable f) => (a -> a) -> a -> f a
iterate f x0 =
   snd $ mapAccumL (\xi fi -> (fi xi, xi)) x0 $ C.repeat f


class Choose f where
   {- |
   Select tuples of list elements:
   @choose "abc" == ['a'!:'b'!:empty,'a'!:'c'!:empty,'b'!:'c'!:empty]@
   -}
   choose :: [a] -> [f a]

instance Choose Empty.T where
   choose _ = [Empty.Cons]

instance (Choose f) => Choose (NonEmpty.T f) where
   choose xs = do
      (y:ys) <- ListHT.tails xs
      map (NonEmpty.cons y) $ choose ys

instance Choose [] where
   choose [] = [[]]
   choose (x:xs) =
      let ys = choose xs
      in  map (x:) ys ++ ys
