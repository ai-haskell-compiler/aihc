module Data.List.Key.Private where

import Data.Function.HT (compose2, )

import Data.List (nubBy, sortBy, minimumBy, maximumBy, )

import Prelude hiding (minimum, maximum, )


attach :: (a -> b) -> [a] -> [(b,a)]
attach key = map (\x -> (key x, x))


aux ::
   (((key, a) -> (key, a) -> b) -> [(key, a)] -> c) ->
      (key -> key -> b) -> (a -> key) ->
          ([a] -> c)
aux listFunc cmpFunc key =
   listFunc (compose2 cmpFunc fst) . attach key

aux' ::
   ((a -> a -> b) -> [a] -> c) ->
      (key -> key -> b) -> (a -> key) ->
          ([a] -> c)
aux' listFunc cmpFunc key =
   listFunc (compose2 cmpFunc key)


{- |
Divides a list into sublists such that the members in a sublist
share the same key.
It uses semantics of 'Data.List.HT.groupBy',
not that of 'Data.List.groupBy'.
-}
group :: Eq b => (a -> b) -> [a] -> [[a]]
group key = map (map snd) . aux groupBy (==) key

{- |
Will be less efficient than 'group'
if @key@ is computationally expensive.
This is so because the key is re-evaluated for each list item.
Alternatively you may write @groupBy ((==) `on` key)@.
-}
group' :: Eq b => (a -> b) -> [a] -> [[a]]
group'  =  aux' groupBy (==)

propGroup :: (Eq a, Eq b) => (a -> b) -> [a] -> Bool
propGroup key xs =
   group key xs == group' key xs

{- | argmin -}
minimum :: Ord b => (a -> b) -> [a] -> a
minimum key  =  snd . aux minimumBy compare key

{- | argmax -}
maximum :: Ord b => (a -> b) -> [a] -> a
maximum key  =  snd . aux maximumBy compare key

sort :: Ord b => (a -> b) -> [a] -> [a]
sort key  =  map snd . aux sortBy compare key

merge :: Ord b => (a -> b) -> [a] -> [a] -> [a]
merge key xs ys  =
   map snd $
   mergeBy
      (compose2 (<=) fst)
      (attach key xs) (attach key ys)

nub :: Eq b => (a -> b) -> [a] -> [a]
nub key  =  map snd . aux nubBy (==) key


-- * helper functions

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p = map (uncurry (:)) . groupByNonEmpty p

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [(a,[a])]
groupByNonEmpty p =
   foldr
      (\x0 yt ->
         let (xr,yr) =
               case yt of
                  (x1,xs):ys ->
                     if p x0 x1
                       then (x1:xs,ys)
                       else ([],yt)
                  [] -> ([],yt)
         in  (x0,xr):yr)
      []

groupByEmpty :: (a -> a -> Bool) -> [a] -> [[a]]
groupByEmpty p =
   uncurry (:) .
   foldr
      (\x0 ~(y,ys) ->
         if (case y of x1:_ -> p x0 x1; _ -> True)
           then (x0:y,ys)
           else (x0:[],y:ys))
      ([],[])

mergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy p =
   let recourse [] yl = yl
       recourse xl [] = xl
       recourse xl@(x:xs) yl@(y:ys) =
         uncurry (:) $
         if p x y
           then (x, recourse xs yl)
           else (y, recourse xl ys)
   in  recourse
