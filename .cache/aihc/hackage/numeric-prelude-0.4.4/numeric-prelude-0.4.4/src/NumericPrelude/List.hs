module NumericPrelude.List where

import Data.List.HT (switchL, switchR, )


{- * Zip lists -}

{- | zip two lists using an arbitrary function, the shorter list is padded -}
{-# INLINE zipWithPad #-}
zipWithPad :: a               {-^ padding value -}
           -> (a -> a -> b)   {-^ function applied to corresponding elements of the lists -}
           -> [a]
           -> [a]
           -> [b]
zipWithPad z f =
   let aux l []          = map (\x -> f x z) l
       aux [] l          = map (\y -> f z y) l
       aux (x:xs) (y:ys) = f x y : aux xs ys
   in  aux

{-# INLINE zipWithOverlap #-}
zipWithOverlap :: (a -> c) -> (b -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithOverlap fa fb fab =
   let aux (x:xs) (y:ys) = fab x y : aux xs ys
       aux xs [] = map fa xs
       aux [] ys = map fb ys
   in  aux

{-
This is exported as Checked.zipWith.
We need to define it here in order to prevent an import cycle.
-}
zipWithChecked
   :: (a -> b -> c)   {-^ function applied to corresponding elements of the lists -}
   -> [a]
   -> [b]
   -> [c]
zipWithChecked f =
   let aux (x:xs) (y:ys) = f x y : aux xs ys
       aux []     []     = []
       aux _      _      = error "Checked.zipWith: lists must have the same length"
   in  aux


{- |
Apply a function to the last element of a list.
If the list is empty, nothing changes.
-}
{-# INLINE mapLast #-}
mapLast :: (a -> a) -> [a] -> [a]
mapLast f =
   switchL []
      (\x xs ->
         uncurry (:) $
         foldr (\x1 k x0 -> (x0, uncurry (:) (k x1)))
            (\x0 -> (f x0, [])) xs x)

mapLast' :: (a -> a) -> [a] -> [a]
mapLast' f =
   let recourse [] = [] -- behaviour as needed in powerBasis
          -- otherwise: error "mapLast: empty list"
       recourse (x:xs) =
          uncurry (:) $
          if null xs
            then (f x, [])
            else (x, recourse xs)
   in  recourse

mapLast'' :: (a -> a) -> [a] -> [a]
mapLast'' f =
   switchR [] (\xs x -> xs ++ [f x])
