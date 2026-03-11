module Data.Array.Comfort.Check where

import qualified Data.Array.Comfort.Shape as Shape

import Text.Printf (printf)


{-# INLINE reshape #-}
reshape ::
   (Shape.C sh0, Shape.C sh1) =>
   String ->
   (array0 -> sh0) ->
   (sh1 -> array0 -> array1) ->
   sh1 -> array0 -> array1
reshape name shape uncheckedReshape sh1 arr =
   let n0 = Shape.size $ shape arr
       n1 = Shape.size sh1
   in if n0 == n1
         then uncheckedReshape sh1 arr
         else error $
              printf
                 ("Array.Comfort.%s.reshape: " ++
                  "different sizes of old (%d) and new (%d) shape")
                 name n0 n1
