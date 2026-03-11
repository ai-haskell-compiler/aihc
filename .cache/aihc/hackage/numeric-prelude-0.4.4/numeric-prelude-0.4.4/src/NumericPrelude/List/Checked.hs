{-# LANGUAGE RebindableSyntax #-}
{- |
Some functions that are counterparts of functions from "Data.List"
using NumericPrelude.Numeric type classes.
They are distinct in that they check for valid arguments,
e.g. the length argument of 'take' must be at most the length of the input list.
However, since many Haskell programs rely on the absence of such checks,
we did not make these the default implementations
as in "NumericPrelude.List.Generic".
-}
module NumericPrelude.List.Checked
   (take, drop, splitAt, (!!), zipWith,
   ) where

import qualified Algebra.ToInteger  as ToInteger
import Algebra.Ring (one, )
import Algebra.Additive (zero, (-), )

import Data.Tuple.HT (mapFst, )

import qualified NumericPrelude.List as NPList

import NumericPrelude.Base hiding (take, drop, splitAt, length, replicate, (!!), zipWith, )


moduleError :: String -> String -> a
moduleError name msg =
   error $ "NumericPrelude.List.Left." ++ name ++ ": " ++ msg

{- |
Taken number of elements must be at most the length of the list,
otherwise the end of the list is undefined.
-}
take :: (ToInteger.C n) => n -> [a] -> [a]
take n =
   if n<=zero
     then const []
     else \xt ->
       case xt of
          [] -> moduleError "take" "index out of range"
          (x:xs) -> x : take (n-one) xs

{- |
Dropped number of elements must be at most the length of the list,
otherwise the end of the list is undefined.
-}
drop :: (ToInteger.C n) => n -> [a] -> [a]
drop n =
   if n<=zero
     then id
     else \xt ->
       case xt of
          [] -> moduleError "drop" "index out of range"
          (_:xs) -> drop (n-one) xs

{- |
Split position must be at most the length of the list,
otherwise the end of the first list and the second list are undefined.
-}
splitAt :: (ToInteger.C n) => n -> [a] -> ([a], [a])
splitAt n xt =
   if n<=zero
     then ([], xt)
     else
       case xt of
          [] -> moduleError "splitAt" "index out of range"
          (x:xs) -> mapFst (x:) $ splitAt (n-one) xs

{- |
The index must be smaller than the length of the list,
otherwise the result is undefined.
-}
(!!) :: (ToInteger.C n) => [a] -> n -> a
(!!) [] _ = moduleError "(!!)" "index out of range"
(!!) (x:xs) n =
   if n<=zero
     then x
     else (!!) xs (n-one)


{- |
Zip two lists which must be of the same length.
This is checked only lazily, that is unequal lengths are detected only
if the list is evaluated completely.
But it is more strict than @zipWithPad undefined f@
since the latter one may succeed on unequal length list if @f@ is lazy.
-}
zipWith
   :: (a -> b -> c)   {-^ function applied to corresponding elements of the lists -}
   -> [a]
   -> [b]
   -> [c]
zipWith = NPList.zipWithChecked
