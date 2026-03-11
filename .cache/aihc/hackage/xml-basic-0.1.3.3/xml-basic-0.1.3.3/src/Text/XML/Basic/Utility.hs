module Text.XML.Basic.Utility where

import Data.List.HT (switchL, break, )
import Data.Tuple.HT (mapSnd, )


import Prelude hiding (break, )


{- |
Needs 'break' from utility-ht in order to be as lazy as 'updateAppend''.
-}
updateAppend :: (a -> Bool) -> a -> (a -> a) -> [a] -> [a]
updateAppend p deflt f =
   uncurry (++) .
   mapSnd (uncurry (:) . switchL (deflt,[]) ((,) . f)) .
   break p

{- |
Apply @f@ to the first element, where @p@ holds.
If no such element exists, append the default value @deflt@ to the list.
-}
updateAppend' :: (a -> Bool) -> a -> (a -> a) -> [a] -> [a]
updateAppend' p deflt f =
   let recourse xt =
          uncurry (:) $
          case xt of
             [] -> (deflt,[])
             (x:xs) ->
                if p x
                  then (f x, xs)
                  else (x, recourse xs)
   in  recourse
