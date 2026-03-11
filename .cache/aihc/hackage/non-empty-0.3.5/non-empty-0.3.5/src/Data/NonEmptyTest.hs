module Data.NonEmptyTest where

import qualified Data.NonEmpty.Mixed as NonEmptyMixed
import qualified Data.NonEmptyPrivate as NonEmpty

import Control.Monad (guard, )

import qualified Data.List.HT as ListHT
import Data.Tuple.HT (mapFst, )


foldBalanced :: NonEmpty.T [] Integer -> Bool
foldBalanced xs =
   NonEmpty.foldBalanced (+) xs == NonEmpty.sum xs



filterToInfixesAlt :: (a -> Bool) -> [a] -> [NonEmpty.T [] a]
filterToInfixesAlt p =
   snd .
   foldr
      (\x ~(b1, yt) ->
         let b0 = p x
         in  (b0,
              if b0
                then uncurry (:) $
                     mapFst (NonEmpty.cons x) $
                     case guard b1 >> ListHT.viewL yt of
                        Just (y,ys) -> (NonEmpty.flatten y, ys)
                        Nothing -> ([], yt)
                else yt))
      (True, [])

filterToInfixes :: [Int] -> Bool
filterToInfixes xs =
   NonEmptyMixed.filterToInfixes (>0) xs == filterToInfixesAlt (>0) xs
