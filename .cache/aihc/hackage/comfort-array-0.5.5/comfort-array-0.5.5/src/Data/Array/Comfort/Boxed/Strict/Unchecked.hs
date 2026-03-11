{-# LANGUAGE TypeFamilies #-}
module Data.Array.Comfort.Boxed.Strict.Unchecked where

import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Boxed.Unchecked (Array(Array))

import qualified Data.Primitive.Array as Prim

import qualified Control.Monad.ST.Strict as STStrict
import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS

import Prelude hiding (map, zipWith)


toList :: (Shape.C sh) => Array sh a -> [a]
toList (Array sh arr) =
   STStrict.runST (mapM (Prim.indexArrayM arr) $ take (Shape.size sh) [0..])

map :: (Shape.C sh) => (a -> b) -> Array sh a -> Array sh b
map f (Array sh arr) = Array sh $ Prim.mapArray' f arr

zipWith ::
   (Shape.C sh) => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWith f (Array sha arra) (Array _shb arrb) =
   Array sha $
   STStrict.runST
      (flip MS.evalStateT 0 $
       Prim.traverseArrayP
         (\a -> do
            k <- MS.get
            b <- MT.lift $ Prim.indexArrayM arrb k
            MS.put (k+1)
            return $ f a b)
         arra)
