{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98


Event lists starting with a body and ending with a body.

-}
module Data.EventList.Relative.BodyBody
   (T,
    concatMapMonoid, traverse, mapM,
   ) where

import Data.EventList.Relative.BodyBodyPrivate

-- import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform

import Control.Monad (Monad, )
import Control.Applicative (Applicative, WrappedMonad(WrapMonad, unwrapMonad), liftA, )
import Data.Monoid (Monoid, )

import Data.Function ((.), )
import Prelude ()


concatMapMonoid :: Monoid m =>
   (time -> m) -> (body -> m) ->
   T time body -> m
concatMapMonoid f g = Uniform.concatMapMonoid f g . decons

traverse :: Applicative m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   T time0 body0 -> m (T time1 body1)
traverse f g =
   liftA Cons . Uniform.traverse f g . decons

mapM :: Monad m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   T time0 body0 -> m (T time1 body1)
mapM f g =
   unwrapMonad . traverse (WrapMonad . f) (WrapMonad . g)
