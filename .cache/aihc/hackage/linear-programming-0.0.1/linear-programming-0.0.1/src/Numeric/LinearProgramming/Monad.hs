{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Generic implementation of a monad that collects constraints
over multiple stages.
It can be used to test solvers that allow for warm start
or for solvers that do not allow for warm start at all
(like GLPK's interior point solver).
-}
module Numeric.LinearProgramming.Monad (
   T,
   run,
   lift,
   ) where

import Numeric.LinearProgramming.Common

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape

import qualified Control.Monad.Trans.RWS as MRWS
import Control.Monad (when)
import Control.Applicative (Applicative)


newtype T sh a =
   Cons (MRWS.RWS
            (sh, Bounds (Shape.Index sh))
            ()
            (Constraints Double (Shape.Index sh))
            a)
      deriving (Functor, Applicative, Monad)


run ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   sh -> Bounds ix -> T sh a -> a
run shape bounds (Cons act) =
   fst $ MRWS.evalRWS act (shape, bounds) []

lift ::
   (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix) =>
   (Bounds ix -> Constraints Double ix -> (Direction, Objective sh) -> a) ->
   Constraints Double ix -> (Direction, Objective sh) -> T sh a
lift solver constrs dirObj@(_dir,obj) = Cons $ do
   (shape,bounds) <- MRWS.ask
   when (shape /= Array.shape obj) $
      error "LinearProgramming.Monad.solve: objective shape mismatch"
   MRWS.modify (constrs++)
   allConstrs <- MRWS.get
   return $ solver bounds allConstrs dirObj
