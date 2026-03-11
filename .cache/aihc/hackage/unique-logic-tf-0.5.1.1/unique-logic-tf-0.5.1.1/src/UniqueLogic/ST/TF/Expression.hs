{-# LANGUAGE FlexibleContexts #-}
module UniqueLogic.ST.TF.Expression (
   T,
   -- * Construct primitive expressions
   constant, fromVariable,
   -- * Operators from rules with small numbers of arguments
   fromRule1, fromRule2, fromRule3,
   -- * Operators from rules with any number of arguments
   Apply, arg, runApply,
   -- * Predicates on expressions
   (=:=),
   -- * Common operators (see also 'Num' and 'Fractional' instances)
   (=!=),
   sqr, sqrt,
   max, maximum,
   pair,
   ) where

import qualified UniqueLogic.ST.TF.ZeroFractional as ZeroFractional
import qualified UniqueLogic.ST.TF.Rule as Rule
import qualified UniqueLogic.ST.TF.System as Sys
import qualified Data.Ref as Ref

import Control.Monad (ap, )
import Control.Applicative (Applicative, pure, liftA, liftA2, (<*>), )

-- import Control.Category ((.))
-- import Data.Maybe (Maybe)

-- import Prelude (Double, Eq, Ord, (+), (*), (/))
import qualified Prelude as P
import Prelude hiding (max, maximum, sqrt)


{- |
An expression is defined by a set of equations
and the variable at the top-level.
The value of the expression equals the value of the top variable.
-}
newtype T w s a = Cons (Sys.T w s (Sys.Variable w s a))


{- |
Make a constant expression of a simple numeric value.
-}
constant :: (Sys.C w, Sys.Value w a, Ref.C s) => a -> T w s a
constant = Cons . Sys.constant

fromVariable :: (Ref.C s) => Sys.Variable w s a -> T w s a
fromVariable = Cons . return


fromRule1 ::
   (Sys.C w, Sys.Value w a, Ref.C s) =>
   (Sys.Variable w s a -> Sys.T w s ()) ->
   (T w s a)
fromRule1 rule = Cons $ do
   xv <- Sys.localVariable
   rule xv
   return xv

fromRule2, _fromRule2 ::
   (Sys.C w, Sys.Value w b, Ref.C s) =>
   (Sys.Variable w s a -> Sys.Variable w s b -> Sys.T w s ()) ->
   (T w s a -> T w s b)
fromRule2 rule (Cons x) = Cons $ do
   xv <- x
   yv <- Sys.localVariable
   rule xv yv
   return yv

fromRule3, _fromRule3 ::
   (Sys.C w, Sys.Value w c, Ref.C s) =>
   (Sys.Variable w s a -> Sys.Variable w s b -> Sys.Variable w s c -> Sys.T w s ()) ->
   (T w s a -> T w s b -> T w s c)
fromRule3 rule (Cons x) (Cons y) = Cons $ do
   xv <- x
   yv <- y
   zv <- Sys.localVariable
   rule xv yv zv
   return zv


newtype Apply w s f = Apply (Sys.T w s f)

instance (Ref.C s) => Functor (Apply w s) where
   fmap f (Apply a) = Apply $ fmap f a

instance (Ref.C s) => Applicative (Apply w s) where
   pure a = Apply $ return a
   Apply f <*> Apply a = Apply $ ap f a


{- |
This function allows to generalize 'fromRule2' and 'fromRule3' to more arguments
using 'Applicative' combinators.

Example:

> fromRule3 rule x y
>    = runApply $ liftA2 rule (arg x) (arg y)
>    = runApply $ pure rule <*> arg x <*> arg y

Building rules with 'arg' provides more granularity
than using auxiliary 'pair' rules!
-}
arg ::
   T w s a -> Apply w s (Sys.Variable w s a)
arg (Cons x) = Apply x

runApply ::
   (Sys.C w, Sys.Value w a, Ref.C s) =>
   Apply w s (Sys.Variable w s a -> Sys.T w s ()) ->
   T w s a
runApply (Apply rule) = Cons $ do
   f <- rule
   xv <- Sys.localVariable
   f xv
   return xv

{-
examples of how to use 'arg' and 'runApply'
-}
_fromRule2 rule x = runApply $ liftA rule $ arg x
_fromRule3 rule x y = runApply $ liftA2 rule (arg x) (arg y)


instance
   (Sys.C w, Sys.Value w a, ZeroFractional.C a, Ref.C s) =>
      P.Num (T w s a) where
   fromInteger = constant . fromInteger
   (+) = fromRule3 Rule.add
   (-) = fromRule3 (\z x y -> Rule.add x y z)
   (*) = fromRule3 Rule.mul
   abs = fromRule2 (Sys.assignment2 abs)
   signum = fromRule2 (Sys.assignment2 signum)

instance
   (Sys.C w, Sys.Value w a, ZeroFractional.C a, Ref.C s) =>
      P.Fractional (T w s a) where
   fromRational = constant . fromRational
   (/) = fromRule3 (\z x y -> Rule.mul x y z)

sqr :: (Sys.C w, Sys.Value w a, P.Floating a, Ref.C s) => T w s a -> T w s a
sqr = fromRule2 Rule.square

sqrt :: (Sys.C w, Sys.Value w a, P.Floating a, Ref.C s) => T w s a -> T w s a
sqrt = fromRule2 (flip Rule.square)


infixl 4 =!=

(=!=) :: (Sys.C w, Ref.C s) => T w s a -> T w s a -> T w s a
(=!=) (Cons x) (Cons y) = Cons $ do
   xv <- x
   yv <- y
   Rule.equ xv yv
   return xv

infix 0 =:=

(=:=) :: (Sys.C w, Ref.C s) => T w s a -> T w s a -> Sys.T w s ()
(=:=) (Cons x) (Cons y) = do
   xv <- x
   yv <- y
   Rule.equ xv yv


{- |
We are not able to implement a full Ord instance
including Eq superclass and comparisons,
but we need to compute maxima.
-}
max :: (Sys.C w, Ord a, Sys.Value w a, Ref.C s) => T w s a -> T w s a -> T w s a
max = fromRule3 Rule.max

maximum :: (Sys.C w, Ord a, Sys.Value w a, Ref.C s) => [T w s a] -> T w s a
maximum = foldl1 max


{- |
Construct or decompose a pair.
-}
pair ::
   (Sys.C w, Sys.Value w a, Sys.Value w b, Sys.Value w (a,b), Ref.C s) =>
   T w s a -> T w s b -> T w s (a,b)
pair = fromRule3 Rule.pair
