{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.LinearProgramming.Common (
   Term(..), (.*),
   Inequality(..),
   Bound(..),
   Bounds,
   Constraints,
   Direction(..),
   Objective,
   free, (<=.), (>=.), (==.), (>=<.),
   objectiveFromTerms,
   ) where

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable (Array)



data Term a ix = Term a ix
   deriving (Show)


infix 7 .*

(.*) :: a -> ix -> Term a ix
(.*) = Term


data Inequality x = Inequality x Bound
   deriving Show

data Bound =
     LessEqual Double
   | GreaterEqual Double
   | Between Double Double
   | Equal Double
   | Free
   deriving Show

instance Functor Inequality where
   fmap f (Inequality x bnd)  =  Inequality (f x) bnd

type Bounds ix = [Inequality ix]

type Constraints a ix = [Inequality [Term a ix]]

data Direction = Minimize | Maximize
   deriving (Eq, Enum, Bounded, Show)

type Objective sh = Array sh Double



infix 4 <=., >=., >=<., ==.

(<=.), (>=.), (==.) :: x -> Double -> Inequality x
x <=. bnd = Inequality x $ LessEqual bnd
x >=. bnd = Inequality x $ GreaterEqual bnd
x ==. bnd = Inequality x $ Equal bnd

(>=<.) :: x -> (Double,Double) -> Inequality x
x >=<. bnd = Inequality x $ uncurry Between bnd

free :: x -> Inequality x
free x = Inequality x Free



objectiveFromTerms ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   sh -> [Term Double ix] -> Objective sh
objectiveFromTerms sh =
   Array.fromAssociations 0 sh . map (\(Term x ix) -> (ix,x))
