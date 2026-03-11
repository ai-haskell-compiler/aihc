{-|
    Module      :  Numeric.OrdGenericBool
    Description :  Order with generic Bool-like type
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Order ops with generic return type.  
    
    Originally developed for semi-decidable comparisons that return Kleenean instead of Bool.
    
    Use in combination with:

    import Prelude hiding ((==),(/=),(<),(<=),(>),(>=),abs,max,min,not,(&&),(||))
    import qualified Prelude as P

    The usual comparison operators are still available using the P prefix.
-}
module Numeric.OrdGenericBool
(
    -- * order operators
    (==), (/=), (<), (<=), (>), (>=), abs, max, min,
    -- * logical operators
    not, (&&), (||)
)
where

import Prelude hiding ((==),(/=),(<),(<=),(>),(>=),abs,max,min,not,(&&),(||))

import qualified MixedTypesNumPrelude as M

infix  4  ==, /=

(==) :: (M.HasEqAsymmetric t t) => t -> t -> (M.EqCompareType t t)
(==) = (M.==)

(/=) :: (M.HasEqAsymmetric t t) => t -> t -> (M.EqCompareType t t)
(/=) = (M./=)

infix  4  <, <=, >=, >

(<) :: (M.HasOrderAsymmetric t t) => t -> t -> (M.OrderCompareType t t)
(<) = (M.<)

(<=) :: (M.HasOrderAsymmetric t t) => t -> t -> (M.OrderCompareType t t)
(<=) = (M.<=)

(>) :: (M.HasOrderAsymmetric t t) => t -> t -> (M.OrderCompareType t t)
(>) = (M.>)

(>=) :: (M.HasOrderAsymmetric t t) => t -> t -> (M.OrderCompareType t t)
(>=) = (M.>=)

abs :: (M.CanAbsSameType t) => t -> t
abs = (M.abs)

max :: (M.CanMinMaxSameType t) => t -> t -> t
max = (M.max)

min :: (M.CanMinMaxSameType t) => t -> t -> t
min = (M.min)

infixr 3  &&
infixr 2  ||

(&&) :: (M.CanAndOrSameType t) => t -> t -> t
(&&) = (M.&&)

(||) :: (M.CanAndOrSameType t) => t -> t -> t
(||) = (M.||)

not :: (M.CanNegSameType t) => t -> t
not = M.not
