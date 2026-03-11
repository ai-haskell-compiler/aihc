{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean.Overload
-- License     :  BSD3
-- 
-- Author      :  Alex Horsman (aninhumer)
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- 
-- Definitions of Prelude function names in terms of their corresponding
-- Data.Boolean generalised implementation. This can then be used as part
-- of a partial or complete Prelude replacement.
--
-- Also exports ifThenElse for use with RebindableSyntax.
----------------------------------------------------------------------

module Data.Boolean.Overload
  ( module Data.Boolean,
    (&&), (||), not,
    ifThenElse,
    (==), (/=), 
    (<), (>), (<=), (>=),
    min, max
  ) where

import Data.Boolean
import Prelude hiding
  ( (&&), (||), not,
    (==), (/=), 
    (<), (>), (<=), (>=),
    min, max
#if MIN_VERSION_base(4,8,0)
    , (<*)
#endif
  )

infix  4  ==, /=, <, <=, >=, >
infixr 3 &&
infixr 2 ||

(&&) :: Boolean a => a -> a -> a
(&&) = (&&*)

(||) :: Boolean a => a -> a -> a
(||) = (||*)

not :: Boolean a => a -> a
not = notB


-- For use with RebindableSyntax
ifThenElse :: IfB a => BooleanOf a -> a -> a -> a
ifThenElse = ifB


(==) :: EqB a => a -> a -> BooleanOf a
(==) = (==*)
(/=) :: EqB a => a -> a -> BooleanOf a
(/=) = (/=*)


(<) :: OrdB a => a -> a -> BooleanOf a
(<) = (<*)
(>) :: OrdB a => a -> a -> BooleanOf a
(>) = (>*)
(<=) :: OrdB a => a -> a -> BooleanOf a
(<=) = (<=*)
(>=) :: OrdB a => a -> a -> BooleanOf a
(>=) = (>=*)

min :: (IfB a, OrdB a) => a -> a -> a
min = minB
max :: (IfB a, OrdB a) => a -> a -> a
max = maxB


