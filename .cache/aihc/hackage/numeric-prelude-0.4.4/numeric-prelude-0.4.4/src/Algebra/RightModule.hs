{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Algebra.RightModule where

import qualified Algebra.Ring     as Ring
import qualified Algebra.Additive as Additive



-- Is this right?
infixl 7 <*

class (Ring.C a, Additive.C b) => C a b where
    (<*) :: b -> a -> b
