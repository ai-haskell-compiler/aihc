{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Algebra.VectorSpace where

import qualified Algebra.Module as Module
import qualified Algebra.Field  as Field
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Number.Ratio   as Ratio

import qualified Data.Complex as Complex98

import qualified Prelude as P


class (Field.C a, Module.C a b) => C a b


{-* Instances for atomic types -}

instance C P.Float P.Float

instance C P.Double P.Double

{-* Instances for composed types -}

instance (PID.C a) => C (Ratio.T a) (Ratio.T a)

instance (C a b0, C a b1) => C a (b0, b1)

instance (C a b0, C a b1, C a b2) => C a (b0, b1, b2)

instance (C a b) => C a [b]

instance (C a b) => C a (c -> b)

instance (C a b, P.RealFloat b) => C a (Complex98.Complex b)
