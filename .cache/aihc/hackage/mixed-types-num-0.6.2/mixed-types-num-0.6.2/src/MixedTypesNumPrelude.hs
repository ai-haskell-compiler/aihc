{-|
    Module      :  MixedTypesNumPrelude
    Description :  Bottom-up typed numeric expressions
    Copyright   :  (c) Michal Konecny, Pieter Collins
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    @MixedTypesNumPrelude@ provides a version of @Prelude@ where
    unary and binary operations such as @not@, @+@, @==@
    have their result type derived from the parameter type(s).

    This module facilitates a single-line import for the package
    mixed-types-num.  See the re-exported modules for further details.
-}


module MixedTypesNumPrelude
(
  -- * Re-exporting Prelude, hiding the operators we are changing
  module Numeric.MixedTypes.PreludeHiding,
  -- * Error-collecting wrapper type
  CN, cn, unCN, clearPotentialErrors,
  -- * A part of package ``convertible''
  module Data.Convertible.Base,
  -- * Modules with Prelude alternatives
  module Numeric.MixedTypes.Literals,
  module Numeric.MixedTypes.Bool,
  module Numeric.MixedTypes.Eq,
  module Numeric.MixedTypes.Ord,
  module Numeric.MixedTypes.MinMaxAbs,
  module Numeric.MixedTypes.AddSub,
  module Numeric.MixedTypes.Round,
  module Numeric.MixedTypes.Reduce,
  module Numeric.MixedTypes.Mul,
  module Numeric.MixedTypes.Ring,
  module Numeric.MixedTypes.Div,
  module Numeric.MixedTypes.Power,
  module Numeric.MixedTypes.Field,
  module Numeric.MixedTypes.Elementary,
  module Numeric.MixedTypes.Complex,
  -- module Numeric.CollectErrors,
  module Utils.TH.DeclForTypes,
  module Utils.Test.EnforceRange,
  -- * Re-export for convenient Rational literals
  (%)
)
where

import Data.Ratio ((%))
import Numeric.CollectErrors (CN, cn, unCN, clearPotentialErrors)
import Data.Convertible.Instances.Num()
import Data.Convertible.Base
import Utils.TH.DeclForTypes
import Utils.Test.EnforceRange
import Numeric.MixedTypes.PreludeHiding
import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Round
import Numeric.MixedTypes.Reduce
import Numeric.MixedTypes.Mul
import Numeric.MixedTypes.Ring
import Numeric.MixedTypes.Div
import Numeric.MixedTypes.Power
import Numeric.MixedTypes.Field
import Numeric.MixedTypes.Elementary
import Numeric.MixedTypes.Complex

