{-|
    Module      :  Utils.Test.EnforceRange
    Description :  squash generated numbers to a given range
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Utility for squashing randomly generated numbers to a given range.
-}
module Utils.Test.EnforceRange 
    (enforceRange, CanEnforceRange)
where

import Numeric.MixedTypes.PreludeHiding
-- import qualified Prelude as P

-- import Numeric.CollectErrors

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
-- import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Mul
import Numeric.MixedTypes.Field
import Numeric.MixedTypes.Round

type CanEnforceRange t b =
    (CanAddSubMulDivBy t Integer
    , CanAddSameType t, CanSubSameType t, CanAbsSameType t
    , CanDivIModIntegerSameType t
    , ConvertibleExactlyWithSample b t
    , HasOrderCertainly t t)

{-| 
    @enforceRange (Just l, Just u) a@ where @l < u@ returns an arbitrary value @b@ with @u < b < l@.
    Moreover, the returned values are distributed roughly evenly if the input values @a@ are distributed 
    roughly evenly in a large neighbourhood of the interval @[l,r]@.
    In most cases, when @l<a<u@, then @b=a@.
-}
enforceRange ::
    (CanEnforceRange t b) => (Maybe b, Maybe b) -> t -> t
enforceRange (Just l_, Just u_) (a::t) 
    | not (l !<! u) = error "enforceRange: inconsistent range"
    | l !<! a && a !<! u = a
    | l !<! b && b !<! u = b
    | otherwise = (u+l)/2
    where
    l = convertExactlyWithSample a l_ :: t
    u = convertExactlyWithSample a u_ :: t
    b = l + ((abs a) `mod` (u-l))
enforceRange (Just l_, _) (a::t)
    | l !<! a = a
    | otherwise = (2*l-a+1)
    where
    l = convertExactlyWithSample a l_ :: t
enforceRange (_, Just u_) (a::t)
    | a !<! u = a
    | otherwise = (2*u-a-1)
    where
    u = convertExactlyWithSample a u_ :: t
enforceRange _ a = a
