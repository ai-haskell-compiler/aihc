module Data.Function.Tools (
  const2
, const3
, applyWhen
, applyUnless
, apply2way
) where

{-
import Control.Arrow ((&&&))
 -}

const2 :: a -> b -> c -> a
const2 = const . const

const3 :: a -> b -> c -> d -> a
const3 = const . const . const

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen b f = if b then f else id

applyUnless :: Bool -> (a -> a) -> a -> a
applyUnless b f = if b then id else f

apply2way :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
apply2way fgen f1 f2 x = fgen (f1 x) (f2 x)

{-
apply2way fgen f1 f2 = uncurry fgen . (f1 &&& f2)
 -}
