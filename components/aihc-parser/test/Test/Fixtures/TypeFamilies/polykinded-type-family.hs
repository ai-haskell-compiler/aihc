{- ORACLE_TEST
id: polykinded-type-family
category: declarations
expected: xfail
reason: polykinded type family
-}
{-# LANGUAGE TypeFamilies, PolyKinds #-}
module PolykindedTypeFamily where

type family J a :: k
type instance J Int = Bool
type instance J Int = Maybe
