{- ORACLE_TEST pass -}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}

module DerivingViaMVector where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

data TestPair a b = TestPair a b

deriving via (TestPair a b `U.As` (a, b)) instance (U.Unbox a, U.Unbox b) => GM.MVector U.MVector (TestPair a b)
deriving via (TestPair a b `U.As` (a, b)) instance (U.Unbox a, U.Unbox b) => G.Vector   U.Vector  (TestPair a b)
