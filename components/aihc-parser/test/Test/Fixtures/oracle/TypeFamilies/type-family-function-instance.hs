{- ORACLE_TEST xfail parser rejects type family instance with Function class syntax -}
{-# LANGUAGE TypeFamilies #-}

module TypeFamilyFunctionInstance where

import qualified Data.Map as Map

instance Ord k => Function (Map.Map k v) where
    type instance Domain (Map.Map k v) = k
    type instance Codomain (Map.Map k v) = Maybe v
