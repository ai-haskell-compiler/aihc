{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}

module TypeFamilyFunctionInstance where

import qualified Data.Map as Map

instance Ord k => Function (Map.Map k v) where
    type instance Domain (Map.Map k v) = k
    type instance Codomain (Map.Map k v) = Maybe v
