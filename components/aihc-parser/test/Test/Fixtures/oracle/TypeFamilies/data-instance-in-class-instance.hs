{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}

module DataInstanceInClassInstance where

import qualified Data.Map as Map

class Lookupable v where
    data Lookup v a

instance Lookupable (Map.Map k v) where
    data instance Lookup (Map.Map k v) a = LookupResult (Maybe a)