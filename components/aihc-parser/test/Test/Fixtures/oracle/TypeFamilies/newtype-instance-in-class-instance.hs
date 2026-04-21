{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}

module NewtypeInstanceInClassInstance where

import qualified Data.Map as Map

class Wrappable v where
    type Wrap v a

instance Wrappable (Map.Map k v) where
    newtype instance Wrap (Map.Map k v) a = WrapResult (Maybe a)