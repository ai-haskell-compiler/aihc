{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.REST.MetaTerm where

import Data.String
import Data.Hashable
import GHC.Generics (Generic)

import Language.REST.Op
import Language.REST.RuntimeTerm

-- | A MetaTerm is a term with variables; used for 'Rewrite' rules
data MetaTerm =
    Var String
  | RWApp Op [MetaTerm] deriving (Eq, Ord, Show, Generic, Hashable)

instance IsString MetaTerm where
  fromString = Var

-- | Helper class, enabling conversion of 'RuntimeTerm's to 'MetaTerm's
class ToMetaTerm a where
  toMetaTerm :: a -> MetaTerm

instance ToMetaTerm MetaTerm where
  toMetaTerm = id

instance ToMetaTerm RuntimeTerm where
  toMetaTerm (App f xs) = RWApp f (map toMetaTerm xs)
