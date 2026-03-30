{- ORACLE_TEST pass -}
{-# LANGUAGE GADTSyntax #-}

module GadtDeriving where

data Maybe1 a where {
    Nothing1 :: Maybe1 a ;
    Just1    :: a -> Maybe1 a
  } deriving (Eq, Ord)

data Maybe2 a = Nothing2 | Just2 a
     deriving (Eq, Ord)