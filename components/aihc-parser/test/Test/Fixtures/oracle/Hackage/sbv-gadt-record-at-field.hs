{- ORACLE_TEST pass -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module S where

data family C

data instance C where
  (:+) :: {(@) :: Int} -> C
