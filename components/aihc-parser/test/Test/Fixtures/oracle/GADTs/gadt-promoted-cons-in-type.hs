{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoListTuplePuns #-}
{-# LANGUAGE TypeFamilies #-}

module UnionPromotedCons where

import Data.List (List)

data Union f (as :: List u) where
  This :: f a -> Union f (a : as)
  That :: Union f as -> Union f (a : as)
