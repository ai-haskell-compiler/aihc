{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module DefaultAssociatedTypeWildcard where

class C a where
  type F a
  type F _ = ()
