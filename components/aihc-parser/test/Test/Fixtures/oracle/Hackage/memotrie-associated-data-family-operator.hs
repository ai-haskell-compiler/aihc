{- ORACLE_TEST xfail reason="associated data family declarations reject operator names" -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MemoTrieAssociatedDataFamilyOperator where

class C a where
  data (:*:) a :: * -> *
