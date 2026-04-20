{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MemoTrieAssociatedDataFamilyOperator where

class C a where
  data (:*:) a :: * -> *
