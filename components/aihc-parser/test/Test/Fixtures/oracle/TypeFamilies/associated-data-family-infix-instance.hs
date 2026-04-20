{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AssociatedDataFamilyInfixInstance where

data X = X

class C a where
  data a :->: b

instance C X where
  data a :->: b = VoidTrie
