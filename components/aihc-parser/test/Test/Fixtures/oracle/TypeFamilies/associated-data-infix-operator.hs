{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AssociatedDataInfixOperator where

class C a where
  data x :*: y :: *
