{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module ExplicitNamespacesFixityDataOperator where

-- GHC 9.16 adds `data` namespaces in fixity declarations.
-- Enable this when the oracle switches to GHC 9.16:
-- infixr 0 data $

x :: ()
x = ()
