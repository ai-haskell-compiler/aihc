{- ORACLE_TEST pass -}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}

module EmptyDataDeclsWithKindContext where

data (:+) :: C => ()
