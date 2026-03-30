{- ORACLE_TEST
id: kindsig-data-param
category: declarations
expected: pass
-}
{-# LANGUAGE KindSignatures #-}

module KindSignaturesDataParam where

import Data.Kind (Type)

data Proxy (a :: Type) = Proxy
