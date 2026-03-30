{- ORACLE_TEST
id: datakinds-numeric-type-literal
category: types
expected: pass
-}
{-# LANGUAGE DataKinds #-}
module M where
import Data.Proxy
_1 :: Proxy 1
_1 = Proxy
