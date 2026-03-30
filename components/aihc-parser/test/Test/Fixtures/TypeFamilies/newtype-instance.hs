{- ORACLE_TEST
id: newtype-instance
category: declarations
expected: xfail
reason: newtype instance
-}
{-# LANGUAGE TypeFamilies #-}
module NewtypeInstance where

data family T a
newtype instance T Char = TC Bool
