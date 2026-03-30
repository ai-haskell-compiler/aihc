{- ORACLE_TEST
id: data-family
category: declarations
expected: xfail
reason: data family
-}
{-# LANGUAGE TypeFamilies #-}
module DataFamily where

data family D a
