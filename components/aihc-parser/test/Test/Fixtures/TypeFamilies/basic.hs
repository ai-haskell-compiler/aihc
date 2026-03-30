{- ORACLE_TEST
id: basic
category: declarations
expected: xfail
reason: basic type family
-}
{-# LANGUAGE TypeFamilies #-}
module Basic where

type family F a
