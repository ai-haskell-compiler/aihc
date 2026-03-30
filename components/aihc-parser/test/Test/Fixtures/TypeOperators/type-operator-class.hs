{- ORACLE_TEST
id: type-operator-class
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE TypeOperators #-}

module TypeOperatorClass where

infix 4 :=:
class a :=: b where
  proof :: a -> b -> ()
