{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}

module AssociatedTypeDefault where

-- Type family default equation without 'instance' keyword.
-- GHC allows: type F a = <rhs> inside a class body as shorthand for
-- type instance F a = <rhs>.
class Foo n where
  type O n
  type O n = Int
