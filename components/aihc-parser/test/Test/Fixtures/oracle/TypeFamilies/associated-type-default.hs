{- ORACLE_TEST xfail associated type default equation without 'instance' keyword is not yet supported -}
{-# LANGUAGE TypeFamilies #-}
module AssociatedTypeDefault where

-- Type family default equation without 'instance' keyword.
-- GHC allows: type F a = <rhs> inside a class body as shorthand for
-- type instance F a = <rhs>.  Parser currently rejects the '=' here.
class Foo n where
  type O n
  type O n = Int
