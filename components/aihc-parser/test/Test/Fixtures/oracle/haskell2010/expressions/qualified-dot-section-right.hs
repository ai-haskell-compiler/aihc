{- ORACLE_TEST pass -}
module ExprQualifiedDotSectionRight where

import qualified Prelude as Foo

x = (Foo.. Foo.id)
