{- ORACLE_TEST xfail DEPRECATED pragma with multiple declarations not supported by parser -}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module DeclWarnings where

data T = T1 | T2
newtype Wrap = Wrap Int
class C a where
  methodC :: a -> Int

legacy :: Int
legacy = 10

unsafeHeadish :: [a] -> a
unsafeHeadish (x:_) = x
unsafeHeadish [] = error "empty"

pattern D :: ()
pattern D = ()

data DType = MkDType

{-# DEPRECATED legacy, C, T ["Do not use these", "Use replacements instead"] #-}
{-# WARNING in "x-partial" unsafeHeadish "This function is partial" #-}
{-# DEPRECATED data D "This deprecates only the pattern synonym D" #-}
{-# DEPRECATED type DType "This deprecates only the type constructor DType" #-}
