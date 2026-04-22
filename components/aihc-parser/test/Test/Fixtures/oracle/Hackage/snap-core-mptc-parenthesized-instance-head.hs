{- ORACLE_TEST xfail parenthesized prefix instance head merges trailing types losing paren boundary in roundtrip -}
{-# LANGUAGE MultiParamTypeClasses #-}
module A where
class C a b
instance (C Int) Bool where
