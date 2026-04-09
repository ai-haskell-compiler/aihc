{- ORACLE_TEST pass -}
{-# LANGUAGE UnicodeSyntax #-}

-- Unicode type variable in newtype declaration

module UnicodeTypeVarNewtype where

newtype Wrapper ψ = Wrap { unwrap :: ψ }
