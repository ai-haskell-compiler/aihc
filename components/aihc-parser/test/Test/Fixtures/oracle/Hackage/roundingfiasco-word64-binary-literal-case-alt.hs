{- ORACLE_TEST xfail reason="binary extended literals with primitive suffixes fail in case alternatives" -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}

module M where

f x = case x of
  0b0#Word64 -> 0#
