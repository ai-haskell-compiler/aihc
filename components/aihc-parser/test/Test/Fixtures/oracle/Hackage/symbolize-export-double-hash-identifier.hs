{- ORACLE_TEST xfail reason="module export lists reject identifiers that end in double hashes" -}
{-# LANGUAGE MagicHash #-}

module M (f##) where

f## = undefined
