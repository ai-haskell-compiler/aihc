{- ORACLE_TEST xfail reason="parenthesized section with fmap operator not handled" -}
{-# LANGUAGE GHC2021 #-}
module ParenSectionFmap where

writePandocWith f wo =
    (foo . f <$>)
    undefined
