{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module ParenSectionFmap where

writePandocWith f wo =
    (foo . f <$>)
    undefined
