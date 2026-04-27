{- ORACLE_TEST pass -}
module InlinePragmaLowercase where

fn :: Int -> Int
fn x = x + 1
{-#inline fn#-}
