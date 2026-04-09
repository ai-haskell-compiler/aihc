{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module ExportQualifiedOperatorDot (
    (M..|.)
) where

import qualified Data.Bits as M

(.|.) :: Int -> Int -> Int
x .|. y = x
