{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module ExportQualifiedOperatorMultiModule (
    (Data.Bits..&.)
) where

import qualified Data.Bits
