{- ORACLE_TEST xfail parser does not accept qualified module prefix in export list -}
{-# LANGUAGE GHC2021 #-}

module ExportQualifiedOperator (
    (M..&.)
) where

import qualified Data.Bits as M
