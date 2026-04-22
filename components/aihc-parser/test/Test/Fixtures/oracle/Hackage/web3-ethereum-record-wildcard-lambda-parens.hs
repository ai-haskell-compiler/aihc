{- ORACLE_TEST pass -}
{-# LANGUAGE RecordWildCards #-}
module Web3EthereumRecordWildcardLambdaParens where
data T = T {x :: Int}
f = \T{..} -> x
