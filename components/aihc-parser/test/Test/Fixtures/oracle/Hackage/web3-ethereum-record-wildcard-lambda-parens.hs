{- ORACLE_TEST xfail record wildcard pattern gets spurious parens in lambda -}
{-# LANGUAGE RecordWildCards #-}
module Web3EthereumRecordWildcardLambdaParens where
data T = T {x :: Int}
f = \T{..} -> x
