{- ORACLE_TEST xfail constructor application pattern gets spurious parens in infix position -}
module Web3EthereumConpatInfixParens where
f (Just _ : _) = ()
