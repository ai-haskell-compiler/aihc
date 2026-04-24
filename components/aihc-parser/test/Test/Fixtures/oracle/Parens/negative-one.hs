{- ORACLE_TEST xfail "we add parens around negative one when we shouldn't" -}

module M where

svDecrement x = svAddConstant x (-1 :: Integer)
