{- ORACLE_TEST pass -}

module NestedParenthesizedOperatorAsPattern where

data C = C

fn (+)@(+)@C = ()
