{- ORACLE_TEST xfail reserved keyword 'as' used as record field name in construction -}
{-# LANGUAGE OverloadedRecordDot #-}

data Node = Node { as :: Int }

f :: Node
f = Node { as = 1 }
