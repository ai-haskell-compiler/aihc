{- ORACLE_TEST pass -}
{-# LANGUAGE OverloadedRecordDot #-}

data Node = Node {as :: Int}

f :: Node
f = Node {as = 1}
