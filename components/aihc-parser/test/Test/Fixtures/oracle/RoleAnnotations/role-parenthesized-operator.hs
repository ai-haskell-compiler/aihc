{- ORACLE_TEST pass -}
{-# LANGUAGE RoleAnnotations #-}

module RoleParenthesizedOperator where

data a := b = Pair a b

type role (:=) nominal nominal
