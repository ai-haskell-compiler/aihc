{- ORACLE_TEST
id: flexible-instance-empty-no-where
category: FlexibleInstances
expected: pass
reason: parser supports empty flexible instances without where clause
-}
{-# LANGUAGE FlexibleInstances #-}

module FlexibleInstanceEmptyNoWhere where

class Unconstrained1 a

instance Unconstrained1 a
