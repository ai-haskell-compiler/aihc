{- ORACLE_TEST
id: flexible-instance-empty-where
category: FlexibleInstances
expected: pass
reason: parser supports empty flexible instances with where clause
-}
{-# LANGUAGE FlexibleInstances #-}

module FlexibleInstanceEmptyWhere where

class Unconstrained2 a

instance Unconstrained2 a where
