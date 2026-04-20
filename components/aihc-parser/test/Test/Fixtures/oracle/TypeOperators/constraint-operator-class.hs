{- ORACLE_TEST xfail parser rejects multi-parameter constraint operator in class head -}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module ConstraintOperatorClass where

class (c1 a, c2 a) => (:&&:) c1 c2 a
