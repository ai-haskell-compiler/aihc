{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module ConstraintOperatorClass where

class (c1 a, c2 a) => (:&&:) c1 c2 a
