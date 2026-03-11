{-# LANGUAGE OverloadedStrings #-}

module KBO where

import Language.REST.OCAlgebra
import Language.REST.KBO
import Language.REST.SMT
import Language.REST.RuntimeTerm
import Nat () -- IsString RuntimeTerm

tests :: SolverHandle -> [(String, IO Bool)]
tests solver = testList where

  gte :: RuntimeTerm ->  RuntimeTerm -> IO Bool
  gte t u = isSat (kbo solver) (kboGTE t u)

  testList =
    [ ("Comm"  , gte "f(a, b)" "f(b, a)")
    , ("Assoc" , gte "f(a, f(b, c))"  "f((a b), c)")
    , ("Dist"  , not <$> gte "f(a, g(b, c))" "g(f(a, b), f(a, c))")
    ]
