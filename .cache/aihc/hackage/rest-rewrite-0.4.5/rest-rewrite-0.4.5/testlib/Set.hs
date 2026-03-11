{-# LANGUAGE OverloadedStrings #-}

module Set where

import Arith as A
import DSL
import Language.REST.Internal.Rewrite (Rewrite)
import Language.REST.MetaTerm

import qualified Data.HashSet as S

emptyset :: MetaTerm
emptyset  = RWApp "∅" []

(/\), (\/) :: MetaTerm -> MetaTerm -> MetaTerm
x1 /\ y1 = RWApp "intersect" [x1, y1]
x1 \/ y1  = RWApp "union" [x1, y1]

s0, s1 :: MetaTerm
s0 = RWApp "s₀" []
s1 = RWApp "s₁" []

isSubset :: MetaTerm -> MetaTerm -> Rewrite
isSubset mt1 mt2 = mt1 \/ mt2 ~> mt2

userRWs :: S.HashSet Rewrite
userRWs = S.union A.evalRWs $ S.fromList
  [
    distribL (/\) (\/)
  , distribR (/\) (\/)
  , distribL (\/) (/\)
  , distribR (\/) (/\)
  , assocL (\/)
  , assocL (/\)
  , x /\ x        ~> x
  , x \/ x        ~> x
  , x \/ emptyset ~> x
  , commutes (\/)
  , commutes (/\)

  -- Example 1
  -- , s1 /\ s0      ~> emptyset

  -- Example 2
  , s0 \/ s1      ~> s0
  ]

evalRWs :: S.HashSet Rewrite
evalRWs = S.union A.userRWs $ S.fromList --
  [ RWApp "t2" [] ~> emptyset
  , isSubset (RWApp "right1" []) (RWApp "right" [])
  ]

disjointExample, disjointExample2 :: String
disjointExample  = "union(union(left, right1), union(left,right))"
disjointExample2 = "union(left, union(right1, union(left,right)))"

example1, example2 :: String
example1 = "f(intersect(union(s₀,s₁), s₀))"
example2 = "f(union(intersect(s₀,s₁), s₀))"
