{-# LANGUAGE OverloadedStrings #-}

module NonTerm where

import Arith as A
import DSL
import Language.REST.Op
import Language.REST.MetaTerm
import Language.REST.Internal.Rewrite
import qualified Data.HashSet as S

a', b', c', d' :: MetaTerm -> MetaTerm
a' x1 = RWApp (Op "a") [x1]
b' x1 = RWApp (Op "b") [x1]
c' x1 = RWApp (Op "c") [x1]
d' x1 = RWApp (Op "d") [x1]

userRWs :: S.HashSet Rewrite
userRWs = S.fromList
  [
    a' (b' x) ~> a' (d' x)
  , d' (b' x) ~> b' (d' x)
  , b' (d' x) ~> d' (b' x)
  , d' (b' x) ~> b' (b' (b' x))
  ]

evalRWs :: S.HashSet Rewrite
evalRWs = A.evalRWs
