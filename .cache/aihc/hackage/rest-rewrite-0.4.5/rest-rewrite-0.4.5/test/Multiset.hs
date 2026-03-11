{-# LANGUAGE OverloadedStrings #-}

module Multiset where

import Data.Text ()
import DSL hiding (a, b, c)
import Language.REST.MetaTerm
import Language.REST.Internal.Rewrite
import qualified Data.HashSet as S

(\/) :: MetaTerm -> MetaTerm -> MetaTerm
x1 \/ y1  = RWApp "union" [x1, y1]

singleton, multisetOf :: MetaTerm -> MetaTerm
singleton x1 = RWApp "m" [x1]
multisetOf x1 = RWApp "toMS" [x1]

cons :: MetaTerm -> MetaTerm -> MetaTerm
cons x1 y1 = RWApp "cons" [x1, y1]

ite :: MetaTerm -> MetaTerm -> MetaTerm -> MetaTerm
ite a b c = RWApp "ite" [a,b,c]

hd, tl, isEmpty :: MetaTerm -> MetaTerm
hd x1 = RWApp "head" [x1]
tl x1 = RWApp "tail" [x1]
isEmpty x1 = RWApp "isEmpty" [x1]

empty :: MetaTerm
empty = RWApp "empty" []

xs, ys :: MetaTerm
xs = RWApp "xs" []
ys = RWApp "ys" []

expandM :: MetaTerm -> Rewrite
expandM xs0 = multisetOf xs0 ~> ite (isEmpty xs0) empty (singleton (hd xs0) \/ multisetOf (tl xs0))

userRWs :: S.HashSet Rewrite
userRWs = S.fromList
  [
    commutes (\/) `named` "mpComm"
  , assocL (\/) `named` "mpAssoc"
  , assocR (\/) `named` "mpAssoc"
  , singleton x \/ multisetOf y ~> multisetOf (cons x y)
  ]

evalRWs :: S.HashSet Rewrite
evalRWs = S.fromList
  [ multisetOf (cons x y) ~> singleton x \/ multisetOf y
  , expandM xs
  , expandM ys
  ]

perm1t, perm1s :: String
perm1s = "union(m(y), union(toMS(cons(a,xs)),toMS(ys)))"
perm1t = "union(m(a), union(toMS(xs), toMS(cons(y,ys))))"
