{-# LANGUAGE OverloadedStrings #-}

module DSL where

import           Language.REST.Op
import qualified Language.REST.MetaTerm as MT
import           Language.REST.RuntimeTerm as RT
import           Language.REST.Internal.Rewrite
import           Nat

commutes, assocL, assocR :: (MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm) -> Rewrite
commutes op      = x `op` y            ~> y `op` x
assocL   op      = (x `op` y) `op` z'   ~> x `op` (y `op` z')
assocR   op      = x `op` (y `op` z')   ~> (x `op` y) `op` z'

distribL, distribR
  :: (MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm)
  -> (MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm)
  -> Rewrite
distribL op1 op2 = (x `op1` y) `op2` z' ~> (x `op2` z') `op1` (y `op2` z')
distribR op1 op2 = z' `op2` (x `op1` y) ~> (z' `op2` x) `op1` (z' `op2` y)

ackOp, plus, minus, times :: Op
ackOp  = Op "ack"
plus   = Op "+"
minus  = Op "-"
times  = Op "*"

a, b, c, d :: RuntimeTerm
a = App (Op "a") []
b = App (Op "b") []
c = App (Op "c") []
d = App (Op "d") []

x, y, v, w, z' :: MT.MetaTerm
x = MT.Var "X"
y = MT.Var "Y"
v = MT.Var "V"
w = MT.Var "W"
z' = MT.Var "Z"

f, g, h :: Op
f = Op "f"
g = Op "g"
h = Op "h"

t1Op, t2Op :: Op
t1Op = Op "t1"
t2Op = Op "t2"

t1, t2, t3, t4, t5 :: RuntimeTerm
t1 = App (Op "t1") []
t2 = App (Op "t2") []
t3 = App (Op "t3") []
t4 = App (Op "t4") []
t5 = App (Op "t5") []

zero, one, two :: RuntimeTerm
zero    = App z []
one     = suc zero
two     = suc one

suc :: RuntimeTerm -> RuntimeTerm
suc x1   = App s [x1]

ack :: RuntimeTerm -> RuntimeTerm -> RuntimeTerm
ack x1 y1 = App ackOp [x1, y1]

zero' :: MT.MetaTerm
zero'    = MT.toMetaTerm zero

one' :: MT.MetaTerm
one'     = suc' zero'

suc' :: MT.MetaTerm -> MT.MetaTerm
suc' x1   = MT.RWApp s [x1]

ack' :: MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm
ack' x1 y1 = MT.RWApp ackOp [x1, y1]

infixl 1 .+
(.+) :: RuntimeTerm -> RuntimeTerm -> RuntimeTerm
(.+) x1 y1 = App plus [x1, y1]

(#+) :: MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm
(#+) x1 y1 = MT.RWApp plus [x1, y1]

(#-) :: MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm
(#-) x1 y1 = MT.RWApp minus [x1, y1]

(#*) :: MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm
(#*) x1 y1 = MT.RWApp times [x1, y1]

infix 0 ~>
(~>) :: MT.MetaTerm -> MT.MetaTerm -> Rewrite
t ~> u = Rewrite t u Nothing

infix 0 <~>
(<~>) :: MT.MetaTerm -> MT.MetaTerm -> [Rewrite]
t <~> u = [ t ~> u, u ~> t ]
