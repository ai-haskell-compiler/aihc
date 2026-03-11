{-# LANGUAGE OverloadedStrings #-}
module Lists where

import Prelude hiding (reverse)

import           Language.REST.Internal.Rewrite (Rewrite)
import qualified Language.REST.MetaTerm as MT
import           DSL hiding (t1, t2)

import qualified Data.HashSet as S

xs, ys, nil :: MT.MetaTerm
xs = MT.Var "xs"
ys = MT.Var "ys"
nil = MT.RWApp "nil"     []

(.:) :: MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm
(.:) t1 t2    = MT.RWApp "cons"    [t1, t2]

reverse :: MT.MetaTerm -> MT.MetaTerm
reverse t     = MT.RWApp "reverse" [t]

(.++) :: MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm
(.++) lhs rhs = MT.RWApp "append"  [lhs, rhs]


evalRWs :: S.HashSet Rewrite
evalRWs = S.fromList [
    reverse (x .: xs) ~> reverse xs .++ (x .: nil)
  , reverse nil ~> nil
  , nil .++ xs          ~> xs
  , (x .: xs) .++ ys    ~> x .: (xs .++ ys)
  , reverse (reverse (MT.RWApp "xs" [])) ~> MT.RWApp "xs" []
  ]

userRWs :: S.HashSet Rewrite
userRWs = S.fromList [
    reverse (xs .++ ys) ~> reverse ys .++ reverse xs
  , reverse ys .++ reverse xs ~> reverse (xs .++ ys)
  ]
