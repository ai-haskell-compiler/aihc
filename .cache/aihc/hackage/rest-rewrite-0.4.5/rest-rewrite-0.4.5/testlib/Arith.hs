{-# LANGUAGE OverloadedStrings #-}
module Arith where

import DSL
import Language.REST.Internal.Rewrite (Rewrite)
import Language.REST.MetaTerm
import Language.REST.Op

import qualified Data.HashSet as S

neg :: MetaTerm -> MetaTerm
neg x1 = RWApp (Op "neg") [x1]

double :: MetaTerm -> MetaTerm
double x1 = RWApp (Op "double") [x1]

twicePlus :: MetaTerm -> MetaTerm -> MetaTerm
twicePlus x1 y1 = RWApp (Op "twicePlus") [x1, y1]

(<#) :: MetaTerm -> MetaTerm -> MetaTerm
x1 <# y1 = RWApp (Op "<") [x1, y1]

evalRWs :: S.HashSet Rewrite
evalRWs =
    S.fromList
      [
        suc' x <# suc' y ~> x <# y
      , suc' x #+ y ~> suc' (x #+ y)
      , zero'    #+ x ~> x

      , suc' x #* y ~> y #+ (x #* y)
      , zero'     #* y ~> zero'

      , ack' zero' x           ~> suc' x
      , ack' (suc' x) zero'    ~> ack' x one'
      , ack' (suc' x) (suc' y) ~> ack' x (ack' (suc' x) y)
      , double x               ~> x #+ x
      , twicePlus x y          ~> (x #+ x) #+ y
      ]

userRWs :: S.HashSet Rewrite
userRWs =
    S.fromList $
      [ x #+ y        ~> y #+ x

      , x #* y        ~> y #* x

      , (x #+ y) #* v ~> (x #* v) #+ (y #* v)
      , neg x #+ x ~> zero'
      -- , (x #* v) #+ (y #* v) ~> (x #+ y) #* v

      --  , x ~> x #+ zero'
      ] ++ [ x #+ (y #+ v) ~> (x #+ y) #+ v]
