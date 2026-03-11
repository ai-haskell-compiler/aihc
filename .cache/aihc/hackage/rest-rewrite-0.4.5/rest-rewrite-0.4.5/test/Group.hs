{-# LANGUAGE OverloadedStrings #-}

module Group where

import DSL
import Language.REST.Internal.Rewrite (Rewrite)
import Language.REST.Op
import Language.REST.MetaTerm

import qualified Data.HashSet as S

neg :: MetaTerm -> MetaTerm
neg x1 = RWApp (Op "neg") [x1]

evalRWs :: S.HashSet Rewrite
evalRWs = S.empty

userRWs :: S.HashSet Rewrite
userRWs =
    S.fromList
      [
          x #+ zero'    ~> x
        , zero'    #+ x ~> x
        , neg x #+ x  ~> zero'
        , (x #+ y) #+ v ~> x #+ (y #+ v)
      ]
