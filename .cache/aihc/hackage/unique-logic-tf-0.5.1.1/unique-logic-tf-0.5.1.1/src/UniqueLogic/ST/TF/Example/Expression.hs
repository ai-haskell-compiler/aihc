module UniqueLogic.ST.TF.Example.Expression
{-# WARNING "This module is intended for documentation purposes. Do not import it!" #-}
 where

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System.Simple as Sys
import UniqueLogic.ST.TF.Expression ((=:=))

import Control.Monad.ST (runST, )
import Control.Monad (liftM2, )


example :: (Maybe Double, Maybe Double)
example =
   runST (do
      xv <- Sys.globalVariable
      yv <- Sys.globalVariable
      Sys.solve $ do
         let x = Expr.fromVariable xv
             y = Expr.fromVariable yv
         x*3 =:= y/2
         5 =:= 2+x
      liftM2
         (,)
         (Sys.query xv)
         (Sys.query yv))

exampleZero :: (Maybe Double, Maybe Double)
exampleZero =
   runST (do
      xv <- Sys.globalVariable
      yv <- Sys.globalVariable
      Sys.solve $ do
         let x = Expr.fromVariable xv
             y = Expr.fromVariable yv
         x*0 =:= 0
         5+0*y+x*0 =:= 2+x
      liftM2
         (,)
         (Sys.query xv)
         (Sys.query yv))
