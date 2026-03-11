module UniqueLogic.ST.TF.Example.Rule
{-# WARNING "This module is intended for documentation purposes. Do not import it!" #-}
 where

import qualified UniqueLogic.ST.TF.Rule as Rule
import qualified UniqueLogic.ST.TF.System.Simple as Sys

import Control.Monad.ST (runST, )
import Control.Monad (liftM4, )

import Prelude hiding (max)


{- |
> x=1
> y=2
> z=3
> w=3

> x+y=3
> y*z=6
> z=3
> y^w=8
-}
example :: (Maybe Double, Maybe Double, Maybe Double, Maybe Double)
example =
   runST (do
      x <- Sys.globalVariable
      y <- Sys.globalVariable
      z <- Sys.globalVariable
      w <- Sys.globalVariable
      Sys.solve $ do
         c3 <- Sys.constant 3
         c6 <- Sys.constant 6
         c8 <- Sys.constant 8
         Rule.add x y c3
         Rule.mul y z c6
         Rule.equ z c3
         Rule.pow y w c8
      liftM4
         (,,,)
         (Sys.query x)
         (Sys.query y)
         (Sys.query z)
         (Sys.query w))
