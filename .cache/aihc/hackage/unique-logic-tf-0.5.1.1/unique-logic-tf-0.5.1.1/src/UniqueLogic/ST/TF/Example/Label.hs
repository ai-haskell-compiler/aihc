module UniqueLogic.ST.TF.Example.Label
{-# WARNING "This module is intended for documentation purposes. Do not import it!" #-}
 where

import qualified UniqueLogic.ST.TF.Example.Term as Term
import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.Rule as Rule
import qualified UniqueLogic.ST.TF.System.Label as Sys
import UniqueLogic.ST.TF.Expression ((=:=))

import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Trans.Writer (writer, )
import Control.Monad.ST (runST, )
import Control.Monad (liftM2, liftM3, )

import qualified Data.Ref as Ref

import Prelude hiding (max, log)



data Assign = Assign Term.Name Term.T
   deriving (Show)

type Assigns = [Assign]

type Variable s = Sys.Variable Assigns s Term.T

globalVariable :: (Ref.C s) => Term.Name -> s (Variable s)
globalVariable name =
   Sys.globalVariable $
      \x -> writer (Term.Var name, [Assign name x])

constant :: (Ref.C s) => Rational -> Sys.T Assigns s (Variable s)
constant = Sys.constant . fromRational


{- |
> x=1
> y=2
> z=3

> x+y=3
> y*z=6
> z=3
-}
rule :: ((Maybe Term.T, Maybe Term.T, Maybe Term.T), Assigns)
rule =
   runST (do
      x <- globalVariable "x"
      y <- globalVariable "y"
      z <- globalVariable "z"
      MW.runWriterT $ do
         Sys.solve $ do
            c3 <- constant 3
            c6 <- constant 6
            Rule.add x y c3
            Rule.mul y z c6
            Rule.equ z c3
         MT.lift $ liftM3
            (,,)
            (Sys.query x)
            (Sys.query y)
            (Sys.query z))

expression :: ((Maybe Term.T, Maybe Term.T), Assigns)
expression =
   runST (do
      xv <- globalVariable "x"
      yv <- globalVariable "y"
      MW.runWriterT $ do
         Sys.solve $ do
            let x = Expr.fromVariable xv
                y = Expr.fromVariable yv
            x*3 =:= y/2
            5 =:= 2+x
         MT.lift $ liftM2 (,)
            (Sys.query xv)
            (Sys.query yv))
