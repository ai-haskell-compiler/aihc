module UniqueLogic.ST.Example.Label
{-# DEPRECATED "This module is intended for documentation purposes. Do not import it!" #-}
 where

import qualified UniqueLogic.ST.Example.Term as Term
import qualified UniqueLogic.ST.Expression as Expr
import qualified UniqueLogic.ST.Rule as Rule
import qualified UniqueLogic.ST.System.Label as Sys
import qualified UniqueLogic.ST.Duplicate as Duplicate
import UniqueLogic.ST.Expression ((=:=))

import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Trans.Writer (writer, )
import Control.Monad.ST (ST, runST, )
import Control.Monad (liftM2, liftM3, )

import qualified Prelude as P
import Prelude hiding (max, log)



data Assign = Assign Term.Name Term.T
   deriving (Show)

type Assigns = [Assign]

type Variable s = Sys.Variable Assigns s (Duplicate.Ignore Term.T)

globalVariable :: Term.Name -> ST s (Variable s)
globalVariable name =
   Sys.globalVariable $
      \(Duplicate.Ignore x) ->
         writer (Duplicate.Ignore $ Term.Var name, [Assign name x])

constant :: Rational -> Sys.T Assigns s (Variable s)
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
            (Sys.queryIgnore x)
            (Sys.queryIgnore y)
            (Sys.queryIgnore z))

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
            (Sys.queryIgnore xv)
            (Sys.queryIgnore yv))
