{- |
We have a set of variables and equality relations between them.
We want to assign a distinct number to each group of equal variables.
Eventually every variable shall carry its group identifier.

We solve this by an incremental solution.
For every variable we query its identifier.
If it is still unset we discovered a new group.
In this case we generate a new identifier
and solve the system (again) with an additional
equality constraint with respect to the new identifier.

In general it is not a good idea to solve a system incrementally,
because earlier assignments cannot trigger later added assignments.
In this example it works because we add assignments from constants,
that is, we add only new trigger seeds.
-}
module UniqueLogic.ST.TF.Example.Incremental
{-# WARNING "This module is intended for documentation purposes. Do not import it!" #-}
 where

import qualified UniqueLogic.ST.TF.Rule as Rule
import qualified UniqueLogic.ST.TF.System.Simple as Sys

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import Control.Monad.ST (ST, runST, )


{- |
> a=c
> d=f
> c=e
> b

Groups: {a,c,e}, {d,f}, {b}

> Incremental> example
> [0,1,0,2,0,2]
-}
example :: [Int]
example =
   runST (do
      a <- Sys.globalVariable
      b <- Sys.globalVariable
      c <- Sys.globalVariable
      d <- Sys.globalVariable
      e <- Sys.globalVariable
      f <- Sys.globalVariable
      Sys.solve $ do
         Rule.equ a c
         Rule.equ d f
         Rule.equ c e
      MS.evalStateT (mapM incQuery [a,b,c,d,e,f]) 0)

incQuery :: Sys.Variable (ST s) Int -> MS.StateT Int (ST s) Int
incQuery v = do
   mk <- MT.lift $ Sys.query v
   case mk of
      Just k -> return k
      Nothing -> do
         k <- MS.get
         MS.put (k+1)
         MT.lift $ Sys.solve $ Rule.equ v =<< Sys.constant k
         return k
