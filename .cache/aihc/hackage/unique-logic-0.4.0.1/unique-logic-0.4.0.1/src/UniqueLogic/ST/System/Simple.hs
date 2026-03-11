module UniqueLogic.ST.System.Simple (
   -- * Preparation
   Variable,
   globalVariable,
   -- * Posing statements
   T,
   localVariable,
   constant,
   Sys.assignment2,
   Sys.assignment3,
   Sys.Apply, Sys.arg, Sys.runApply,
   -- * Solution
   solve,
   query,
   ) where

import qualified UniqueLogic.ST.System as Sys
import qualified UniqueLogic.ST.Duplicate as Duplicate

import Control.Monad.Trans.Identity (IdentityT, runIdentityT, )
import Control.Monad.ST (ST, )


type T = Sys.T IdentityT

type Variable s a = Sys.Variable IdentityT s (Duplicate.Ignore a)


globalVariable :: ST s (Variable s a)
globalVariable =
   Sys.globalVariable Sys.simpleUpdate

localVariable :: T s (Variable s a)
localVariable = Sys.localVariable

constant :: a -> T s (Variable s a)
constant = Sys.constant . Duplicate.Ignore

solve :: T s a -> ST s a
solve = runIdentityT . Sys.solve

query :: Variable s a -> ST s (Maybe a)
query = fmap (fmap (\(Duplicate.Ignore a) -> a)) . Sys.query
