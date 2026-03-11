module UniqueLogic.ST.TF.System.Simple (
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

import qualified UniqueLogic.ST.TF.System as Sys
import qualified Data.Ref as Ref

import Control.Monad.Trans.Identity (IdentityT, runIdentityT, )


type T = Sys.T IdentityT

type Variable s a = Sys.Variable IdentityT s a


globalVariable :: (Ref.C s) => s (Variable s a)
globalVariable =
   Sys.globalVariable Sys.simpleUpdate

localVariable :: (Ref.C s) => T s (Variable s a)
localVariable = Sys.localVariable

constant :: (Ref.C s) => a -> T s (Variable s a)
constant = Sys.constant

solve :: (Ref.C s) => T s a -> s a
solve = runIdentityT . Sys.solve

query :: (Ref.C s) => Variable s a -> s (Maybe a)
query = Sys.query
