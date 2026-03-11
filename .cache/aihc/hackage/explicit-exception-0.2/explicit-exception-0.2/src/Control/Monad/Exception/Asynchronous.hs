{- |
Asynchronous exceptions can occur during the construction of a lazy data structure.
They are represented by a lazy data structure itself.

This module re-exports the type with lazy combinators.


TODO:

* Is the Null type appropriate anywhere?
  Should it be better a Monoid type with mempty?
  Shall Monoid.mempty be the default, or functions with explicit default values?

* Shall we replace Monad constraint by Functor constraint,
  where we only need liftM?
-}
module Control.Monad.Exception.Asynchronous (
   module Control.Monad.Exception.Asynchronous.Lazy
   ) where

import Control.Monad.Exception.Asynchronous.Lazy
