{- |
Module      :  Control.Monad.Trans.Indexed
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

Indexed monad transformers.
-}

module Control.Monad.Trans.Indexed
  ( IxMonadTrans (..)
  , Indexed (..)
  , (&)
  ) where

import Control.Category (Category (..))
import Control.Monad
import Control.Monad.Trans
import Data.Function ((&))
import Data.Kind
import Prelude hiding (id, (.))

{- |
An [Atkey indexed monad]
(https://bentnib.org/paramnotions-jfp.pdf)
is a `Functor` [enriched category]
(https://ncatlab.org/nlab/show/enriched+category).
An indexed monad transformer transforms a `Monad` into an indexed monad.
It is a monad and monad transformer when its source and target index
are the same, enabling use of standard @do@ notation in that case.
In the general case, qualified @Indexed.do@ notation can be used,
even if the source and target index are different.

>>> :set -XQualifiedDo
>>> import qualified Control.Monad.Trans.Indexed.Do as Indexed
-}
type IxMonadTrans
  :: (k -> k -> (Type -> Type) -> Type -> Type)
  -> Constraint
class
  ( forall i j m. Monad m => Functor (t i j m)
  , forall i j m. (i ~ j, Monad m) => Monad (t i j m)
  , forall i j. i ~ j => MonadTrans (t i j)
  ) => IxMonadTrans t where

  {-# MINIMAL joinIx | bindIx #-}

  {- |
  indexed analog of `<*>`

  prop> (<*>) = apIx
  -}
  apIx
    :: Monad m
    => t i j m (x -> y)
    -> t j k m x
    -> t i k m y
  apIx tf tx = bindIx (<$> tx) tf

  {- |
  indexed analog of `join`

  prop> join = joinIx
  prop> joinIx = bindIx id
  -}
  joinIx
    :: Monad m
    => t i j m (t j k m y)
    -> t i k m y
  joinIx = bindIx id

  {- |
  indexed analog of `=<<`

  prop> (=<<) = bindIx
  prop> bindIx f x = joinIx (f <$> x)
  prop> x & bindIx return = x
  prop> x & bindIx f & bindIx g = x & bindIx (f & andThenIx g)
  -}
  bindIx
    :: Monad m
    => (x -> t j k m y)
    -> t i j m x
    -> t i k m y
  bindIx f t = joinIx (f <$> t)

  {- |
  indexed analog of flipped `>>`

  prop> (>>) = flip thenIx
  prop> return () & thenIx y = y
  -}
  thenIx
    :: Monad m
    => t j k m y
    -> t i j m x
    -> t i k m y
  thenIx ix2 ix1 = ix1 & bindIx (\ _ -> ix2)

  {- |
  indexed analog of `<=<`

  prop> (<=<) = andThenIx
  prop> andThenIx g f x = bindIx g (f x)
  prop> f & andThenIx return = f
  prop> return & andThenIx f = f
  prop> f & andThenIx g & andThenIx h = f & andThenIx (g & andThenIx h)
  -}
  andThenIx
    :: Monad m
    => (y -> t j k m z)
    -> (x -> t i j m y)
    -> x -> t i k m z
  andThenIx g f x = bindIx g (f x)

{- |
`Indexed` reshuffles the type parameters of an `IxMonadTrans`,
exposing its `Category` instance.
-}
newtype Indexed t m r i j = Indexed {runIndexed :: t i j m r}
instance
  ( IxMonadTrans t
  , Monad m
  , Monoid r
  ) => Category (Indexed t m r) where
    id = Indexed (pure mempty)
    Indexed g . Indexed f = Indexed $ apIx (fmap (<>) f) g
