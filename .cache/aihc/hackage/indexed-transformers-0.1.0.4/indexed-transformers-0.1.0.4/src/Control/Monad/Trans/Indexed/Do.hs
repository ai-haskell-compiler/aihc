{- |
Module      :  Control.Monad.Trans.Indexed.Do
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

Qualified @Indexed.do@ notation.

>>> :set -XQualifiedDo
>>> import qualified Control.Monad.Trans.Indexed.Do as Indexed
-}

module Control.Monad.Trans.Indexed.Do
  ( (>>=)
  , (>>)
  , return
  , fail
  ) where

import qualified Control.Monad as M
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Trans.Indexed as Ix
import Prelude hiding ((>>=), (>>), fail)

(>>=)
  :: (Ix.IxMonadTrans t, M.Monad m)
  => t i j m x
  -> (x -> t j k m y)
  -> t i k m y
(>>=) = flip Ix.bindIx

(>>)
  :: (Ix.IxMonadTrans t, M.Monad m)
  => t i j m x
  -> t j k m y
  -> t i k m y
(>>) = flip Ix.thenIx

fail
  :: (Ix.IxMonadTrans t, M.MonadFail m, i ~ j)
  => String
  -> t i j m x
fail = T.lift . M.fail
