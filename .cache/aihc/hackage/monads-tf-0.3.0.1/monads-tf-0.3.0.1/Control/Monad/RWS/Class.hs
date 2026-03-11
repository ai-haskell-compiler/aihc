
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.RWS.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- Declaration of the MonadRWS class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.RWS.Class (
    MonadRWS,
    module Control.Monad.Reader.Class,
    module Control.Monad.State.Class,
    module Control.Monad.Writer.Class,
  ) where

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Control.Monad.Trans.Except(ExceptT)
import Control.Monad.Trans.Maybe(MaybeT)
import Control.Monad.Trans.Identity(IdentityT)
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)

class (Monoid (WriterType m), MonadReader m, MonadWriter m, MonadState m) =>
    MonadRWS m

instance (Monoid w, Monad m) => MonadRWS (Lazy.RWST r w s m)

instance (Monoid w, Monad m) => MonadRWS (Strict.RWST r w s m)

---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (MonadRWS m) => MonadRWS (ExceptT e m)

instance (MonadRWS m) => MonadRWS (IdentityT m)

instance (MonadRWS m) => MonadRWS (MaybeT m)
