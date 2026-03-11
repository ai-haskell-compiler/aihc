{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_base(4,4,0)
{-# LANGUAGE Safe #-}
#endif

#if MIN_VERSION_transformers(0,4,0)
-- Hide warnings for the deprecated ErrorT transformer:
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif

module Control.Monad.Base
  ( MonadBase(..)
  , liftBaseDefault
  ) where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.Writer.CPS as C
import qualified Control.Monad.Trans.RWS.CPS as C
#endif
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.List
import Control.Monad.Trans.Error
#endif
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Select
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Control.Applicative (Applicative(..))
#endif
#if !MIN_VERSION_base(4,4,0) && HS_TRANSFORMERS_BASE__ORPHANS
import qualified Control.Monad.ST.Lazy as L
import qualified Control.Monad.ST.Strict as S
import Data.Orphans ()
#endif
#if MIN_VERSION_base(4,4,0)
import qualified Control.Monad.ST.Lazy.Safe as L
import qualified Control.Monad.ST.Safe as S
#endif
import Control.Monad.STM (STM)

class (Applicative b, Applicative m, Monad b, Monad m)
      ⇒ MonadBase b m | m → b where
  -- | Lift a computation from the base monad
  liftBase ∷ b α → m α

#define BASE(M) \
instance MonadBase (M) (M) where liftBase = id

BASE(IO)
BASE(Maybe)
BASE(Either e)
BASE([])
BASE((→) r)
BASE(Identity)

BASE(STM)

#if !MIN_VERSION_base(4,4,0) && HS_TRANSFORMERS_BASE__ORPHANS
BASE(L.ST s)
BASE(S.ST s)
#endif

#if MIN_VERSION_base(4,4,0)
BASE(L.ST s)
BASE(S.ST s)
#endif

#undef BASE

-- | Can be used as a default implementation for 'liftBase'.
--
-- Note that: @liftBaseDefault = 'lift' . 'liftBase'@
liftBaseDefault ∷ (MonadTrans t, MonadBase b m) ⇒ b α → t m α
liftBaseDefault = lift . liftBase

#define TRANS(T) \
instance (MonadBase b m) ⇒ MonadBase b (T m) where liftBase = liftBaseDefault

TRANS(IdentityT)
TRANS(MaybeT)
TRANS(ReaderT r)
TRANS(L.StateT s)
TRANS(S.StateT s)
TRANS(ContT r)
TRANS(ExceptT e)
TRANS(SelectT r)
#if !MIN_VERSION_transformers(0,6,0)
TRANS(ListT)
#endif
#undef TRANS

#define TRANS_CTX(CTX, T) \
instance (CTX, MonadBase b m) ⇒ MonadBase b (T m) where liftBase = liftBaseDefault

TRANS_CTX(Monoid w, L.WriterT w)
TRANS_CTX(Monoid w, S.WriterT w)
TRANS_CTX(Monoid w, L.RWST r w s)
TRANS_CTX(Monoid w, S.RWST r w s)
#if MIN_VERSION_transformers(0,5,6)
TRANS_CTX(Monoid w, C.WriterT w)
TRANS_CTX(Monoid w, C.RWST r w s)
#endif
#if !MIN_VERSION_transformers(0,6,0)
TRANS_CTX(Error e,  ErrorT e)
#endif
TRANS_CTX(Monoid w, AccumT w)
#undef TRANS_CTX
