module Control.Monad.HReader.Class
       ( MonadHReader(..)
       , MHRElemsConstraint
       , hask
       , haskTagged
       ) where


import Control.Monad.Cont
#if !(MIN_VERSION_mtl(2, 3, 0))
import Control.Monad.List
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.HSet
import Data.Tagged
import GHC.Exts

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#endif

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
import Data.Monoid
#endif

import qualified Control.Monad.RWS.Lazy      as RWSL
import qualified Control.Monad.RWS.Strict    as RWSS
import qualified Control.Monad.State.Lazy    as SL
import qualified Control.Monad.State.Strict  as SS
import qualified Control.Monad.Trans.Cont    as Cont
import qualified Control.Monad.Writer.Lazy   as WL
import qualified Control.Monad.Writer.Strict as WS

-- | Easy generate constraint like
-- __(HGettable (MHRElements m) Int, HGettable (MHRElements m) Bool)__
-- from type list __[Int, Bool]__. Usable to reuse type lists for
-- constraints and concrete HSet.
type family MHRElemsConstraint (m :: * -> *) (els :: [*]) :: Constraint where
  MHRElemsConstraint m '[] = (MonadHReader m)
  MHRElemsConstraint m (e ': els) = (HGettable (MHRElements m) e, MHRElemsConstraint m els)

-- | Monad which is a reader of HSet (or just can construct it).
class (Monad m, Applicative m) => MonadHReader m where
  type MHRElements m :: [*]
  askHSet :: m (HSet (MHRElements m))
  hlocal :: (HSet (MHRElements m) -> HSet (MHRElements m)) -> m a -> m a


instance (MonadHReader m) => MonadHReader (ContT r m) where
  type MHRElements (ContT r m) = MHRElements m
  askHSet = lift askHSet
  hlocal = Cont.liftLocal askHSet hlocal

#define MHR(MONAD)                                        \
instance (MonadHReader m) => MonadHReader (MONAD) where { \
  type MHRElements (MONAD) = MHRElements m ;            \
  askHSet = lift askHSet ;                                \
  hlocal f ma = liftThrough (hlocal f) ma  ; \
  }

MHR(IdentityT m)
MHR(ReaderT r m)
#if !(MIN_VERSION_mtl(2, 3, 0))
MHR(ListT m)
#endif
MHR(MaybeT m)

#if MIN_VERSION_mtl(2, 2, 1)
MHR(ExceptT e m)
#endif

MHR(SL.StateT s m)
MHR(SS.StateT s m)


#define MHRWRITER(MONAD)                                        \
instance (MonadHReader m, Monoid w) => MonadHReader (MONAD) where { \
  type MHRElements (MONAD) = MHRElements m ;            \
  askHSet = lift askHSet ;                                \
  hlocal f ma = liftThrough (hlocal f) ma  ; \
  }

MHRWRITER(WL.WriterT w m)
MHRWRITER(WS.WriterT w m)
MHRWRITER(RWSL.RWST r w s m)
MHRWRITER(RWSS.RWST r w s m)

-- | Ask arbitrary element of hset inside HReader
hask :: (MonadHReader m, HGettable (MHRElements m) e)
      => m e
hask = hget <$> askHSet

-- | Ask arbitrary labeled element of hset in HReader
haskTagged :: (MonadHReader m, HGettable (MHRElements m) (Tagged tag e))
           => proxy tag
           -> m e
haskTagged p = hgetTagged p <$> askHSet
