{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  Language.Haskell.TH.Instances
-- Copyright   :  (c) Matt Morrow 2008
-- License     :  BSD3
-- Maintainer  :  Michael Sloan <mgsloan at gmail>
-- Stability   :  experimental
-- Portability :  portable (template-haskell)
--
-- It provides the following instances:
--
--   * 'Lift' instances for the datatypes in "Language.Haskell.TH"
--
--   * 'Quasi' for 'ReaderT', 'WriterT', 'StateT', and 'RWST'.
--
--   * 'MonadFix' for 'Q'
--
-- More recent versions of template-haskell provide these instances. However, in
-- order to support older versions you should import this module.
module Language.Haskell.TH.Instances () where

import Language.Haskell.TH hiding (newName)
import Language.Haskell.TH.Instances.Internal
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.ReifyMany
import Language.Haskell.TH.Syntax hiding (newName)
import Language.Haskell.TH.Syntax.Compat (Quote(..))

import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.RWS (RWST(RWST), runRWST)
import Control.Monad.State (StateT(StateT), runStateT)
import qualified Control.Monad.Trans as Trans (MonadTrans(lift))
import Control.Monad.Writer (WriterT(WriterT), runWriterT)

#if !(MIN_VERSION_template_haskell(2,15,0))
import Language.Haskell.TH.LanguageExtensions (Extension(..))
#endif

#if MIN_VERSION_template_haskell(2,16,0) && !(MIN_VERSION_template_haskell(2,23,0))
import GHC.Ptr (Ptr(Ptr))
import GHC.ForeignPtr (newForeignPtr_)
import Language.Haskell.TH.Syntax.Compat (liftTypedFromUntypedSplice)
import System.IO.Unsafe (unsafePerformIO)
#endif

#if !MIN_VERSION_template_haskell(2,17,0)
import Control.Applicative (liftA2)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad.Fix (MonadFix (..))
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.Semigroup as Semi

# if MIN_VERSION_base(4,11,0)
import Control.Exception (throwIO, catch)
import GHC.IO.Exception (BlockedIndefinitelyOnMVar (..), FixIOException (..))
# endif
#endif

#if !(MIN_VERSION_template_haskell(2,15,0))
deriving instance Bounded Extension
#endif

instance Quote m => Quote (ReaderT r m) where
    newName = Trans.lift . newName
$(deriveQuasiTrans
    [t| forall r m. Quasi m => Quasi (ReaderT r m) |]
    [e| \m1 m2 -> ReaderT $ \ r -> runReaderT m1 r `qRecover` runReaderT m2 r |])

instance (Quote m, Monoid w) => Quote (WriterT w m) where
    newName = Trans.lift . newName
$(deriveQuasiTrans
    [t| forall w m. (Quasi m, Monoid w) => Quasi (WriterT w m) |]
    [e| \m1 m2 -> WriterT $ runWriterT m1 `qRecover` runWriterT m2 |])

instance Quote m => Quote (StateT s m) where
    newName = Trans.lift . newName
$(deriveQuasiTrans
    [t| forall s m. Quasi m => Quasi (StateT s m) |]
    [e| \m1 m2 -> StateT $ \ s -> runStateT m1 s `qRecover` runStateT m2 s |])

instance (Quote m, Monoid w) => Quote (RWST r w s m) where
    newName = Trans.lift . newName
$(deriveQuasiTrans
    [t| forall r w s m. (Quasi m, Monoid w) => Quasi (RWST r w s m) |]
    [e| \m1 m2 -> RWST $ \ r s -> runRWST m1 r s `qRecover` runRWST m2 r s |])

#if MIN_VERSION_template_haskell(2,16,0) && !(MIN_VERSION_template_haskell(2,23,0))
instance Lift Bytes where
  lift bytes =
    [| Bytes
      { bytesPtr = unsafePerformIO $ newForeignPtr_ (Ptr $(litE (BytesPrimL bytes)))
      , bytesOffset = 0
      , bytesSize = size
      }
    |]
    where
      size = bytesSize bytes
  liftTyped = liftTypedFromUntypedSplice
#endif

#if !MIN_VERSION_template_haskell(2,17,0)
instance Semi.Semigroup a => Semi.Semigroup (Q a) where
  (<>) = liftA2 (Semi.<>)

instance Monoid a => Monoid (Q a) where
  mempty = return mempty
#if !MIN_VERSION_base(4,11,0)
  mappend = liftA2 mappend
#endif

-- | If the function passed to 'mfix' inspects its argument,
-- the resulting action will throw a 'FixIOException'
-- (@base >=4.11@) or a 'BlockedIndefinitelyOnMVar'
-- with older @base@.
--
instance MonadFix Q where
  mfix k = do
    m <- runIO newEmptyMVar
    ans <- runIO (unsafeInterleaveIO (readMVar m
#if MIN_VERSION_base(4,11,0)
        `catch` \BlockedIndefinitelyOnMVar -> throwIO FixIOException
#endif
        ))
    result <- k ans
    runIO (putMVar m result)
    return result
#endif

$(reifyManyWithoutInstances ''Lift [''Info, ''Loc] (const True) >>=
  deriveLiftMany)
