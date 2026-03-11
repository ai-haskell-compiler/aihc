{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Here we implement a special Reader monad
that can be used to manage a call stack.
This way you can generate exception messages like
\"Corrupt file content encountered
while reading file \'foo.txt\'
while loading document \'bar.doc\'\"
using the functions in "Control.Monad.Exception.Label".

However, currently I believe that this datatype is unnecessary,
since you can extend exceptions by context information
using 'Control.Monad.Exception.Synchronous.mapException'.
-}
module Control.Monad.Label where

import Control.Applicative (Applicative(pure, (<*>)), Alternative, )

import Control.Monad (MonadPlus, )
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, )
import Control.Monad.Trans.Class (MonadTrans, )
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.Reader (Reader, ReaderT(ReaderT), runReader, runReaderT, )


-- * Plain monad

newtype Label l a = Label { runLabelPriv :: Reader [l] a }
-- newtype Label l a = Label { runLabelPriv :: [l] -> a }
   deriving (Functor, Applicative, Monad, MonadFix)

{-
instance Functor (Label l) where
   fmap f m = Label $ \l -> f (runLabelPriv m l)

instance Applicative (Label l) where
   pure  = return
   (<*>) = ap

instance Monad (Label l) where
   return a = Label $ \_ -> a
   m >>= k  = Label $ \l -> runLabelPriv (k (runLabelPriv m l)) l

instance MonadFix (Label l) where
   mfix f = Label $ \l -> let a = runLabelPriv (f a) l in a
-}


runLabel :: Label l a -> [l] -> a
runLabel = runReader . runLabelPriv

ask :: Label l [l]
ask = Label Reader.ask
-- ask = Label id

local :: l -> Label l a -> Label l a
local l m = Label $ Reader.local (l:) $ runLabelPriv m
-- local l m = Label $ runLabelPriv m . (l:)




-- * Monad transformer


newtype LabelT l m a = LabelT { runLabelPrivT :: ReaderT [l] m a }
-- newtype LabelT l m a = LabelT { runLabelPrivT :: l -> m a }
   deriving (Alternative, Monad, MonadPlus, MonadFix, MonadTrans, MonadIO)

{-
instance (Monad m) => Functor (LabelT l m) where
   fmap f m = LabelT $ \l -> do
      a <- runLabelPrivT m l
      return (f a)

instance (Monad m) => Monad (LabelT l m) where
   return a = LabelT $ \_ -> return a
   m >>= k  = LabelT $ \l -> do
      a <- runLabelPrivT m l
      runLabelPrivT (k a) l
   fail msg = LabelT $ \_ -> fail msg

instance (MonadPlus m) => MonadPlus (LabelT l m) where
   mzero       = LabelT $ \_ -> mzero
   m `mplus` n = LabelT $ \l -> runLabelPrivT m l `mplus` runLabelPrivT n l

instance (MonadFix m) => MonadFix (LabelT l m) where
   mfix f = LabelT $ \l -> mfix $ \a -> runLabelPrivT (f a) l

instance MonadTrans (LabelT l) where
   lift m = LabelT $ \_ -> m

instance (MonadIO m) => MonadIO (LabelT l m) where
   liftIO = lift . liftIO
-}

{-
instance Monad m => Applicative (LabelT l m) where
   pure = return
   (<*>) = ap
-}


fmapReaderT :: (Functor f) =>
   (a -> b) -> ReaderT r f a -> ReaderT r f b
fmapReaderT f m = ReaderT $ \l -> fmap f $ runReaderT m l

instance (Functor m) => Functor (LabelT l m) where
   fmap f m = LabelT $ fmapReaderT f $ runLabelPrivT m


pureReaderT :: (Applicative f) =>
   a -> ReaderT r f a
pureReaderT a = ReaderT $ const $ pure a

apReaderT :: (Applicative f) =>
   ReaderT r f (a -> b) ->
   ReaderT r f a ->
   ReaderT r f b
apReaderT f x = ReaderT $ \r -> runReaderT f r <*> runReaderT x r

instance Applicative m => Applicative (LabelT l m) where
   pure a  = LabelT $ pureReaderT a
   f <*> x = LabelT $ runLabelPrivT f `apReaderT` runLabelPrivT x


runLabelT :: Monad m => LabelT l m a -> [l] -> m a
runLabelT = runReaderT . runLabelPrivT

askT :: Monad m => LabelT l m [l]
askT = LabelT Reader.ask

localT :: Monad m => l -> LabelT l m a -> LabelT l m a
localT l m = LabelT $ Reader.local (l:) $ runLabelPrivT m
