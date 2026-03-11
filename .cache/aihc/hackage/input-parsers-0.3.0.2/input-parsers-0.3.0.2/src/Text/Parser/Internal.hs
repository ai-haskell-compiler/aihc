module Text.Parser.Internal where

import Control.Applicative (liftA2)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(WriterT))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT(WriterT))
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT(StateT))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT(StateT))
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST(RWST))
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST(RWST))

mapLazyWriterT :: Applicative m => (m a -> m b) -> Lazy.WriterT w m a -> Lazy.WriterT w m b
mapLazyWriterT f (Lazy.WriterT p) = Lazy.WriterT (apply p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)

mapStrictWriterT :: Applicative m => (m a -> m b) -> Strict.WriterT w m a -> Strict.WriterT w m b
mapStrictWriterT f (Strict.WriterT p) = Strict.WriterT (apply p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)

mapLazyStateT :: Applicative m => (m a -> m b) -> Lazy.StateT w m a -> Lazy.StateT w m b
mapLazyStateT f (Lazy.StateT p) = Lazy.StateT (apply . p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)

mapStrictStateT :: Applicative m => (m a -> m b) -> Strict.StateT s m a -> Strict.StateT s m b
mapStrictStateT f (Strict.StateT p) = Strict.StateT (apply . p)
   where apply m = liftA2 (,) (f $ fst <$> m) (snd <$> m)

mapLazyRWST :: Applicative m => (m a -> m b) -> Lazy.RWST r w s m a -> Lazy.RWST r w s m b
mapLazyRWST f (Lazy.RWST p) = Lazy.RWST (\r-> apply . p r)
   where apply m = liftA2 replaceFstOf3 (f $ fstOf3 <$> m) m

mapStrictRWST :: Applicative m => (m a -> m b) -> Strict.RWST r w s m a -> Strict.RWST r w s m b
mapStrictRWST f (Strict.RWST p) = Strict.RWST (\r-> apply . p r)
   where apply m = liftA2 replaceFstOf3 (f $ fstOf3 <$> m) m

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) = a

replaceFstOf3 :: a -> (x, b, c) -> (a, b, c)
replaceFstOf3 a (_, b, c) = (a, b, c)
