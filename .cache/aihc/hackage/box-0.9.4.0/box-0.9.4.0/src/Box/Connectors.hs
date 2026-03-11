-- | Various ways to connect things up.
module Box.Connectors
  ( qList,
    qListWith,
    popList,
    pushList,
    pushListN,
    sink,
    sinkWith,
    source,
    sourceWith,
    forkEmit,
    bufferCommitter,
    bufferEmitter,
    concurrentE,
    concurrentC,
    takeQ,
    evalEmitter,
    evalEmitterWith,
  )
where

import Box.Box
import Box.Codensity
import Box.Committer
import Box.Emitter
import Box.Functor
import Box.Queue
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.State.Lazy
import Data.Foldable
import Data.Functor
import Data.Sequence qualified as Seq
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Prelude
-- >>> import Data.Bool
-- >>> import Control.Monad

-- | Queue a list 'Unbounded'.
--
-- >>> pushList <$|> qList [1,2,3]
-- [1,2,3]
qList :: [a] -> CoEmitter IO a
qList xs = qListWith Unbounded xs

-- | Queue a list with an explicit 'Queue'.
--
-- >>> pushList <$|> qListWith Single [1,2,3]
-- [1,2,3]
qListWith :: Queue a -> [a] -> CoEmitter IO a
qListWith q xs = emitQ q (\c -> fmap and (traverse (commit c) xs))

-- | Directly supply a list to a committer action, via pop.
--
-- >>> popList [1..3] showStdout
-- 1
-- 2
-- 3
popList :: (Monad m) => [a] -> Committer m a -> m ()
popList xs c = glueES (Seq.fromList xs) c pop

-- | Push an Emitter into a list, via push.
--
-- >>> pushList <$|> qList [1..3]
-- [1,2,3]
pushList :: (Monad m) => Emitter m a -> m [a]
pushList e = toList <$> flip execStateT Seq.empty (glue push (foist lift e))

-- | Push an Emitter into a list, finitely.
--
-- >>> pushListN 2 <$|> qList [1..3]
-- [1,2]
pushListN :: (Monad m) => Int -> Emitter m a -> m [a]
pushListN n e = toList <$> flip execStateT Seq.empty (glueN n push (foist lift e))

-- singleton sink
sink1 :: (Monad m) => (a -> m ()) -> Emitter m a -> m ()
sink1 f e = do
  a <- emit e
  forM_ a f

-- | Create a finite Committer Unbounded Queue.
--
-- > glue <$> sink 2 print <*|> qList [1..3]
-- > 1
-- > 2
sink :: Int -> (a -> IO ()) -> CoCommitter IO a
sink n f = sinkWith Unbounded n f

-- | Create a finite Committer Queue.
sinkWith :: Queue a -> Int -> (a -> IO ()) -> CoCommitter IO a
sinkWith q n f = commitQ q $ replicateM_ n . sink1 f

-- singleton source
source1 :: (Monad m) => m a -> Committer m a -> m ()
source1 a c = do
  a' <- a
  void $ commit c a'

-- | Create a finite (Co)Emitter Unbounded Queue.
--
-- >>> glue toStdout <$|> source 2 (pure "hi")
-- hi
-- hi
source :: Int -> IO a -> CoEmitter IO a
source n f = sourceWith Unbounded n f

-- | Create a finite (Co)Emitter Unbounded Queue.
--
-- >>> glue toStdout <$|> sourceWith Single 2 (pure "hi")
-- hi
-- hi
sourceWith :: Queue a -> Int -> IO a -> CoEmitter IO a
sourceWith q n f = emitQ q $ replicateM_ n . source1 f

-- | Glues an emitter to a committer, then resupplies the emitter.
--
-- >>> (c1,l1) <- refCommitter :: IO (Committer IO Int, IO [Int])
-- >>> close $ toListM <$> (forkEmit <$> (qList [1..3]) <*> pure c1)
-- [1,2,3]
--
-- >>> l1
-- [1,2,3]
forkEmit :: (Monad m) => Emitter m a -> Committer m a -> Emitter m a
forkEmit e c =
  Emitter $ do
    a <- emit e
    maybe (pure ()) (void <$> commit c) a
    pure a

-- | Buffer a committer.
bufferCommitter :: Committer IO a -> CoCommitter IO a
bufferCommitter c = Codensity $ \caction -> queueL Unbounded caction (glue c)

-- | Buffer an emitter.
bufferEmitter :: Emitter IO a -> CoEmitter IO a
bufferEmitter e = Codensity $ \eaction -> queueR Unbounded (`glue` e) eaction

-- | Concurrently run two emitters.
--
-- This differs to (<>), which is left-biased.
--
-- Note that functions such as toListM, which complete on the first Nothing emitted, will not work as expected.
--
-- >>> close $ (fmap toListM) (join $ concurrentE Single <$> qList [1..3] <*> qList [5..9])
-- [1,2,3]
--
-- In the code below, the ordering is non-deterministic.
--
-- > (c,l) <- refCommitter :: IO (Committer IO Int, IO [Int])
-- > close $ glue c <$> (join $ concurrentE Single <$> qList [1..30] <*> qList [40..60])
concurrentE ::
  Queue a ->
  Emitter IO a ->
  Emitter IO a ->
  CoEmitter IO a
concurrentE q e e' =
  Codensity $ \eaction -> snd . fst <$> concurrently (queue q (`glue` e) eaction) (queue q (`glue` e') eaction)

-- | Concurrently run two committers.
--
-- >>> import Data.Functor.Contravariant
-- >>> import Data.Text (pack)
-- >>> cFast = witherC (\b -> pure (Just b)) . contramap ("fast: " <>) $ toStdout
-- >>> cSlow = witherC (\b -> sleep 0.1 >> pure (Just b)) . contramap ("slow: " <>) $ toStdout
-- >>> close $ (popList ((pack . show) <$> [1..3]) <$> (concurrentC Unbounded cFast cSlow)) <> pure (sleep 1)
-- fast: 1
-- fast: 2
-- fast: 3
-- slow: 1
-- slow: 2
-- slow: 3
concurrentC :: Queue a -> Committer IO a -> Committer IO a -> CoCommitter IO a
concurrentC q c c' = mergeC <$> eitherC q c c'

eitherC ::
  Queue a ->
  Committer IO a ->
  Committer IO a ->
  Codensity IO (Either (Committer IO a) (Committer IO a))
eitherC q cl cr =
  Codensity $
    \kk ->
      fst
        <$> concurrently
          (queueL q (kk . Left) (glue cl))
          (queueL q (kk . Right) (glue cr))

mergeC :: Either (Committer IO a) (Committer IO a) -> Committer IO a
mergeC ec =
  Committer $ \a ->
    case ec of
      Left lc -> commit lc a
      Right rc -> commit rc a

-- | Take and queue n emits.
--
-- >>> import Control.Monad.State.Lazy
-- >>> toListM <$|> (takeQ Single 4 =<< qList [0..])
-- [0,1,2,3]
takeQ :: Queue a -> Int -> Emitter IO a -> CoEmitter IO a
takeQ q n e = emitQ q $ \c -> glueES 0 c (takeE n e)

-- | queue a stateful emitter, supplying initial state
--
-- >>> import Control.Monad.State.Lazy
-- >>> toListM <$|> (evalEmitter 0 <$> takeE 4 =<< qList [0..])
-- [0,1,2,3]
evalEmitter :: s -> Emitter (StateT s IO) a -> CoEmitter IO a
evalEmitter s e = evalEmitterWith Unbounded s e

-- | queue a stateful emitter, supplying initial state
evalEmitterWith :: Queue a -> s -> Emitter (StateT s IO) a -> CoEmitter IO a
evalEmitterWith q s e = emitQ q $ \c -> glueES s c e
