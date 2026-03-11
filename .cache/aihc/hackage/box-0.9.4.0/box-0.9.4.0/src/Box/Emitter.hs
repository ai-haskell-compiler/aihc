-- | 'Emitter' wraps a producer destructor.
--
-- "Every Thought emits a Dice Throw" ~ Stéphane Mallarmé
module Box.Emitter
  ( Emitter (..),
    type CoEmitter,
    toListM,
    witherE,
    filterE,
    readE,
    unlistE,
    takeE,
    takeUntilE,
    dropE,
    pop,
  )
where

import Box.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.State.Lazy
import Data.Bool
import Data.DList qualified as D
import Data.Sequence qualified as Seq
import Data.Text (Text, pack, unpack)
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude
-- >>> import Box
-- >>> import Data.Bool
-- >>> import Data.Text (Text)

-- | An `Emitter` `emit`s values of type 'Maybe' a. Source and producer are similar metaphors. An Emitter reaches into itself for the value to emit, where itself is an opaque thing from the pov of usage.
--
-- >>> e = Emitter (pure (Just "I'm emitted"))
-- >>> emit e
-- Just "I'm emitted"
--
-- >>> emit mempty
-- Nothing
newtype Emitter m a = Emitter
  { emit :: m (Maybe a)
  }

-- | An 'Emitter' continuation.
type CoEmitter m a = Codensity m (Emitter m a)

instance FFunctor Emitter where
  foist nat (Emitter e) = Emitter (nat e)

instance (Functor m) => Functor (Emitter m) where
  fmap f m = Emitter (fmap (fmap f) (emit m))

instance (Applicative m) => Applicative (Emitter m) where
  pure r = Emitter (pure (pure r))

  mf <*> mx = Emitter ((<*>) <$> emit mf <*> emit mx)

instance (Monad m) => Monad (Emitter m) where
  return = pure

  m >>= f =
    Emitter $ do
      ma <- emit m
      case ma of
        Nothing -> pure Nothing
        Just a -> emit (f a)

instance (Monad m, Alternative m) => Alternative (Emitter m) where
  empty = Emitter (pure Nothing)

  x <|> y =
    Emitter $ do
      (i, ma) <- fmap ((,) y) (emit x) <|> fmap ((,) x) (emit y)
      case ma of
        Nothing -> emit i
        Just a -> pure (Just a)

  -- Zero or more.
  many e = Emitter $ Just <$> toListM e

instance (Alternative m, Monad m) => MonadPlus (Emitter m) where
  mzero = empty

  mplus = (<|>)

instance (Alternative m, Monad m) => Semigroup (Emitter m a) where
  (<>) = (<|>)

instance (Alternative m, Monad m) => Monoid (Emitter m a) where
  mempty = empty

  mappend = (<>)

-- | This fold completes on the first Nothing emitted, which may not be what you want.
instance FoldableM Emitter where
  foldrM acc begin e =
    maybe begin (\a' -> foldrM acc (acc a' begin) e) =<< emit e

-- | Collect emits into a list, and close on the first Nothing.
--
-- >>> toListM <$|> qList [1..3]
-- [1,2,3]
toListM :: (Monad m) => Emitter m a -> m [a]
toListM e = D.toList <$> foldrM (\a acc -> fmap (`D.snoc` a) acc) (pure D.empty) e

-- | A monadic [Witherable](https://hackage.haskell.org/package/witherable)
--
-- >>> toListM <$|> witherE (\x -> bool (print x >> pure Nothing) (pure (Just x)) (even x)) <$> (qList [1..3])
-- 1
-- []
witherE :: (Monad m) => (a -> m (Maybe b)) -> Emitter m a -> Emitter m b
witherE f e = Emitter go
  where
    go = do
      a <- emit e
      case a of
        Nothing -> pure Nothing
        Just a' -> do
          fa <- f a'
          case fa of
            Nothing -> pure Nothing
            Just fa' -> pure (Just fa')

-- | Like witherE but does not emit Nothing on filtering.
--
-- >>> toListM <$|> filterE (\x -> bool (print x >> pure Nothing) (pure (Just x)) (even x)) <$> (qList [1..3])
-- 1
-- 3
-- [2]
filterE :: (Monad m) => (a -> m (Maybe b)) -> Emitter m a -> Emitter m b
filterE f e = Emitter go
  where
    go = do
      a <- emit e
      case a of
        Nothing -> pure Nothing
        Just a' -> do
          fa <- f a'
          case fa of
            Nothing -> go
            Just fa' -> pure (Just fa')

-- | Read parse 'Emitter', returning the original text on error
--
-- >>> (toListM . readE) <$|> (qList ["1","2","3","four"]) :: IO [Either Text Int]
-- [Right 1,Right 2,Right 3,Left "four"]
readE ::
  (Functor m, Read a) =>
  Emitter m Text ->
  Emitter m (Either Text a)
readE = fmap $ parsed . unpack
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _err -> Left (pack str)

-- | Convert a list emitter to a (Stateful) element emitter.
--
-- >>> import Control.Monad.State.Lazy
-- >>> flip runStateT [] . toListM . unlistE <$|> (qList [[0..3],[5..7]])
-- ([0,1,2,3,5,6,7],[])
unlistE :: (Monad m) => Emitter m [a] -> Emitter (StateT [a] m) a
unlistE es = Emitter unlists
  where
    -- unlists :: (Monad m) => StateT [a] m (Maybe a)
    unlists = do
      rs <- get
      case rs of
        [] -> do
          xs <- lift $ emit es
          case xs of
            Nothing -> pure Nothing
            Just xs' -> do
              put xs'
              unlists
        (x : rs') -> do
          put rs'
          pure (Just x)

-- | Take n emits.
--
-- >>> import Control.Monad.State.Lazy
-- >>> flip evalStateT 0 <$|> toListM . takeE 4 <$> qList [0..]
-- [0,1,2,3]
takeE :: (Monad m) => Int -> Emitter m a -> Emitter (StateT Int m) a
takeE n (Emitter e) =
  Emitter $ get >>= \n' -> bool (pure Nothing) (put (n' + 1) >> lift e) (n' < n)

-- | Drop n emits.
--
-- >>> import Control.Monad.State.Lazy
-- >>> toListM <$|> (dropE 2 =<< qList [0..3])
-- [2,3]
dropE :: (Monad m) => Int -> Emitter m a -> CoEmitter m a
dropE n e = Codensity $ \k -> do
  replicateM_ n (emit e)
  k e

-- | Take from an emitter until a predicate.
--
-- >>> (toListM . takeUntilE (==3)) <$|> (qList [0..])
-- [0,1,2]
takeUntilE :: (Monad m) => (a -> Bool) -> Emitter m a -> Emitter m a
takeUntilE p e = Emitter $ do
  x <- emit e
  case x of
    Nothing -> pure Nothing
    Just x' ->
      bool (pure (Just x')) (pure Nothing) (p x')

-- | Pop from a State sequence.
--
-- >>> import qualified Data.Sequence as Seq
-- >>> import Control.Monad.State.Lazy (evalStateT)
-- >>> flip evalStateT (Seq.fromList [1..3]) $ toListM pop
-- [1,2,3]
pop :: (Monad m) => Emitter (StateT (Seq.Seq a) m) a
pop = Emitter $ do
  xs <- get
  case xs of
    Seq.Empty -> pure Nothing
    (x Seq.:<| xs') -> do
      put xs'
      pure (Just x)
