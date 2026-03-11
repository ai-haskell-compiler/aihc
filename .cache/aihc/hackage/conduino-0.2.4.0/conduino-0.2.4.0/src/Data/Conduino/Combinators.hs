{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}

-- |
-- Module      : Data.Conduino.Combinators
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- A basic collection of base 'Pipe's that serve as a "prelude" for the
-- package.  This module is meant to be imported qualified.
--
-- > import qualified Data.Conduino.Combinators as C
--
module Data.Conduino.Combinators (
  -- * Sources
  -- ** Pure
  -- *** Infinite
    unfold
  , iterate
  , repeat
  -- *** Finite
  , unfoldMaybe
  , unfoldEither
  , iterateMaybe
  , iterateEither
  , sourceList
  , replicate
  -- ** Monadic
  -- *** Infinite
  , repeatM
  -- *** Finite
  , repeatMaybeM
  , repeatEitherM
  , replicateM
  , sourceHandleLines
  , sourceHandleLinesText
  , stdinLines
  , sourceHandle
  , stdin
  -- * Pipes
  , map
  , mapM
  , iterM
  , scan
  , mapAccum
  , take
  , takeWhile
  , filter
  , concatMap
  , concat
  , pairs
  , consecutive
  -- * Sinks
  -- ** Pure
  , drop
  , dropWhile
  , foldr
  , foldl
  , foldMap
  , fold
  , sinkNull
  , sinkList
  , last
  -- ** Monadic
  , sinkHandle
  , stdout
  , stderr
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad hiding          (mapM, replicateM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Conduino
import           Data.Either
import           Data.Foldable hiding          (foldr, foldl, fold, concat, concatMap, foldMap)
import           Data.Maybe
import           Data.Semigroup
import           Prelude hiding                (map, iterate, mapM, replicate, repeat, foldr, drop, foldl, last, take, concatMap, filter, concat, takeWhile, dropWhile, foldMap)
import           System.IO.Error
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified System.IO                     as S

-- | A version of 'unfoldMaybe' that can choose the "result" value by
-- passing it in as 'Left'.
unfoldEither
    :: (s -> Either a (o, s))
    -> s
    -> Pipe i o u m a
unfoldEither f = go
  where
    go z = case f z of
      Left  r       -> pure r
      Right (x, z') -> yield x *> go z'
{-# INLINE unfoldEither #-}

-- | A version of 'unfold' that can terminate and end by returning
-- 'Nothing'.
unfoldMaybe
    :: (s -> Maybe (o, s))
    -> s
    -> Pipe i o u m ()
unfoldMaybe f = unfoldEither (maybe (Left ()) Right . f)
{-# INLINE unfoldMaybe #-}

-- | Repeatedly apply an "unfolding" function to a given initial state,
-- yielding the first item in the tuple as output and updating the state as
-- the second item in the tuple.  Goes on forever.  See 'unfoldMaybe' for
-- a version that stops.
unfold
    :: (s -> (o, s))
    -> s
    -> Pipe i o u m a
unfold f = go
  where
    go z = yield x *> go z'
      where
        (x, z') = f z
{-# INLINE unfold #-}

-- | A version of 'iterateMaybe' that can specify a result value by
-- providing it in the 'Left'.
iterateEither
    :: (o -> Either a o)
    -> o
    -> Pipe i o u m a
iterateEither f = unfoldEither (fmap (join (,)) . f)
{-# INLINE iterateEither #-}

-- | A version of 'iterate' that can choose to terminate and stop by
-- returning 'Nothing'.
iterateMaybe
    :: (o -> Maybe o)
    -> o
    -> Pipe i o u m ()
iterateMaybe f = unfoldMaybe (fmap (join (,)) . f)
{-# INLINE iterateMaybe #-}

-- | Repeatedly apply a function to a given starting value and yield each
-- result forever.
--
-- >>> runPipePure $ iterate succ 0
--       .| take 5
--       .| sinkList
--
-- [1,2,3,4,5]
--
-- This doesn't yield the original starting value.  However, you can yield
-- it iterate after:
--
-- >>> runPipePure $ (yield 0 >> iterate succ 0)
--       .| take 5
--       .| sinkList
--
-- [0,1,2,3,4,5]
iterate
    :: (o -> o)
    -> o
    -> Pipe i o u m a
iterate f = unfold (join (,) . f)
{-# INLINE iterate #-}

-- | Yield every item in a foldable container.
sourceList :: Foldable t => t a -> Pipe i a u m ()
sourceList = traverse_ yield
{-# INLINE sourceList #-}

-- | Repeatedly yield a given item forever.
repeat :: o -> Pipe i o u m a
repeat = forever . yield
{-# INLINE repeat #-}

-- | Yield a given item a certain number of times.
replicate :: Int -> o -> Pipe i o u m ()
replicate n = replicateM_ n . yield
{-# INLINE replicate #-}

-- | Like 'repeatMaybeM', but allow specification of a final result type.
repeatEitherM
    :: Monad m
    => m (Either a o)
    -> Pipe i o u m a
repeatEitherM x = go
  where
    go = lift x >>= \case
      Left r  -> pure r
      Right y -> yield y *> go
{-# INLINE repeatEitherM #-}

-- | Repeat a monadic action, yielding the item in the 'Just' every time.
-- As soon as it sees 'Nothing', stop producing forever.
--
-- Remember that each item will only be "executed" when something
-- downstream requests output.
repeatMaybeM
    :: Monad m
    => m (Maybe o)
    -> Pipe i o u m ()
repeatMaybeM = repeatEitherM . fmap (maybe (Left ()) Right)
{-# INLINE repeatMaybeM #-}

-- | Repeat a monadic action a given number of times, yielding each result,
-- and then stop producing forever.
--
-- Remember that each item will only be "executed" when something
-- downstream requests output.
replicateM
    :: Monad m
    => Int
    -> m o
    -> Pipe i o u m ()
replicateM n = replicateM_ n . (yield =<<) . lift
{-# INLINE replicateM #-}

-- | Source from each line received from 'stdin'.  This stops as soon as
-- end-of-file is reached, or an empty line is seen.
stdinLines :: MonadIO m => Pipe i String u m ()
stdinLines = sourceHandleLines S.stdin
          .| takeWhile (not . null)
{-# INLINE stdinLines #-}

-- | Source from stdin, yielding bytestrings as they are drawn.  If you
-- want to retrieve each line as a string, see 'stdinLines'.
stdin :: MonadIO m => Pipe i BS.ByteString u m ()
stdin = sourceHandle S.stdin
{-# INLINE stdin #-}

-- | Source from a given I/O handle, yielding each line drawn as a string.
-- To draw raw bytes, use 'sourceHandle'.
--
-- This stop as soon as end-of-file is reached.
--
-- Sinve v0.2.3.0, this now continues through empty lines.
sourceHandleLines
    :: MonadIO m
    => S.Handle
    -> Pipe i String u m ()
sourceHandleLines h = repeatMaybeM $ do
    d <- liftIO $ S.hIsEOF h
    if d
      then pure Nothing
      else liftIO . catchJust
                (guard . isEOFError)
                (Just <$> S.hGetLine h)
                $ \_ -> pure Nothing
{-# INLINE sourceHandleLines #-}

-- | Source from a given I/O handle, yielding each line drawn as a string.
-- To draw raw bytes, use 'sourceHandle'.
--
-- This stop as soon as end-of-file is reached, or an empty line is seen.
sourceHandleLinesText
    :: MonadIO m
    => S.Handle
    -> Pipe i T.Text u m ()
sourceHandleLinesText h = repeatMaybeM $ do
    d <- liftIO $ S.hIsEOF h
    if d
      then pure Nothing
      else liftIO . catchJust
                (guard . isEOFError)
                (Just <$> T.hGetLine h)
                $ \_ -> pure Nothing
{-# INLINE sourceHandleLinesText #-}

-- | Source from a given I/O handle, yielding bytestrings as they are
-- pulled.  If you want to retrieve each line as a string, see
-- 'sourceHandleLines'.
sourceHandle
    :: MonadIO m
    => S.Handle
    -> Pipe i BS.ByteString u m ()
sourceHandle h = repeatMaybeM
               . fmap (mfilter (not . BS.null) . Just)
               . liftIO
               $ BS.hGetSome h BSL.defaultChunkSize
{-# INLINE sourceHandle #-}

-- | Sink into a given I/O handle, writing each input to the handle.
sinkHandle
    :: MonadIO m
    => S.Handle
    -> Pipe BS.ByteString o u m ()
sinkHandle h = mapM (liftIO . BS.hPut h)
            .| sinkNull
{-# INLINE sinkHandle #-}

-- | A sink into stdout.
stdout :: MonadIO m => Pipe BS.ByteString o u m ()
stdout = sinkHandle S.stdout
{-# INLINE stdout #-}

-- | A sink into stderr.
stderr :: MonadIO m => Pipe BS.ByteString o u m ()
stderr = sinkHandle S.stderr
{-# INLINE stderr #-}

-- | Repeat a monadic action forever, yielding each output.
--
-- Remember that each item will only be "executed" when something
-- downstream requests output.
repeatM
    :: Monad m
    => m o
    -> Pipe i o u m a
repeatM x = go
  where
    go = (yield =<< lift x) *> go
{-# INLINE repeatM #-}

-- | Process every incoming item with a pure function, and yield its
-- output.
map :: (i -> o) -> Pipe i o u m u
map f = awaitForever (yield . f)
{-# INLINE map #-}

-- | Map a monadic function to process every input, and yield its output.
mapM :: Monad m => (i -> m o) -> Pipe i o u m u
mapM f = awaitForever ((yield =<<) . lift . f)
{-# INLINE mapM #-}

-- | Execute a monadic function to process every input, passing through the
-- original value back downstream.
--
-- @since 0.2.1.0
iterM :: Monad m => (i -> m ()) -> Pipe i i u m u
iterM f = mapM (\x -> x <$ f x)
{-# INLINE iterM #-}

-- | Map a pure "stateful" function over each incoming item.  Give
-- a function to update the state and return an output and an initial
-- state.
mapAccum
    :: (i -> s -> (s, o))       -- ^ update state and output
    -> s                        -- ^ initial state
    -> Pipe i o u m u
mapAccum f = go
  where
    go !x = awaitWith $ \y ->
        let (!x', !z) = f y x
        in  yield z *> go x'
{-# INLINE mapAccum #-}

-- | Like 'foldl', but yields every accumulator value downstream.
--
-- >>> runPipePure $ sourceList [1..10]
--       .| scan (+) 0
--       .| sinkList
-- [1,3,6,10,15,21,28,36,45,55]
-- @
scan
    :: (o -> i -> o)
    -> o
    -> Pipe i o u m u
scan f = go
  where
    go !x = awaitWith $ \y ->
      let x' = f x y
      in  yield x' *> go x'
{-# INLINE scan #-}

-- | Yield consecutive pairs of values.
--
-- >>> runPipePure $ sourceList [1..5]
--       .| pairs
--       .| sinkList
-- [(1,2),(2,3),(3,4),(4,5)]
pairs :: Pipe i (i, i) u m u
pairs = awaitWith go
  where
    go x = awaitWith $ \y -> do
      yield (x, y)
      go y
{-# INLINE pairs #-}

-- | Yield consecutive runs of at most @n@ of values, starting with an
-- empty sequence.
--
-- To get only "full" sequences, pipe with 'filter'.
--
-- >>> runPipePure $ sourceList [1..6]
--       .| consecutive 3
--       .| map toList
--       .| sinkList
-- [[],[1],[1,2],[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
--
-- >>> runPipePure $ sourceList [1..6]
--       .| consecutive 3
--       .| filter ((== 3) . Seq.length)
--       .| map toList
--       .| sinkList
-- [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
consecutive :: Int -> Pipe i (Seq.Seq i) u m u
consecutive n = go Seq.empty
  where
    go xs = do
      yield xs
      awaitWith $ \y -> go . Seq.drop (Seq.length xs - n + 1) $ (xs Seq.:|> y)
{-# INLINE consecutive #-}

-- | Let a given number of items pass through the stream uninhibited, and
-- then stop producing forever.
--
-- This is most useful if you sequence a second conduit after it.
--
-- >>> runPipePure $ sourceList [1..8]
--       .| (do take 3 .| map (*2)         -- double the first 3 items
--              map negate                 -- negate the rest
--          )
--       .| sinkList
-- [2,4,6,-4,-5,-6,-7,-8]
take :: Int -> Pipe i i u m ()
take n = void . runMaybeT . replicateM_ n $
    lift . yield =<< MaybeT await
{-# INLINE take #-}

-- | Let elements pass until an element is received that does not satisfy
-- the predicate, then stop producing forever.
--
-- Like 'take', is most useful if you sequence a second conduit after it.
takeWhile :: (i -> Bool) -> Pipe i i u m ()
takeWhile p = go
  where
    go = await >>= \case
      Nothing -> pure ()
      Just x
        | p x       -> yield x *> go
        | otherwise -> pure ()
{-# INLINE takeWhile #-}

-- | Only allow values satisfying a predicate to pass.
filter
    :: (i -> Bool)
    -> Pipe i i u m u
filter p = awaitForever $ \x -> when (p x) $ yield x
{-# INLINE filter #-}

-- | Map a function returning a container onto every incoming item, and
-- yield all of the outputs from that function.
concatMap
    :: Foldable t
    => (i -> t o)
    -> Pipe i o u m u
concatMap f = awaitForever (sourceList . f)
{-# INLINE concatMap #-}

-- | Take an input of containers and output each of their elements
-- successively.
concat :: Foldable t => Pipe (t i) i u m u
concat = awaitForever sourceList
{-# INLINE concat #-}

-- | Right-fold every input into an accumulated value.
--
-- Essentially this builds up a giant continuation that will be run all at
-- once on the final result.
foldr :: (a -> b -> b) -> b -> Pipe a o u m b
foldr f z = go
  where
    go = await >>= \case
      Nothing -> pure z
      Just x  -> f x <$> go
{-# INLINE foldr #-}

-- | Left-fold every input into an accumulated value.
--
-- Essentially this maintains a state and modifies that state with every
-- input, using the given accumulating function.
foldl :: (b -> a -> b) -> b -> Pipe a o u m b
foldl f = go
  where
    go !z = await >>= \case
      Nothing -> pure z
      Just !x -> go (f z x)
{-# INLINE foldl #-}

-- | Fold every incoming item monoidally, and return the result once
-- finished.
fold :: Monoid a => Pipe a o u m a
fold = foldl (<>) mempty
{-# INLINE fold #-}

-- | Fold every incoming item according to a monoidal projection, and
-- return the result once finished.
--
-- This can be used to implement many useful consumers, like ones that find
-- the sum or the maximum item:
--
-- @
-- sum :: Num i => Pipe i o u m i
-- sum = getSum <$> foldMap Sum
--
-- maximum :: Ord i => Pipe i o u m (Maybe i)
-- maximum = fmap getMax <$> foldMap (Just . Max)
-- @
foldMap :: Monoid a => (i -> a) -> Pipe i o u m a
foldMap f = foldl (\x y -> x <> f y) mempty
{-# INLINE foldMap #-}

-- | Sink every incoming item into a list.
--
-- Note that this keeps the entire list in memory until it is all
-- eventually read, and does no "lazy IO".  It exhauts the whole stream
-- before returning anything.
sinkList :: Pipe i o u m [i]
sinkList = foldr (:) []
{-# INLINE sinkList #-}

-- | Ignore a certain amount of items from the input stream, and then stop
-- producing forever.
--
-- This is most useful if you sequence a second consumer after it:
--
-- >>> runPipePure $ sourceList [1..8]
--       .| (drop 3 >> 'sinkList')
-- [4,5,6,7,8]
drop :: Int -> Pipe i o u m ()
drop n = replicateM_ n await
{-# INLINE drop #-}

-- | Ignore items from an input stream as long as they match a predicate.
-- Afterwards, stop producing forever.
--
-- Like for 'drop', is most useful of you sequence a second consumer after
-- it.
dropWhile
    :: (i -> Bool)
    -> Pipe i o u m ()
dropWhile p = go
  where
    go = await >>= \case
      Nothing -> pure ()
      Just x
        | p x       -> go
        | otherwise -> pure ()
{-# INLINE dropWhile #-}

-- | Consume an entire input stream and ignore all of its outputs.
sinkNull :: Pipe i o u m ()
sinkNull = await >>= \case
    Nothing -> pure ()
    Just _  -> sinkNull

-- | Get the last item emitted by a stream.
--
-- To get the first item ("head"), use 'await' or 'awaitSurely'.
last :: Pipe i o u m (Maybe i)
last = go Nothing
  where
    go x = await >>= \case
      Nothing -> pure x
      Just r  -> go (Just r)
{-# INLINE last #-}
