{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Data.Conduino
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Base API for 'Pipe'.  See documentation for 'Pipe', '.|', and 'runPipe'
-- for information on usage.
--
-- A "prelude" of useful pipes can be found in "Data.Conduino.Combinators".
--
-- == Why a stream processing library?
--
-- A stream processing library is a way to stream processors in a /composable/ way:
-- instead of defining your entire stream processing function as a single
-- recursive loop with some global state, instead think about each "stage" of the process,
-- and isolate each state to its own segment.  Each component can contain its own
-- isolated state:
--
-- >>> runPipePure $ sourceList [1..10]
--       .| scan (+) 0
--       .| sinkList
-- [1,3,6,10,15,21,28,36,45,55]
--
-- All of these components have internal "state":
--
-- *   @sourceList@ keeps track of "which" item in the list to yield next
-- *   @scan@ keeps track of the current running sum
-- *   @sinkList@ keeps track of all items that have been seen so far, as a list
--
-- They all work together without knowing any other component's internal state, so
-- you can write your total streaming function without concerning yourself, at
-- each stage, with the entire part.
--
-- In addition, there are useful functions to "combine" stream processors:
--
-- *   'zipSink' combines sinks in an "and" sort of way: combine two sinks in
--     parallel and finish when all finish.
-- *   'altSink' combines sinks in an "or" sort of way: combine two sinks in
--     parallel and finish when any of them finish
-- *   'zipSource' combines sources in parallel and collate their outputs.
--
-- Stream processing libraries are also useful for streaming composition of
-- monadic effects (like IO or State), as well.
--
module Data.Conduino (
    Pipe
  , (.|)
  , runPipe, runPipePure
  -- * Primitives
  , awaitEither, await, awaitWith, awaitSurely, awaitForever
  , yield, yieldLazy
  -- * Special chaining
  , (&|), (|.)
  , fuseBoth, fuseUpstream, fuseBothMaybe
  -- * Incremental running
  , squeezePipe, squeezePipeEither
  , feedPipe, feedPipeEither
  -- * Pipe transformers
  , mapInput, mapOutput, mapUpRes, trimapPipe
  , passthrough
  , hoistPipe
  , feedbackPipe, feedbackPipeEither
  -- * Wrappers
  , ZipSource(..)
  , unconsZipSource
  , zipSource, liftZipSource
  , ZipSink(..)
  , zipSink, liftZipSink, altSink
  -- * Generators
  , toListT, fromListT
  , pattern PipeList
  , withSource, genSource
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Free         (FreeT(..), FreeF(..))
import           Control.Monad.Trans.Free.Church
import           Data.Bifunctor
import           Data.Conduino.Internal
import           Data.Functor
import           Data.Functor.Identity
import           Data.Sequence                    (Seq(..))
import           Data.Void
import           List.Transformer                 (ListT(..), Step(..))
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Data.Sequence                    as Seq
import qualified List.Transformer                 as LT

-- | Await input from upstream.  Will block until upstream 'yield's.
--
-- Will return 'Nothing' if the upstream pipe finishes and terminates.
--
-- If the upstream pipe never terminates, then you can use 'awaitSurely' to
-- guarantee a result.
--
-- Will always return 'Just' if @u@ is 'Void'.
await :: Pipe i o u m (Maybe i)
await = either (const Nothing) Just <$> awaitEither
{-# INLINE await #-}

-- | 'await', but directly chaining a continuation if the 'await' was
-- succesful.
--
-- The await will always be succesful if @u@ is 'Void'.
--
-- This is a way of writing code in a way that is agnostic to how the
-- upstream pipe terminates.
awaitWith :: (i -> Pipe i o u m u) -> Pipe i o u m u
awaitWith f = awaitEither >>= \case
    Left  r -> pure r
    Right x -> f x
{-# INLINE awaitWith #-}

-- | Await input from upstream where the upstream pipe is guaranteed to
-- never terminate.
--
-- A common type error will occur if @u@ (upstream pipe result type) is not
-- 'Void' -- it might be @()@ or some non-'Void' type.  This means that the
-- upstream pipe terminates, so awaiting cannot be assured.
--
-- In that case, either change your upstream pipe to be one that never
-- terminates (which is most likely not possible), or use 'await' instead
-- of 'awaitSurely'.
awaitSurely :: Pipe i o Void m i
awaitSurely = either absurd id <$> awaitEither
{-# INLINE awaitSurely #-}

-- | A useful utility function over repeated 'await's.  Will repeatedly
-- 'await' and then continue with the given pipe whenever the upstream pipe
-- yields.
--
-- Can be used to implement many pipe combinators:
--
-- @
-- 'Data.Conduino.Combinators.map' f = 'awaitForever' $ \x -> 'yield' (f x)
-- @
awaitForever :: (i -> Pipe i o u m a) -> Pipe i o u m u
awaitForever = awaitForeverWith pure
{-# INLINE awaitForever #-}

-- | 'awaitForever', but with a way to handle the result of the
-- upstream pipe, which will be called when the upstream pipe stops
-- producing.
awaitForeverWith
    :: (u -> Pipe () o u m b)       -- ^ how to handle upstream ending, transitioning to a source
    -> (i -> Pipe i o u m a)        -- ^ how to handle upstream output
    -> Pipe i o u m b
awaitForeverWith f g = go
  where
    go = awaitEither >>= \case
      Left x  -> mapInput (const ()) $ f x
      Right x -> g x *> go
{-# INLINE awaitForeverWith #-}

-- | Run a pipe that is both a source and a sink (an "effect") into the
-- effect that it represents.
--
-- Usually you wouild construct this using something like:
--
-- @
-- 'runPipe' $ someSource
--        '.|' somePipe
--        .| someOtherPipe
--        .| someSink
-- @
--
-- 'runPipe' will produce the result of that final sink.
--
-- Some common errors you might receive:
--
-- *  @i@ is not @()@: If you give a pipe where the first parameter
--    ("input") is not @()@, it means that your pipe is not a producer.
--    Pre-compose it (using '.|') with a producer of the type you need.
--
--    For example, if you have a @myPipe :: 'Pipe' 'Int' o u m a@, this is
--    a pipe that is awaiting 'Int's from upstream.  Pre-compose with
--    a producer of 'Int's, like @'Data.Conduino.Combinators.sourceList'
--    [1,2,3] '.|' myPipe@, in order to be able to run it.
--
-- *  @o@ is not 'Void': If you give a pipe where the second parameter
--    ("output") is not 'Void', it means that your pipe is not a consumer.
--    Post-compose it (using '.|') with a consumer of the type you need.
--
--    For example, if you have @myPipe :: 'Pipe' i 'Int' u m a@, this is
--    a pipe that is yielding 'Int's downstream that are going unhandled.
--    Post-compose it a consumer of 'Int's, like @myPipe '.|'
--    'Data.Conduino.foldl' (+) 0@, in order to be able to run it.
--
--    If you just want to ignore all downstream yields, post-compose with
--    'Data.Conduino.Combinators.sinkNull'.
--
runPipe :: Monad m => Pipe () Void u m a -> m a
runPipe = iterT go . pipeFree
  where
    go = \case
      PAwaitF _ f -> f ()
      PYieldF o _ -> absurd o

-- | 'runPipe' when the underlying monad is 'Identity', and so has no
-- effects.
runPipePure :: Pipe () Void Void Identity a -> a
runPipePure = runIdentity . runPipe
{-# INLINE runPipePure #-}

-- | Repeatedly run 'squeezePipe' by giving it items from an input list.
-- Returns the outputs observed, and 'Left' if the input list was exhausted
-- with more input expected, or 'Right' if the pipe terminated, with the
-- leftover inputs and output result.
--
-- @since 0.2.1.0
feedPipe
    :: Monad m
    => [i]                      -- ^ input to feed in
    -> Pipe i o u m a
    -> m ([o], Either (i -> Pipe i o u m a) ([i], a))
feedPipe xs = (fmap . second . first) (. Right)
            . feedPipeEither xs
{-# INLINE feedPipe #-}

-- | Repeatedly run 'squeezePipeEither' by giving it items from an input
-- list.  Returns the outputs observed, and 'Left' if the input list was
-- exhausted with more input expected (or a @u@ terminating upstream
-- value), or 'Right' if the pipe terminated, with the leftover inputs and
-- output result.
--
-- @since 0.2.1.0
feedPipeEither
    :: Monad m
    => [i]                      -- ^ input to feed in
    -> Pipe i o u m a
    -> m ([o], Either (Either u i -> Pipe i o u m a) ([i], a))
feedPipeEither xs p = do
    (zs, r) <- squeezePipeEither p
    case r of
      Left n -> case xs of
        []   -> pure (zs, Left n)
        y:ys -> first (zs ++) <$> feedPipeEither ys (n (Right y))
      Right z -> pure (zs, Right (xs, z))
{-# INLINE feedPipeEither #-}

-- | "Squeeze" a pipe by extracting all output that can be extracted
-- before any input is requested.  Returns a 'Left' if the pipe eventually
-- does request input (as a continuation on the new input), or a 'Right' if
-- the pipe terminates with a value before ever asking for input.
--
-- @since 0.2.1.0
squeezePipe
    :: Monad m
    => Pipe i o u m a
    -> m ([o], Either (i -> Pipe i o u m a) a)
squeezePipe = (fmap . second . first) (. Right)
            . squeezePipeEither
{-# INLINE squeezePipe #-}

-- | "Squeeze" a pipe by extracting all output that can be extracted before
-- any input is requested.  Returns a 'Left' if the pipe eventually does
-- request input (as a continuation on the new input, or a terminating @u@
-- value), or a 'Right' if the pipe terminates with a value before ever
-- asking for input.
--
-- @since 0.2.1.0
squeezePipeEither
    :: Monad m
    => Pipe i o u m a
    -> m ([o], Either (Either u i -> Pipe i o u m a) a)
squeezePipeEither (Pipe (FT p)) = p
    (pure . ([],) . Right)
    (\pNext -> \case
        PAwaitF f g -> pure . ([],) . Left $ (unSqueeze =<<) . lift . pNext . either f g
        PYieldF o x -> first (o:) <$> pNext x
    )
  where
    unSqueeze (os, nxt) = do
      mapM_ yield os
      case nxt of
        Left f  -> f =<< awaitEither
        Right a -> pure a
{-# INLINE squeezePipeEither #-}

-- | The main operator for chaining pipes together.  @pipe1 .| pipe2@ will
-- connect the output of @pipe1@ to the input of @pipe2@.
--
-- "Running" a pipe will draw from @pipe2@, and if @pipe2@ ever asks for
-- input (with 'await' or something similar), it will block until @pipe1@
-- outputs something (or signals termination).
--
-- The structure of a full pipeline usually looks like:
--
-- @
-- 'runPipe' $ someSource
--        '.|' somePipe
--        .| someOtherPipe
--        .| someSink
-- @
--
-- Where you route a source into a series of pipes, which eventually ends
-- up at a sink.  'runPipe' will then produce the result of that sink.
(.|)
    :: Monad m
    => Pipe a b u m v
    -> Pipe b c v m r
    -> Pipe a c u m r
p .| q = fromRecPipe $ compPipe_ (toRecPipe p) (toRecPipe q)
infixr 2 .|
{-# INLINE (.|) #-}

compPipe_
    :: forall a b c u v m r. (Monad m)
    => RecPipe a b u m v
    -> RecPipe b c v m r
    -> RecPipe a c u m r
compPipe_ p q = FreeT $ runFreeT q >>= \qq -> case qq of
    Pure x             -> pure . Pure $ x
    Free (PAwaitF f g) -> runFreeT p >>= \pp -> case pp of
      Pure x'              -> runFreeT $ compPipe_ (FreeT (pure pp)) (f x')
      Free (PAwaitF f' g') -> pure . Free $ PAwaitF ((`compPipe_` FreeT (pure qq)) . f')
                                                    ((`compPipe_` FreeT (pure qq)) . g')
      Free (PYieldF x' y') -> runFreeT $ compPipe_ y' (g x')
    Free (PYieldF x y) -> pure . Free $ PYieldF x (compPipe_ p y)

-- | Useful prefix version of '&|'.
--
-- @since 0.2.1.0
fuseBoth
    :: Monad m
    => Pipe a b u m v
    -> Pipe b c v m r
    -> Pipe a c u m (v, r)
fuseBoth p q = p
            .| (q >>= exhaust)
  where
    exhaust x = go
      where
        go = awaitEither >>= \case
          Left  y -> pure (y, x)
          Right _ -> go
{-# INLINE fuseBoth #-}

-- | Like 'fuseBoth' and '&|', except does not wait for the upstream pipe
-- to terminate.  Return 'Nothing' in the first field if the upstream pipe hasn't terminated,
-- and 'Just' if it has, with the terminating value.
--
-- @since 0.2.1.0
fuseBothMaybe :: Monad m => Pipe a b u m v -> Pipe b c v m r -> Pipe a c u m (Maybe v, r)
fuseBothMaybe p q = p
                 .| (q >>= check)
  where
    check x = (,x) . either Just (const Nothing) <$> awaitEither
{-# INLINE fuseBothMaybe #-}

-- | Useful prefix version of '|.'.
--
-- @since 0.2.1.0
fuseUpstream
    :: Monad m
    => Pipe a b u m v
    -> Pipe b c v m r
    -> Pipe a c u m v
fuseUpstream p q = fst <$> fuseBoth p q
{-# INLINE fuseUpstream #-}

-- | Like @.|@, but get the result of /both/ pipes on termination, instead
-- of just the second.  This means that @p &| q@ will only terminate with a result when
-- /both/ @p@ and @q@ terminate.  (Typically, @p .| q@ would terminate as soon as
-- @q@ terminates.)
--
-- @since 0.2.1.0
(&|) :: Monad m => Pipe a b u m v -> Pipe b c v m r -> Pipe a c u m (v, r)
(&|) = fuseBoth
{-# INLINE (&|) #-}

-- | Like @.|@, but keep the result of the /first/ pipe, instead of the
-- second.  This means that @p |. q@ will only terminate with a result when
-- /both/ @p@ and @q@ terminate.  (Typically, @p .| q@ would terminate as soon as
-- @q@ terminates.)
--
-- @since 0.2.1.0
(|.) :: Monad m => Pipe a b u m v -> Pipe b c v m r -> Pipe a c u m v
(|.) = fuseUpstream
{-# INLINE (|.) #-}

infixr 2 &|
infixr 2 |.

-- | Passthrough and pair each output with the /last/ input that triggered
-- it.  'Nothing' will occur initially if the pipe outputs anything without
-- consuming any values, but after the first 'Just', should only output
-- Justs forever.
--
-- @since 0.2.3.0
passthrough
    :: Monad m
    => Pipe i o u m a
    -> Pipe i (Maybe i, o) u m a
passthrough p = fmap fst . runStatePS Nothing $
       awaitForever passOn
    .| hoistPipe lift p
    .| awaitForever tagIn
  where
    passOn i = lift (SS.put (Just i)) *> yield i
    tagIn i = yield . (,i) =<< lift SS.get
{-# INLINE passthrough #-}

-- | Loop a pipe into itself.
--
-- *  Will feed all output back to the input
-- *  Will only ask for input upstream if output is stalled.
-- *  Yields all outputted values downstream, effectively duplicating them.
--
-- @since 0.2.1.0
feedbackPipe
    :: Monad m
    => Pipe x x u m a
    -> Pipe x x u m a
feedbackPipe = feedbackPipeEither . mapInput (either id id)
{-# INLINE feedbackPipe #-}

-- | A version of 'feedbackPipe' that distinguishes upstream input from
-- downstream output fed back.  Gets 'Left' from upstream, and 'Right' from
-- its own output.
--
-- *  Will feed all output back to the input
-- *  Will only ask for input upstream if output is stalled.
-- *  Yields all outputted values downstream, effectively duplicating them.
--
-- @since 0.2.2.0
feedbackPipeEither
    :: Monad m
    => Pipe (Either i o) o u m a
    -> Pipe i o u m a
feedbackPipeEither p = fmap fst . runStatePS Seq.empty $
       popper
    .| hoistPipe lift p
    .| awaitForever (\x -> lift (SS.modify (:|> x)) *> yield x)
  where
    popper = lift SS.get >>= \case
      Empty -> awaitEither >>= \case
        Left r  -> pure r
        Right x -> yield (Left x) >> popper
      x :<| xs -> do
        lift $ SS.put xs
        yield (Right x)
        popper
{-# INLINE feedbackPipeEither #-}

-- | A newtype wrapper over a source (@'Pipe' () o 'Void'@) that gives it an
-- alternative 'Applicative' and 'Alternative' instance, matching "ListT
-- done right".
--
-- '<*>' will pair up each output that the sources produce: if you 'await'
-- a value from downstream, it will wait until both paired sources yield
-- before passing them on together.
--
-- '<|>' will completely exhaust the first source before moving on to the
-- next source.
--
-- 'ZipSource' is effectively equivalent to "ListT done right", the true
-- List Monad transformer.  '<|>' is concatentation.  You can use this type
-- with 'lift' to lift a yielding action and '<|>' to sequence yields to
-- implement the pattern described in
-- <http://www.haskellforall.com/2014/11/how-to-build-library-agnostic-streaming.html>,
-- where you can write streaming producers in a polymorphic way, and have
-- it run with pipes, conduit, etc.
--
-- The main difference is that its 'Applicative' instance ("zipping") is
-- different from the traditional 'Applicative' instance for 'ListT'
-- ("all combinations").  Effectively this becomes like a "zipping"
-- 'Applicative' instance for 'ListT'.
--
-- If you want a 'Monad' (or 'Control.Monad.IO.Class.MonadIO') instance,
-- use 'ListT' instead, and convert using 'toListT'/'fromListT' or the
-- 'PipeList' pattern/constructor.
newtype ZipSource m a = ZipSource { getZipSource :: Pipe () a Void m () }

-- | A source is equivalent to a 'ListT' producing a 'Maybe'; this pattern
-- synonym lets you treat it as such.  It essentialyl wraps over 'toListT'
-- and 'fromListT'.
pattern PipeList :: Monad m => ListT m (Maybe a) -> Pipe () a u m ()
pattern PipeList xs <- (toListT->xs)
  where
    PipeList xs = fromListT xs
{-# COMPLETE PipeList #-}

instance Functor (ZipSource m) where
    fmap f = ZipSource . mapOutput f . getZipSource

instance Monad m => Applicative (ZipSource m) where
    pure = ZipSource . yield
    ZipSource p <*> ZipSource q = ZipSource $ zipSource p q

-- | Takes two sources and runs them in parallel, collating their outputs.
--
-- @since 0.2.1.0
zipSource :: Monad m => Pipe () (a -> b) u m () -> Pipe () a v m () -> Pipe () b w m ()
zipSource (PipeList fs) (PipeList xs) = PipeList . fmap Just $
    uncurry ($) <$> LT.zip (concatListT fs) (concatListT xs)
{-# INLINE zipSource #-}

concatListT :: Monad m => ListT m (Maybe a) -> ListT m a
concatListT xs = ListT $ next xs >>= \case
    Nil              -> pure Nil
    Cons Nothing  ys -> next (concatListT ys)
    Cons (Just y) ys -> pure $ Cons y (concatListT ys)

instance Monad m => Alternative (ZipSource m) where
    empty = ZipSource $ pure ()
    ZipSource p <|> ZipSource q = ZipSource (p *> q)

-- | Lift an action into a 'ZipSource'.  Note that this replaces the previous
-- "morally incorrect" 'MonadTrans' instance for 'ZipSource'.
--
-- @since 0.2.4.0
liftZipSource :: Monad m => m a -> ZipSource m a
liftZipSource = ZipSource . (yield =<<) . lift
{-# INLINE liftZipSource #-}

-- | A source is essentially equivalent to 'ListT' producing a 'Maybe'
-- result.  This converts it to the 'ListT' it encodes.
--
-- See 'ZipSource' for a wrapper over 'Pipe' that gives the right 'Functor'
-- and 'Alternative' instances.
toListT
    :: Applicative m
    => Pipe () o u m ()
    -> ListT m (Maybe o)
toListT (Pipe (FT p)) = ListT $ p
    (\_ -> pure Nil)
    (\pNext -> \case
        PAwaitF _ g -> pure $ Cons Nothing  (ListT . pNext $ g ())
        PYieldF x y -> pure $ Cons (Just x) (ListT . pNext $ y   )
    )
{-# INLINE toListT #-}

-- | A source is essentially 'ListT' producing a 'Maybe' result.  This
-- converts a 'ListT' to the source it encodes.
--
-- See 'ZipSource' for a wrapper over 'Pipe' that gives the right 'Functor'
-- and 'Alternative' instances.
fromListT
    :: Monad m
    => ListT m (Maybe o)
    -> Pipe i o u m ()
fromListT xs = lift (next xs) >>= \case
      Nil              -> pure ()
      Cons Nothing  ys -> fromListT ys
      Cons (Just y) ys -> yield y *> fromListT ys

-- | Given a "generator" of @o@ in @m@, return a /source/ that that
-- generator encodes.  Is the inverse of 'withSource'.
--
-- The generator is essentially a church-encoded 'ListT'.
genSource
    :: (forall r. (Maybe (o, m r) -> m r) -> m r)
    -> Pipe i o u m ()
genSource f = Pipe $ FT $ \pDone pFree -> f $ \case
    Nothing      -> pDone ()
    Just (x, xs) -> pFree id (PYieldF x xs)
{-# INLINE genSource #-}

-- | A source can be "run" by providing a continuation to handle and
-- sequence each of its outputs.  Is ths inverse of 'genSource'.
--
-- This essentially turns a pipe into a church-encoded 'ListT'.
withSource
    :: Pipe () o u m ()
    -> (Maybe (o, m r) -> m r)    -- ^ handler ('Nothing' = done, @'Just' (x, next)@ = yielded value and next action
    -> m r
withSource (Pipe (FT p)) f = p
    (\_ -> f Nothing)
    (\pNext -> \case
        PAwaitF _ g -> pNext $ g ()
        PYieldF x y -> f (Just (x, pNext y))
    )

-- | 'ZipSource' is effectively 'ListT' returning a 'Maybe'.  As such, you
-- can use 'unconsZipSource' to "peel off" the first yielded item, if it
-- exists, and return the "rest of the list".
unconsZipSource
    :: Monad m
    => ZipSource m a
    -> m (Maybe (Maybe a, ZipSource m a))
unconsZipSource (ZipSource (PipeList p)) = next p <&> \case
    Cons x xs -> Just (x, ZipSource (PipeList xs))
    Nil       -> Nothing
{-# INLINE unconsZipSource #-}

-- | A newtype wrapper over a sink (@'Pipe' i 'Void'@) that gives it an
-- alternative 'Applicative' and 'Alternative' instance.
--
-- '<*>' will distribute input over both sinks, and output a final result
-- once both sinks finish.
--
-- '<|>' will distribute input over both sinks, and output a final result
-- as soon as one or the other finishes.
newtype ZipSink i u m a = ZipSink { getZipSink :: Pipe i Void u m a }
  deriving Functor

zipSink_
    :: Monad m
    => RecPipe i Void u m (a -> b)
    -> RecPipe i Void u m a
    -> RecPipe i Void u m b
zipSink_ p q = FreeT $ runFreeT p >>= \pp -> case pp of
    Pure x             -> runFreeT q >>= \case
      Pure x'              -> pure . Pure $ x x'
      Free (PAwaitF f' g') -> pure . Free $
        PAwaitF (zipSink_ (FreeT (pure pp)) . f')
                (zipSink_ (FreeT (pure pp)) . g')
      Free (PYieldF x' _ ) -> absurd x'
    Free (PAwaitF f g) -> runFreeT q >>= \qq -> case qq of
      Pure _               -> pure . Free $
        PAwaitF ((`zipSink_` FreeT (pure qq)) . f)
                ((`zipSink_` FreeT (pure qq)) . g)
      Free (PAwaitF f' g') -> pure . Free $
        PAwaitF (zipSink_ <$> f <*> f') (zipSink_ <$> g <*> g')
      Free (PYieldF x' _ ) -> absurd x'
    Free (PYieldF x _) -> absurd x

altSink_
    :: Monad m
    => RecPipe i Void u m a
    -> RecPipe i Void u m a
    -> RecPipe i Void u m a
altSink_ p q = FreeT $ runFreeT p >>= \case
    Pure x             -> pure . Pure $ x
    Free (PAwaitF f g) -> runFreeT q <&> \case
      Pure x'              -> Pure x'
      Free (PAwaitF f' g') -> Free $ PAwaitF (altSink_ <$> f <*> f') (altSink_ <$> g <*> g')
      Free (PYieldF x' _ ) -> absurd x'
    Free (PYieldF x _) -> absurd x

-- | Distribute input to both sinks, and finishes with the final result
-- once both finish.
--
-- Forms an identity with 'pure'.
zipSink
    :: Monad m
    => Pipe i Void u m (a -> b)
    -> Pipe i Void u m a
    -> Pipe i Void u m b
zipSink p q = fromRecPipe $ zipSink_ (toRecPipe p) (toRecPipe q)
{-# INLINE zipSink #-}

-- | Distribute input to both sinks, and finishes with the result of
-- the one that finishes first.
altSink
    :: Monad m
    => Pipe i Void u m a
    -> Pipe i Void u m a
    -> Pipe i Void u m a
altSink p q = fromRecPipe $ altSink_ (toRecPipe p) (toRecPipe q)
{-# INLINE altSink #-}

-- | '<*>' = distribute input to all, and return result when they finish
--
-- 'pure' = immediately finish
instance Monad m => Applicative (ZipSink i u m) where
    pure = ZipSink . pure
    ZipSink p <*> ZipSink q = ZipSink $ zipSink p q

-- | '<|>' = distribute input to all, and return the first result that
-- finishes
--
-- 'empty' = never finish
instance Monad m => Alternative (ZipSink i u m) where
    empty = ZipSink go
      where
        go = forever await
    ZipSink p <|> ZipSink q = ZipSink $ altSink p q

-- | Lift an action into a 'ZipSource'.  Note that this replaces the previous
-- "morally incorrect" 'MonadTrans' instance for 'ZipSource'.
--
-- @since 0.2.4.0
liftZipSink :: Monad m => m a -> ZipSink i u m a
liftZipSink = ZipSink . lift
{-# INLINE liftZipSink #-}
