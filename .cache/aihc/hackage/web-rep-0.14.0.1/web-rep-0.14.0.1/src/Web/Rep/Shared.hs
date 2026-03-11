{-# LANGUAGE OverloadedStrings #-}

-- | A shared-element representation of web page communication.
module Web.Rep.Shared
  ( RepF (..),
    Rep,
    oneRep,
    SharedRepF (..),
    SharedRep,
    runOnce,
    zeroState,
    register,
    message,
    genName,
    genNamePre,
  )
where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Biapplicative
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import MarkupParse
import Optics.Core
import Optics.Zoom

-- |
-- Information contained in a web page can usually be considered to be isomorphic to a map of named values - a 'HashMap'. This is especially true when considering a differential of information contained in a web page. Looking at a page from the outside, it often looks like a streaming differential of a hashmap.
--
-- RepF consists of an underlying value being represented, and, given a hashmap state, a way to produce a representation of the underlying value (or error), in another domain, together with the potential to alter the hashmap state.
data RepF r a = Rep
  { rep :: r,
    make :: HashMap ByteString ByteString -> (HashMap ByteString ByteString, Either ByteString a)
  }
  deriving (Functor)

-- | the common usage, where the representation domain is Html
type Rep a = RepF Markup a

instance (Semigroup r) => Semigroup (RepF r a) where
  (Rep r0 a0) <> (Rep r1 a1) =
    Rep
      (r0 <> r1)
      (\hm -> let (hm', x') = a0 hm in let (hm'', x'') = a1 hm' in (hm'', x' <> x''))

instance (Monoid a, Monoid r) => Monoid (RepF r a) where
  mempty :: RepF r a
  mempty = Rep mempty (,Right mempty)

  mappend = (<>)

instance Bifunctor RepF where
  bimap f g (Rep r a) = Rep (f r) (second (fmap g) . a)

instance Biapplicative RepF where
  bipure r a = Rep r (,Right a)

  (Rep fr fa) <<*>> (Rep r a) =
    Rep
      (fr r)
      ( \hm ->
          let (hm', a') = a hm in let (hm'', fa') = fa hm' in (hm'', fa' <*> a')
      )

instance (Monoid r) => Applicative (RepF r) where
  pure = bipure mempty

  Rep fh fm <*> Rep ah am =
    Rep
      (fh <> ah)
      ( \hm ->
          let (hm', a') = am hm in let (hm'', fa') = fm hm' in (hm'', fa' <*> a')
      )

-- | stateful result of one step, given a 'Rep', and a monadic action.
-- Useful for testing and for initialising a page.
oneRep :: (Monad m) => Rep a -> (Rep a -> HashMap ByteString ByteString -> m ()) -> StateT (HashMap ByteString ByteString) m (HashMap ByteString ByteString, Either ByteString a)
oneRep r@(Rep _ fa) action = do
  m <- get
  let (m', a) = fa m
  put m'
  lift $ action r m'
  pure (m', a)

-- |
-- Driven by the architecture of the DOM, web page components are compositional, and tree-like, where components are often composed of other components, and values are thus shared across components.
--
-- This is sometimes referred to as "observable sharing". See <http://hackage.haskell.org/package/data-reify data-reify> as another library that reifies this (pun intended), and provided the initial inspiration for this implementation.
--
-- unshare should only be run once, which is a terrible flaw that might be fixed by linear types.
newtype SharedRepF m r a = SharedRep
  { unshare :: StateT (Int, HashMap ByteString ByteString) m (RepF r a)
  }
  deriving (Functor)

-- | default representation type of 'Html' ()
type SharedRep m a = SharedRepF m Markup a

instance (Functor m) => Bifunctor (SharedRepF m) where
  bimap f g (SharedRep s) = SharedRep $ fmap (bimap f g) s

instance (Monad m) => Biapplicative (SharedRepF m) where
  bipure r a = SharedRep $ pure $ bipure r a

  (SharedRep f) <<*>> (SharedRep a) = SharedRep $ liftA2 (<<*>>) f a

instance (Monad m, Monoid r) => Applicative (SharedRepF m r) where
  pure = bipure mempty

  SharedRep f <*> SharedRep a = SharedRep $ liftA2 (<*>) f a

-- | name supply for elements of a 'SharedRep'
genName :: (MonadState Int m) => m ByteString
genName = do
  modify (+ 1)
  strToUtf8 . show <$> get

-- | sometimes a number doesn't work properly in html (or js???), and an alpha prefix seems to help
genNamePre :: (MonadState Int m) => ByteString -> m ByteString
genNamePre p = (p <>) <$> genName

-- | Create a sharedRep
register ::
  (Monad m) =>
  -- | Parser
  (ByteString -> Either ByteString a) ->
  -- | Printer
  (a -> ByteString) ->
  -- | create initial object from name and initial value
  (ByteString -> a -> r) ->
  -- | initial value
  a ->
  SharedRepF m r a
register p pr f a =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (HashMap.insert name (pr a)))
    pure $
      Rep
        (f name a)
        ( \s ->
            ( s,
              join $
                maybe
                  (Left "lookup failed")
                  (Right . either (Left . (\x -> name <> ": " <> x)) Right . p)
                  (HashMap.lookup name s)
            )
        )

-- | Like 'register', but does not put a value into the HashMap on instantiation, consumes the value when found in the HashMap, and substitutes a default on lookup failure
message ::
  (Monad m) =>
  -- | Parser
  (ByteString -> Either ByteString a) ->
  -- | create initial object from name and initial value
  (ByteString -> a -> r) ->
  -- | initial value
  a ->
  -- | default value
  a ->
  SharedRepF m r a
message p f a d =
  SharedRep $ do
    n <- zoom _1 genName
    pure $
      Rep
        (f n a)
        ( \s ->
            ( HashMap.delete n s,
              join $
                maybe (Right $ Right d) (Right . p) (HashMap.lookup n s)
            )
        )

runSharedRep :: SharedRepF m r a -> m (RepF r a, (Int, HashMap ByteString ByteString))
runSharedRep s = flip runStateT (0, HashMap.empty) $ unshare s

-- | compute the initial state of a SharedRep (testing)
zeroState ::
  (Monad m) =>
  SharedRep m a ->
  m (Markup, (HashMap ByteString ByteString, Either ByteString a))
zeroState sr = do
  (Rep h fa, (_, m)) <- runSharedRep sr
  pure (h, fa m)

-- | Compute the initial state of a SharedRep and then run an action once (see tests).
runOnce ::
  (Monad m) =>
  SharedRep m a ->
  (Markup -> HashMap ByteString ByteString -> m ()) ->
  m (HashMap ByteString ByteString, Either ByteString a)
runOnce sr action = do
  (Rep h fa, (_, m)) <- runSharedRep sr
  action h m
  pure (fa m)
