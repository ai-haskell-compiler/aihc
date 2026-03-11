-- | 'Committer' wraps a consumer destructor.
--
-- "Commitment is an act, not a word." ~ Jean-Paul Sartre
module Box.Committer
  ( Committer (..),
    CoCommitter,
    witherC,
    listC,
    push,
  )
where

import Box.Codensity (Codensity)
import Box.Functor
import Control.Monad.State.Lazy
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Sequence qualified as Seq
import Data.Void
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude
-- >>> import Box
-- >>> import Data.Bool

-- | A Committer 'commit's values of type a and signals success or otherwise. A sink or a consumer are some other metaphors for this. A Committer absorbs the value being committed; the value disappears into the opaque thing that is a Committer from the pov of usage.
--
-- >>> commit toStdout "I'm committed!"
-- I'm committed!
-- True
newtype Committer m a = Committer
  { commit :: a -> m Bool
  }

-- | 'Committer' continuation.
type CoCommitter m a = Codensity m (Committer m a)

instance FFunctor Committer where
  foist nat (Committer c) = Committer $ nat . c

instance (Applicative m) => Semigroup (Committer m a) where
  (<>) i1 i2 = Committer (\a -> (||) <$> commit i1 a <*> commit i2 a)

instance (Applicative m) => Monoid (Committer m a) where
  mempty = Committer (\_ -> pure False)

  mappend = (<>)

instance Contravariant (Committer m) where
  contramap f (Committer a) = Committer (a . f)

instance (Applicative m) => Divisible (Committer m) where
  conquer = Committer (\_ -> pure False)

  divide f i1 i2 =
    Committer $ \a ->
      case f a of
        (b, c) -> (||) <$> commit i1 b <*> commit i2 c

instance (Applicative m) => Decidable (Committer m) where
  lose f = Committer (absurd . f)

  choose f i1 i2 =
    Committer $ \a ->
      case f a of
        Left b -> commit i1 b
        Right c -> commit i2 c

-- | A monadic [Witherable](https://hackage.haskell.org/package/witherable)
--
-- >>> glue (witherC (\x -> pure $ bool Nothing (Just x) (even x)) showStdout) <$|> qList [0..5]
-- 0
-- 2
-- 4
witherC :: (Monad m) => (b -> m (Maybe a)) -> Committer m a -> Committer m b
witherC f c = Committer go
  where
    go b = do
      fb <- f b
      case fb of
        Nothing -> pure True
        Just fb' -> commit c fb'

-- | Convert a committer to be a list committer.
--
-- >>> glue showStdout <$|> qList [[1..3]]
-- [1,2,3]
--
-- >>>  glue (listC showStdout) <$|> qList [[1..3]]
-- 1
-- 2
-- 3
listC :: (Monad m) => Committer m a -> Committer m [a]
listC c = Committer $ fmap or . mapM (commit c)

-- | Push to a state sequence.
--
-- >>> import Control.Monad.State.Lazy
-- >>> import qualified Data.Sequence as Seq
-- >>> flip execStateT Seq.empty . glue push . foist lift <$|> qList [1..3]
-- fromList [1,2,3]
push :: (Monad m) => Committer (StateT (Seq.Seq a) m) a
push = Committer $ \a -> do
  modify (Seq.:|> a)
  pure True
