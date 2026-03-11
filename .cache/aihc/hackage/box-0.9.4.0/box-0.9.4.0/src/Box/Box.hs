-- | A box is a product of a consumer and producer destructor.
--
-- Consumers and producers (committers and emitters) are paired in two ways:
--
-- - As two ends of a resource such as a file, or screen or queue; input to and output from.
--
-- - As the start and end of a computation pipeline.
module Box.Box
  ( Box (..),
    CoBox,
    CoBoxM (..),
    bmap,
    foistb,
    glue,
    Closure (..),
    glue',
    glueN,
    glueES,
    glueS,
    fuse,
    Divap (..),
    DecAlt (..),
    cobox,
    seqBox,
  )
where

import Box.Codensity
import Box.Committer
import Box.Emitter
import Box.Functor
import Control.Applicative
  ( Alternative (empty, (<|>)),
    Applicative (liftA2),
  )
import Control.Monad
import Control.Monad.State.Lazy
import Data.Bool
import Data.Function
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible
  ( Decidable (choose, lose),
    Divisible (conquer, divide),
  )
import Data.Profunctor (Profunctor (dimap))
import Data.Semigroupoid
import Data.Sequence qualified as Seq
import Data.Void (Void, absurd)
import Prelude hiding (id, liftA2, (.))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude
-- >>> import Box
-- >>> import Data.Text (pack)
-- >>> import Data.Bool
-- >>> import Control.Monad.State.Lazy

-- | A Box is a product of a 'Committer' and an 'Emitter'.
--
-- Think of a box with an incoming arrow an outgoing arrow. And then make your pov ambiguous: are you looking at two wires from "inside a box"; or are you looking from "outside the box"; interacting with a black box object. Either way, it looks the same: it's a box.
--
-- And either way, one of the arrows, the 'Committer', is contravariant and the other, the 'Emitter' is covariant. The combination is a profunctor.
data Box m c e = Box
  { committer :: Committer m c,
    emitter :: Emitter m e
  }

-- | Wrong kind signature for the FFunctor class
foistb :: (forall a. m a -> n a) -> Box m c e -> Box n c e
foistb nat (Box c e) = Box (foist nat c) (foist nat e)

instance (Functor m) => Profunctor (Box m) where
  dimap f g (Box c e) = Box (contramap f c) (fmap g e)

instance (Alternative m, Monad m) => Semigroup (Box m c e) where
  (<>) (Box c e) (Box c' e') = Box (c <> c') (e <> e')

instance (Alternative m, Monad m) => Monoid (Box m c e) where
  mempty = Box mempty mempty
  mappend = (<>)

-- | A profunctor dimapMaybe
bmap :: (Monad m) => (a' -> m (Maybe a)) -> (b -> m (Maybe b')) -> Box m a b -> Box m a' b'
bmap fc fe (Box c e) = Box (witherC fc c) (witherE fe e)

-- | Connect an emitter directly to a committer of the same type.
--
-- >>> glue showStdout <$|> qList [1..3]
-- 1
-- 2
-- 3
glue :: (Monad m) => Committer m a -> Emitter m a -> m ()
glue c e = fix $ \rec -> emit e >>= maybe (pure False) (commit c) >>= bool (pure ()) rec

-- | Whether the committer or emitter closed the computation.
data Closure = CommitterClosed | EmitterClosed deriving (Eq, Show, Ord)

-- | Connect an emitter directly to a committer of the same type, returning whether the emitter or committer caused eventual closure.
--
-- >>> glue' showStdout <$|> qList [1..3]
-- 1
-- 2
-- 3
-- EmitterClosed
glue' :: (Monad m) => Committer m a -> Emitter m a -> m Closure
glue' c e =
  fix $ \rec ->
    emit e
      >>= maybe
        (pure EmitterClosed)
        (commit c >=> bool (pure CommitterClosed) rec)

-- | Connect a Stateful emitter to a (non-stateful) committer of the same type, supplying initial state.
--
-- >>> glueES 0 (showStdout) <$|> (takeE 2 <$> qList [1..3])
-- 1
-- 2
glueES :: (Monad m) => s -> Committer m a -> Emitter (StateT s m) a -> m ()
glueES s c e = flip evalStateT s $ glue (foist lift c) e

-- | Connect a Stateful emitter to a (similarly-stateful) committer of the same type, supplying initial state.
--
-- >>> glueS 0 (foist lift showStdout) <$|> (takeE 2 <$> qList [1..3])
-- 1
-- 2
glueS :: (Monad m) => s -> Committer (StateT s m) a -> Emitter (StateT s m) a -> m ()
glueS s c e = flip evalStateT s $ glue c e

-- | Glues a committer and emitter, and takes n emits
--
-- >>> glueN 3 <$> pure showStdout <*|> qList [1..]
-- 1
-- 2
-- 3
--
-- Note that glueN counts the number of events passing across the connection and doesn't take into account post-transmission activity in the Committer, eg
--
-- >>> glueN 4 (witherC (\x -> bool (pure Nothing) (pure (Just x)) (even x)) showStdout) <$|> qList [0..9]
-- 0
-- 2
glueN :: (Monad m) => Int -> Committer m a -> Emitter m a -> m ()
glueN n c e = flip evalStateT 0 $ glue (foist lift c) (takeE n e)

-- | Glue a Committer to an Emitter within a box.
--
-- > fuse (pure . pure) == \(Box c e) -> glue c e
--
-- A command-line echoer
--
-- > fuse (pure . Just . ("echo " <>)) (Box toStdout fromStdin)
fuse :: (Monad m) => (a -> m (Maybe b)) -> Box m b a -> m ()
fuse f (Box c e) = glue c (witherE f e)

-- | combines 'divide' 'conquer' and 'liftA2' 'pure'
class Divap p where
  divap :: (a -> (b, c)) -> ((d, e) -> f) -> p b d -> p c e -> p a f
  conpur :: a -> p b a

instance (Applicative m) => Divap (Box m) where
  divap split' merge (Box lc le) (Box rc re) =
    Box (divide split' lc rc) (liftA2 (curry merge) le re)

  conpur a = Box conquer (pure a)

-- | combines 'Decidable' and 'Alternative'
class (Profunctor p) => DecAlt p where
  choice :: (a -> Either b c) -> (Either d e -> f) -> p b d -> p c e -> p a f
  loss :: p Void b

instance (Monad m, Alternative m) => DecAlt (Box m) where
  choice split' merge (Box lc le) (Box rc re) =
    Box (choose split' lc rc) (fmap merge $ fmap Left le <|> fmap Right re)
  loss = Box (lose absurd) empty

-- | A box continuation
type CoBox m a b = Codensity m (Box m a b)

-- | Construct a CoBox
--
--
-- > cobox = Box <$> c <*> e
--
-- >>> fuse (pure . Just . ("show: " <>) . pack . show) <$|> (cobox (pure toStdout) (qList [1..3]))
-- show: 1
-- show: 2
-- show: 3
cobox :: CoCommitter m a -> CoEmitter m b -> CoBox m a b
cobox c e = Box <$> c <*> e

-- | State monad queue.
seqBox :: (Monad m) => Box (StateT (Seq.Seq a) m) a a
seqBox = Box push pop

-- | cps composition of monadic boxes
dotco :: (Monad m) => Codensity m (Box m a b) -> Codensity m (Box m b c) -> Codensity m (Box m a c)
dotco b b' = lift $ do
  (Box c e) <- lowerCodensity b
  (Box c' e') <- lowerCodensity b'
  glue c' e
  pure (Box c e')

-- | Wrapper for the semigroupoid instance of a box continuation.
newtype CoBoxM m a b = CoBoxM {uncobox :: Codensity m (Box m a b)}

instance (Monad m) => Semigroupoid (CoBoxM m) where
  o (CoBoxM b) (CoBoxM b') = CoBoxM (dotco b' b)
