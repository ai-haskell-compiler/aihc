{-# LANGUAGE KindSignatures #-}

-- Control.Monad imports this module, so the Kleisli composition cannot
-- use its Kleisli operators.
{-# HLINT ignore "Use <=<" #-}
{-# HLINT ignore "Use >=>" #-}

module Control.Arrow
  ( Arrow (..),
    Kleisli (..),
    returnA,
    (^>>),
    (>>^),
    (>>>),
    (<<<),
    (<<^),
    (^<<),
    ArrowZero (..),
    ArrowPlus (..),
    ArrowChoice (..),
    ArrowApply (..),
    ArrowMonad (..),
    leftApp,
    ArrowLoop (..),
  )
where

import Control.Category (Category (..), (<<<), (>>>))
import Control.Monad.Fix (MonadFix (..))
import Data.Kind (Type)
import Prelude hiding (id, (.))

infixr 5 <+>

infixr 3 ***

infixr 3 &&&

infixr 2 +++

infixr 2 |||

infixr 1 ^>>, >>^

infixr 1 ^<<, <<^

class (Category a) => Arrow (a :: Type -> Type -> Type) where
  {-# MINIMAL arr, (first | (***)) #-}
  arr :: (b -> c) -> a b c

  first :: a b c -> a (b, d) (c, d)
  first = (*** id)

  second :: a b c -> a (d, b) (d, c)
  second = (id ***)

  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  f *** g = first f >>> arr swap >>> first g >>> arr swap
    where
      swap ~(x, y) = (y, x)

  (&&&) :: a b c -> a b c' -> a b (c, c')
  f &&& g = arr (\b -> (b, b)) >>> f *** g

instance Arrow (->) where
  arr f = f
  first f (b, d) = (f b, d)
  second f (d, b) = (d, f b)
  (f *** g) ~(b, b') = (f b, g b')
  (f &&& g) b = (f b, g b)

-- | Kleisli arrows of a monad.
newtype Kleisli m a b = Kleisli {runKleisli :: a -> m b}

instance (Functor m) => Functor (Kleisli m a) where
  fmap f (Kleisli g) = Kleisli (fmap f . g)

instance (Applicative m) => Applicative (Kleisli m a) where
  pure = Kleisli . const . pure
  Kleisli f <*> Kleisli g = Kleisli (\x -> f x <*> g x)
  Kleisli f *> Kleisli g = Kleisli (\x -> f x *> g x)
  Kleisli f <* Kleisli g = Kleisli (\x -> f x <* g x)

instance (Monad m) => Monad (Kleisli m a) where
  Kleisli f >>= k = Kleisli (\x -> f x >>= \a -> runKleisli (k a) x)

instance (Monad m) => Category (Kleisli m) where
  id = Kleisli return
  Kleisli f . Kleisli g = Kleisli (\b -> f =<< g b)

instance (Monad m) => Arrow (Kleisli m) where
  arr f = Kleisli (return . f)
  first (Kleisli f) = Kleisli (\ ~(b, d) -> f b >>= \c -> return (c, d))
  second (Kleisli f) = Kleisli (\ ~(d, b) -> f b >>= \c -> return (d, c))

-- | The identity arrow, which plays the role of 'return' in arrow notation.
returnA :: (Arrow a) => a b b
returnA = id

-- | Precomposition with a pure function.
(^>>) :: (Arrow a) => (b -> c) -> a c d -> a b d
f ^>> a = arr f >>> a

-- | Postcomposition with a pure function.
(>>^) :: (Arrow a) => a b c -> (c -> d) -> a b d
a >>^ f = a >>> arr f

-- | Precomposition with a pure function (right-to-left variant).
(<<^) :: (Arrow a) => a c d -> (b -> c) -> a b d
a <<^ f = a <<< arr f

-- | Postcomposition with a pure function (right-to-left variant).
(^<<) :: (Arrow a) => (c -> d) -> a b c -> a b d
f ^<< a = arr f <<< a

class (Arrow a) => ArrowZero a where
  zeroArrow :: a b c

-- | A monoid on arrows.
class (ArrowZero a) => ArrowPlus a where
  -- | An associative operation with identity 'zeroArrow'.
  (<+>) :: a b c -> a b c -> a b c

-- | Choice, for arrows that support it.
class (Arrow a) => ArrowChoice a where
  {-# MINIMAL (left | (+++)) #-}

  -- | Feed marked inputs through the argument arrow, passing the
  --   rest through unchanged to the output.
  left :: a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  -- | A mirror image of 'left'.
  right :: a b c -> a (Either d b) (Either d c)
  right = (id +++)

  -- | Split the input between the two argument arrows, retagging
  --   and merging their outputs.
  (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
  f +++ g = left f >>> arr mirror >>> left g >>> arr mirror
    where
      mirror :: Either x y -> Either y x
      mirror (Left x) = Right x
      mirror (Right y) = Left y

  -- | Fanin: Split the input between the two argument arrows and
  --   merge their outputs.
  (|||) :: a b d -> a c d -> a (Either b c) d
  f ||| g = f +++ g >>> arr untag
    where
      untag (Left x) = x
      untag (Right y) = y

instance ArrowChoice (->) where
  left f = f +++ id
  right f = id +++ f
  f +++ g = (Left . f) ||| (Right . g)
  (|||) = either

instance (Monad m) => ArrowChoice (Kleisli m) where
  left f = f +++ arr id
  right f = arr id +++ f
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  Kleisli f ||| Kleisli g = Kleisli (either f g)

-- | Some arrows allow application of arrow inputs to other inputs.
class (Arrow a) => ArrowApply a where
  app :: a (a b c, b) c

instance ArrowApply (->) where
  app (f, x) = f x

instance (Monad m) => ArrowApply (Kleisli m) where
  app = Kleisli (\(Kleisli f, x) -> f x)

-- | The 'ArrowApply' class is equivalent to 'Monad': any monad gives rise
--   to a 'Kleisli' arrow, and any instance of 'ArrowApply' defines a monad.
newtype ArrowMonad a b = ArrowMonad (a () b)

instance (Arrow a) => Functor (ArrowMonad a) where
  fmap f (ArrowMonad m) = ArrowMonad (m >>^ f)

instance (Arrow a) => Applicative (ArrowMonad a) where
  pure x = ArrowMonad (arr (const x))
  ArrowMonad f <*> ArrowMonad x = ArrowMonad (f &&& x >>> arr (uncurry id))

instance (ArrowApply a) => Monad (ArrowMonad a) where
  ArrowMonad m >>= f =
    ArrowMonad (m >>> arr (\x -> let ArrowMonad h = f x in (h, ())) >>> app)

-- | Any instance of 'ArrowApply' can be made into an instance of
--   'ArrowChoice' by defining 'left' = 'leftApp'.
leftApp :: (ArrowApply a) => a b c -> a (Either b d) (Either c d)
leftApp f =
  arr
    ( (\b -> (arr (\() -> b) >>> f >>> arr Left, ()))
        ||| (\d -> (arr (\() -> d) >>> arr Right, ()))
    )
    >>> app

-- | The 'loop' operator expresses computations in which an output value
--   is fed back as input, although the computation occurs only once.
class (Arrow a) => ArrowLoop a where
  loop :: a (b, d) (c, d) -> a b c

instance ArrowLoop (->) where
  loop f b = let (c, d) = f (b, d) in c

instance (MonadFix m) => ArrowLoop (Kleisli m) where
  loop (Kleisli f) = Kleisli (fmap fst . mfix . f')
    where
      f' x y = f (x, snd y)
