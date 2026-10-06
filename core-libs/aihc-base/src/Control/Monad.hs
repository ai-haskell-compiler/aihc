module Control.Monad
  ( Functor (..),
    Applicative (..),
    Monad (..),
    MonadFail (..),
    MonadPlus (..),
    ap,
    liftM,
    liftM2,
    liftM3,
    liftM4,
    liftM5,
    (=<<),
    (>=>),
    (<=<),
    (<$!>),
    mapM,
    mapM_,
    forM,
    forM_,
    sequence,
    sequence_,
    when,
    unless,
    foldM,
    foldM_,
    forever,
    void,
    join,
    replicateM,
    replicateM_,
    zipWithM,
    zipWithM_,
    mapAndUnzipM,
    filterM,
    guard,
    mzero,
    mplus,
    msum,
    mfilter,
  )
where

import Control.Applicative (Alternative (..), WrappedMonad (..))
import Control.Arrow (ArrowPlus (..), ArrowZero (..), Kleisli (..))
import Control.Monad.Fail (MonadFail (..))
import Data.Functor (void)
import Prelude
  ( Applicative (..),
    Bool (..),
    Foldable,
    Functor (..),
    Int,
    Maybe,
    Monad (..),
    Num (..),
    Ord (..),
    Traversable,
    const,
    flip,
    foldr,
    id,
    mapM,
    mapM_,
    seq,
    sequence,
    sequence_,
    traverse,
    unzip,
    (<$>),
    (=<<),
  )

-- | These combinators bind and return, as GHC defines them: an instance
-- defines @fmap = liftM@ and @(<*>) = ap@ in terms of them, so they must not
-- call 'fmap' or '(<*>)' themselves. HLint would turn the last bind of each
-- into @<$>@, which is a call of 'fmap'.

{- HLINT ignore ap "Use <$>" -}
{- HLINT ignore liftM "Use <$>" -}
{- HLINT ignore liftM2 "Use <$>" -}
{- HLINT ignore liftM3 "Use <$>" -}
{- HLINT ignore liftM4 "Use <$>" -}
{- HLINT ignore liftM5 "Use <$>" -}
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap function argument = do
  selected <- function
  value <- argument
  return (selected value)

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM function action = do
  value <- action
  return (function value)

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 function left right = do
  leftValue <- left
  rightValue <- right
  return (function leftValue rightValue)

liftM3 :: (Monad m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 function first second third = do
  firstValue <- first
  secondValue <- second
  thirdValue <- third
  return (function firstValue secondValue thirdValue)

liftM4 :: (Monad m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftM4 function first second third fourth = do
  firstValue <- first
  secondValue <- second
  thirdValue <- third
  fourthValue <- fourth
  return (function firstValue secondValue thirdValue fourthValue)

liftM5 :: (Monad m) => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
liftM5 function first second third fourth fifth = do
  firstValue <- first
  secondValue <- second
  thirdValue <- third
  fourthValue <- fourth
  fifthValue <- fifth
  return (function firstValue secondValue thirdValue fourthValue fifthValue)

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

  mzero = empty
  mplus = (<|>)

instance MonadPlus []

instance MonadPlus Maybe

-- GHC declares these instances beside 'Kleisli'. 'Control.Arrow' cannot
-- import this module, so they live beside 'MonadPlus'.
instance (MonadPlus m) => MonadPlus (Kleisli m a)

instance (MonadPlus m) => ArrowZero (Kleisli m) where
  zeroArrow = Kleisli (const mzero)

instance (MonadPlus m) => ArrowPlus (Kleisli m) where
  Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)

-- GHC declares this instance beside 'WrappedMonad'. 'Control.Applicative'
-- cannot import this module, so it lives beside 'MonadPlus'.
instance (MonadPlus m) => Alternative (WrappedMonad m) where
  empty = WrapMonad mzero
  WrapMonad left <|> WrapMonad right = WrapMonad (left `mplus` right)

instance (MonadPlus m) => MonadPlus (WrappedMonad m)

(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) first second value = first value >>= second

infixr 1 >=>

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = flip (>=>)

infixr 1 <=<

(<$!>) :: (Monad m) => (a -> b) -> m a -> m b
function <$!> action = do
  value <- action
  let result = function value
  result `seq` return result

infixl 4 <$!>

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM = flip mapM

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = flip mapM_

when :: (Applicative f) => Bool -> f () -> f ()
when True action = action
when False _ = pure ()

unless :: (Applicative f) => Bool -> f () -> f ()
unless True _ = pure ()
unless False action = action

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM combine initial values = foldr step return values initial
  where
    step value continue accumulator = combine accumulator value >>= continue

{- HLINT ignore foldM_ "Use foldM_" -}
foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
foldM_ combine initial values = void (foldM combine initial values)

forever :: (Monad m) => m a -> m b
forever action = action >> forever action

join :: (Monad m) => m (m a) -> m a
join action = action >>= id

replicateM :: (Applicative m) => Int -> m a -> m [a]
replicateM count action =
  if count <= 0
    then pure []
    else liftA2 (:) action (replicateM (count - 1) action)

replicateM_ :: (Applicative m) => Int -> m a -> m ()
replicateM_ count action =
  if count <= 0
    then pure ()
    else action *> replicateM_ (count - 1) action

zipWithM :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM combine (left : lefts) (right : rights) = do
  value <- combine left right
  values <- zipWithM combine lefts rights
  return (value : values)
zipWithM _ _ _ = return []

{- HLINT ignore zipWithM_ "Use zipWithM_" -}
zipWithM_ :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ combine lefts rights = void (zipWithM combine lefts rights)

mapAndUnzipM :: (Applicative m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapAndUnzipM function values = unzip <$> traverse function values

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM keep (value : values) = do
  selected <- keep value
  rest <- filterM keep values
  return (if selected then value : rest else rest)

guard :: (Alternative f) => Bool -> f ()
guard True = pure ()
guard False = empty

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero

-- | Keep the result of the action when it satisfies the predicate. Give
-- 'mzero' when it does not.
mfilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a
mfilter keep action = do
  value <- action
  if keep value then return value else mzero
