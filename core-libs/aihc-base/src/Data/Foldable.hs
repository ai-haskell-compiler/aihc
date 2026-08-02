{-# LANGUAGE KindSignatures #-}

module Data.Foldable
  ( Foldable (..),
    foldrM,
    foldlM,
    traverse_,
    for_,
    sequenceA_,
    asum,
    mapM_,
    forM_,
    sequence_,
    msum,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    maximumBy,
    minimumBy,
    notElem,
    find,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Prelude
  ( Applicative (..),
    Bool (..),
    Either (..),
    Eq (..),
    Functor (..),
    Int,
    Maybe (..),
    Monad (..),
    Num (..),
    Ord (..),
    Ordering (..),
    id,
    not,
    (&&),
    (++),
    (.),
    (||),
  )

class Foldable (t :: Type -> Type) where
  fold :: (Monoid m) => t m -> m
  foldMap :: (Monoid m) => (a -> m) -> t a -> m
  foldMap' :: (Monoid m) => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: (Eq a) => a -> t a -> Bool
  maximum :: (Ord a) => t a -> a
  minimum :: (Ord a) => t a -> a
  sum :: (Num a) => t a -> a
  product :: (Num a) => t a -> a

  fold = foldMap identityFoldable
  foldMap f = foldr (\value rest -> f value <> rest) mempty
  foldMap' f = foldl' (\rest value -> rest <> f value) mempty
  foldr f initial structure = applyEndo (foldMap (Endo . f) structure) initial
  foldr' f initial structure = foldl strictRightStep id structure initial
    where
      strictRightStep continuation value rest =
        case f value rest of
          result -> continuation result
  foldl f initial structure = foldr leftStep id structure initial
    where
      leftStep value continuation rest = continuation (f rest value)
  foldl' f initial structure = foldr strictLeftStep id structure initial
    where
      strictLeftStep value continuation rest =
        case f rest value of
          result -> continuation result
  foldr1 f structure = fromMaybeFoldable emptyStructure (foldr rightStep Nothing structure)
    where
      rightStep value Nothing = Just value
      rightStep value (Just rest) = Just (f value rest)
  foldl1 f structure = fromMaybeFoldable emptyStructure (foldl leftStep Nothing structure)
    where
      leftStep Nothing value = Just value
      leftStep (Just rest) value = Just (f rest value)
  toList = foldr (:) []
  null = foldr (\_ _ -> False) True
  length = foldl' (\count _ -> count + 1) 0
  elem target = foldr (\value rest -> value == target || rest) False
  maximum = foldr1 maximumValue
  minimum = foldr1 minimumValue
  sum = foldl' (+) 0
  product = foldl' (*) 1

infix 4 `elem`

identityFoldable :: a -> a
identityFoldable value = value

fromMaybeFoldable :: a -> Maybe a -> a
fromMaybeFoldable fallback Nothing = fallback
fromMaybeFoldable _ (Just value) = value

maximumValue :: (Ord a) => a -> a -> a
maximumValue = max

minimumValue :: (Ord a) => a -> a -> a
minimumValue = min

newtype Endo a = Endo (a -> a)

applyEndo :: Endo a -> a -> a
applyEndo (Endo f) = f

instance Semigroup (Endo a) where
  Endo left <> Endo right = Endo (left . right)

instance Monoid (Endo a) where
  mempty = Endo id

emptyStructure :: a
emptyStructure = emptyStructure

instance Foldable [] where
  foldr _ initial [] = initial
  foldr f initial (value : values) = f value (foldr f initial values)

  foldl _ initial [] = initial
  foldl f initial (value : values) = foldl f (f initial value) values

  foldl' _ initial [] = initial
  foldl' f initial (value : values) =
    case f initial value of
      result -> foldl' f result values

  null [] = True
  null (_ : _) = False

instance Foldable Maybe where
  foldr _ initial Nothing = initial
  foldr f initial (Just value) = f value initial

  foldl _ initial Nothing = initial
  foldl f initial (Just value) = f initial value

  null Nothing = True
  null (Just _) = False

instance Foldable (Either e) where
  foldr _ initial (Left _) = initial
  foldr f initial (Right value) = f value initial

  foldl _ initial (Left _) = initial
  foldl f initial (Right value) = f initial value

  null (Left _) = True
  null (Right _) = False

instance Foldable ((,) e) where
  foldr f initial (_, value) = f value initial
  foldl f initial (_, value) = f initial value
  null _ = False

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f initial structure = foldl step pure structure initial
  where
    step continuation value rest = f value rest >>= continuation

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f initial structure = foldr step pure structure initial
  where
    step value continuation rest = f rest value >>= continuation

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr (thenApplicative . f) (pure ())

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ structure f = traverse_ f structure

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr thenApplicative (pure ())

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ = traverse_

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = for_

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = sequenceA_

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero

concat :: (Foldable t) => t [a] -> [a]
concat = foldr (++) []

concatMap :: (Foldable t) => (a -> [b]) -> t a -> [b]
concatMap f = foldr (\value rest -> f value ++ rest) []

and :: (Foldable t) => t Bool -> Bool
and = foldr (&&) True

or :: (Foldable t) => t Bool -> Bool
or = foldr (||) False

any :: (Foldable t) => (a -> Bool) -> t a -> Bool
any predicate = foldr (\value rest -> predicate value || rest) False

all :: (Foldable t) => (a -> Bool) -> t a -> Bool
all predicate = foldr (\value rest -> predicate value && rest) True

maximumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumBy compareValues = foldr1 choose
  where
    choose left right =
      case compareValues left right of
        GT -> left
        _ -> right

minimumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
minimumBy compareValues = foldr1 choose
  where
    choose left right =
      case compareValues left right of
        GT -> right
        _ -> left

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem target structure = not (target `elem` structure)

infix 4 `notElem`

find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find predicate = foldr choose Nothing
  where
    choose value rest =
      case predicate value of
        True -> Just value
        False -> rest

thenApplicative :: (Applicative f) => f a -> f b -> f b
thenApplicative first second = fmap (\_ value -> value) first <*> second
