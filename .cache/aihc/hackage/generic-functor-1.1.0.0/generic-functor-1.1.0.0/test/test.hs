{-# LANGUAGE
  DeriveFoldable,
  DeriveGeneric,
  DerivingVia,
  StandaloneDeriving,
  TypeOperators #-}

import Control.Monad.Trans.State ( evalState, state, State )
import Data.Bifoldable ( Bifoldable() )
import Data.Bifunctor ( Bifunctor(bimap) )
import GHC.Generics ( Generic )
import System.Exit (exitFailure)

import Generic.Functor
    ( gtraverse,
      GenericBifunctor(..),
      GenericFunctor(..) )
import Generic.Functor.Multimap
    ( gmultimap,
      gsolomap,
      multimap,
      solomap,
      type (:+)((:+)) )

-- Testing GenericFunctor (gfmap) and gsolomap

data Empty a
  deriving Generic
  deriving (Functor, Foldable) via (GenericFunctor Empty)

data Unit a = Unit
  deriving (Eq, Show, Generic)
  deriving (Functor, Foldable) via (GenericFunctor Unit)

data Result a r = Error a | Ok r
  deriving (Eq, Show, Generic)
  deriving (Functor, Foldable) via (GenericFunctor (Result a))

instance Traversable Empty where
  traverse = gtraverse

instance Traversable Unit where
  traverse = gtraverse

instance Traversable (Result a) where
  traverse = gtraverse
  
mapError :: (a -> b) -> Result a r -> Result b r
mapError = gsolomap

mapOk :: (r -> s) -> Result a r -> Result a s
mapOk = gsolomap

mapBoth :: (a -> b) -> Result a a -> Result b b
mapBoth = gsolomap

data Writer w a = Writer w a
  deriving (Eq, Show, Generic)
  deriving (Functor, Foldable) via (GenericFunctor (Writer w))

instance Traversable (Writer w) where
  traverse = gtraverse

mapW :: (w -> w') -> Writer w a -> Writer w' a
mapW = gsolomap

data Square a b = Square a a b b
  deriving (Eq, Show, Generic)
  deriving (Functor, Foldable) via (GenericFunctor (Square a))

instance Traversable (Square a) where
  traverse = gtraverse

mapFirst :: (a -> a') -> Square a b -> Square a' b
mapFirst = gsolomap

data Twice a = Twice (Either a a)
  deriving (Eq, Show, Generic)
  deriving (Functor, Foldable) via (GenericFunctor Twice)

instance Traversable Twice where
  traverse = gtraverse

-- Testing solomap

map1, map1' :: (a -> b) -> Either e (Maybe [(e, a)]) -> Either e (Maybe [(e, b)])
map1 = solomap
map1' = fmap . fmap . fmap . fmap  -- equivalent definition, just making sure it typechecks

map2, map2' :: (a -> b) -> (e -> Either [a] r) -> (e -> Either [b] r)
map2 = solomap
map2' f = fmap (bimap (fmap f) id)

type F a = ([a], Either a ())

map3, map3' :: (a -> b) -> F a -> F b
map3 = solomap
map3' f = bimap (fmap f) (bimap f id)

type G t a = (t, Maybe [Either Bool a])

map4, map4', map4'' :: (a -> b) -> G t a -> G t b
map4 = solomap
map4' = fmap . fmap . fmap . fmap
map4'' = multimap

map6, map6' :: (a -> b) -> (c -> d) -> G a c -> G b d
map6 f g = multimap (f :+ g)
map6' f = bimap f . fmap . fmap . fmap

-- Deriving Bifunctor

data Tree a b = Node a (Tree a b) (Tree a b) | Leaf b
  deriving (Eq, Show, Generic)
  deriving (Functor, Foldable) via (GenericFunctor (Tree a))
  deriving (Bifunctor, Bifoldable) via (GenericBifunctor Tree)

instance Traversable (Tree a) where
  traverse = gtraverse

data CofreeF f a b = a :< f b
  deriving (Eq, Show, Generic)
  deriving (Functor, Foldable) via (GenericFunctor (CofreeF f a))
  deriving (Bifunctor, Bifoldable) via (GenericBifunctor (CofreeF f))

instance Traversable f => Traversable (CofreeF f a) where
  traverse = gtraverse

bimapCofreeF' :: Functor f => (a -> b) -> (c -> d) -> CofreeF f a c -> CofreeF f b d
bimapCofreeF' f g = gmultimap (f :+ g)

-- Multimap

data Three a b c = One a | Two b | Three c
  deriving (Eq, Show, Generic)
  deriving (Functor, Foldable) via (GenericFunctor (Three a b))

instance Traversable (Three a b) where
  traverse = gtraverse

mapThree :: (a -> a') -> (b -> b') -> (c -> c') -> Three a b c -> Three a' b' c'
mapThree f g h = gmultimap (f :+ g :+ h)

type F5 a b c = Either a (b, c)

map5, map5' :: (a -> a') -> (b -> b') -> (c -> c') -> F5 a b c -> F5 a' b' c'
map5 f g h = multimap (f :+ g :+ h)
map5' f g h = bimap f (bimap g h)

-- Run at least once

twice :: Int -> Int
twice = (* 2)

foldTest :: Foldable t => t Int -> Int
foldTest = sum  -- specialized

tick :: Int -> State Int Int
tick _ = state (\i -> (i, i+1))

travTest :: Traversable t => t Int -> t Int
travTest = (`evalState` 0) . traverse tick

main :: IO ()
main = do
  Unit @= fmap twice Unit
  Ok 8 @= fmap twice (Ok 4 :: Result () Int)
  Error 8 @= mapError twice (Error 4 :: Result Int ())
  Writer () 8 @= fmap twice (Writer () 4)
  Writer 8 () @= mapW twice (Writer 4 ())
  Square () () 8 10 @= fmap twice (Square () () 4 5)
  Square 8 10 () () @= mapFirst twice (Square 4 5 () ())
  [Twice (Left 8), Twice (Right 10)] @= (fmap . fmap) twice [Twice (Left 4), Twice (Right 5)]
  Node 8 (Leaf 10) (Leaf 12) @= bimap twice twice (Node 4 (Leaf 5) (Leaf 6))
  (8 :< Just 10) @= bimap twice twice (4 :< Just 5)
  [One 8, Two False, Three 1] @= fmap (mapThree twice not length) [One 4, Two True, Three [()]]

  let t1 = Right (Just [((), 4)])
  map1 twice t1 @= map1' twice t1

  let t2 x = Left [x] :: Either [Int] ()
  map2 twice t2 4 @= map2' twice t2 4

  let t3 = ([4], Left 5)
  map3 twice t3 @= map3' twice t3

  let t4 = ((), Just [Right 4])
  map4 twice t4 @= map4' twice t4
  map4 twice t4 @= map4'' twice t4

  map6 (const True) twice t4 @= map6' (const True) twice t4

  let t5 = [Left 4, Right (False, [()])]
  fmap (map5 twice not length) t5 @= fmap (map5 twice not length) t5

  -- Foldable
  0 @= foldTest Unit
  4 @= foldTest (Ok 4 :: Result () Int)
  0 @= foldTest (Error 4 :: Result Int Int)
  4 @= foldTest (Writer () 4)
  9 @= foldTest (Square () () 4 5)
  3 @= foldTest (Node 4 (Leaf 1) (Leaf 2) :: Tree Int Int)

  -- Traversable
  Unit @= travTest Unit
  Ok 0 @= travTest (Ok 4 :: Result () Int)
  Error 4 @= travTest (Error 4 :: Result Int Int)
  Writer () 0 @= travTest (Writer () 4)
  Square () () 0 1 @= travTest (Square () () 4 5)
  Node 4 (Leaf 0) (Leaf 1) @= travTest (Node 4 (Leaf 5) (Leaf 6) :: Tree Int Int)

-- Assert equality
(@=) :: (Eq a, Show a) => a -> a -> IO ()
(@=) x y | x == y = pure ()
         | otherwise = do
  putStrLn "Not equal:"
  print x
  print y
  exitFailure
