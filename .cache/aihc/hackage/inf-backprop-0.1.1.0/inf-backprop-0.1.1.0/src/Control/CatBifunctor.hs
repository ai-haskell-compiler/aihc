{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  Control.CatBifunctor
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Categorical Bifunctor typeclass and its trivial instances.
module Control.CatBifunctor
  ( CatBiFunctor,
    first,
    second,
    (***),
  )
where

import Control.Applicative (liftA2)
import Control.Arrow (Kleisli (Kleisli), (>>>))
import Control.Category (Category, id)
import Control.Comonad (Cokleisli (Cokleisli), Comonad, liftW)
import Data.Bifunctor (bimap)
import GHC.Base (Type)
import Prelude (Either (Left, Right), Monad, fmap, fst, snd, ($))

-- | Categorical generalization for bifunctor with arrow notations.
-- Notice that we do NOT require the categorical morphism '(>>>)'
-- and morphism tensor product '(***)' are interchangeable. Namely,
--
-- @ (f >>> g) *** (h >>> l) != (f *** h) >>> (g *** l) @
--
-- in general.
--
-- ==== __Monad and type product instance examples of usage __
--
-- >>> import Prelude (Int, pure, Maybe(Just, Nothing), const, replicate, String)
-- >>> import Control.Arrow (Kleisli(Kleisli), runKleisli)
--
-- >>> runKleisli (Kleisli pure *** Kleisli pure) (1,2) :: [(Int, Int)]
-- [(1,2)]
--
-- >>> runKleisli (Kleisli pure *** Kleisli pure) (1,2) :: Maybe (Int, Int)
-- Just (1,2)
--
-- >>> runKleisli (Kleisli pure *** Kleisli (const Nothing)) (1,2) :: Maybe (Int, Int)
-- Nothing
--
-- >>> runKleisli (Kleisli (replicate 2) *** Kleisli (replicate 3)) ("a","b") :: [(String, String)]
-- [("a","b"),("a","b"),("a","b"),("a","b"),("a","b"),("a","b")]
--
-- ==== __Comonad and type product instance examples of usage__
--
-- >>> import Prelude (Int, pure, Maybe(..), const, replicate, String, (+), (++), Functor, Show, show, (==), (-))
-- >>> import Control.Comonad (Cokleisli(Cokleisli), runCokleisli, extract, duplicate, (=>=))
-- >>> import Control.Comonad.Store (store, seek, runStore, Store, StoreT)
-- >>> import Control.Category ((>>>))
--
-- >>> runCokleisli (Cokleisli extract *** Cokleisli extract) (store (\x -> (x + 1, x + 2)) 3) :: (Int, Int)
-- (4,5)
--
-- >>> :{
-- up :: Int -> Cokleisli (Store Int) Int Int
-- up n = Cokleisli $ \st -> let (ws, s) = runStore st in ws (s + n)
-- :}
--
-- >>> runCokleisli ((up 3 *** up 5) >>> (up 2 *** up 4)) (store (\x -> (x + 1, x + 2)) 0) :: (Int, Int)
-- (6,11)
--
-- >>> runCokleisli ((up 3 >>> up 2) *** (up 5 >>> up 4)) (store (\x -> (x + 1, x + 2)) 0) :: (Int, Int)
-- (6,11)
--
-- >>> :{
-- data Stream a = Cons a (Stream a)
-- tail :: Stream a -> Stream a
-- tail (Cons _ xs) = xs
-- instance Show a => Show (Stream a) where
--   show (Cons x0 (Cons x1 (Cons x2 (Cons x3 (Cons x4 _))))) = show [x0, x1, x2, x3, x4] ++ "..."
-- instance Functor Stream where
--   fmap f (Cons x xs) = Cons (f x) (fmap f xs)
-- instance Comonad Stream where
--   extract (Cons x _ ) = x
--   duplicate xs = Cons xs (duplicate (tail xs))
-- :}
--
-- >>> :{
-- dup :: a -> (a, a)
-- dup x = (x, x)
-- naturals :: Int -> Stream Int
-- naturals n = Cons n (naturals (n + 1))
-- take :: Int -> Stream a -> a
-- take n (Cons x xs) = if n == 0
--   then x
--   else take (n - 1) xs
-- :}
--
-- >>> naturals 0
-- [0,1,2,3,4]...
--
-- >>> take 5 (naturals 0)
-- 5
--
-- >>> ((take 3) =>= (take 4)) (naturals 0)
-- 7
--
-- >>> runCokleisli (Cokleisli (take 3) *** Cokleisli (take 4)) (fmap dup (naturals 0)) :: (Int, Int)
-- (3,4)
--
-- >>> streamN n = Cokleisli (take n)
--
-- >>> runCokleisli ((streamN 3 *** streamN 5) >>> (streamN 2 *** streamN 4)) (fmap dup (naturals 0)) :: (Int, Int)
-- (5,9)
--
-- >>> runCokleisli ((streamN 3 >>> streamN 2) *** (streamN 5 >>> streamN 4)) (fmap dup (naturals 0)) :: (Int, Int)
-- (5,9)
--
-- ==== __Monad and type sum examples of usage__
--
-- >>> import Prelude (Int, pure, Maybe(Just, Nothing), const, replicate, String)
-- >>> import Control.Arrow (Kleisli(Kleisli), runKleisli)
--
-- >>> runKleisli (Kleisli pure *** Kleisli pure) (Left "a") :: [Either String Int]
-- [Left "a"]
--
-- >>> runKleisli (Kleisli pure *** Kleisli pure) (Right 1) :: Maybe (Either String Int)
-- Just (Right 1)
class
  Category cat =>
  CatBiFunctor (p :: Type -> Type -> Type) (cat :: Type -> Type -> Type)
  where
  -- | Categorical generalization of
  --
  -- @bimap :: (a1 -> b1) -> (a2 -> b2) -> (p a1 a2 -> p c1 c2)@
  --
  -- borrowed from arrows.
  (***) :: cat a1 b1 -> cat a2 b2 -> cat (p a1 a2) (p b1 b2)

  -- | Categorical generalization of
  --
  -- @first :: (a -> b) -> (p a c -> p c b)@
  --
  -- borrowed from arrows.
  first :: cat a b -> cat (p a c) (p b c)
  first f = f *** id

  -- | Categorical generalization of
  --
  -- @second :: (a -> b) -> (p a c -> p c b)@
  --
  -- borrowed from arrows.
  second :: cat a b -> cat (p c a) (p c b)
  second f = id *** f

instance CatBiFunctor (,) (->) where
  first f = bimap f id
  second = bimap id
  (***) = bimap

instance forall m. Monad m => CatBiFunctor (,) (Kleisli m) where
  (***) :: Kleisli m a1 b1 -> Kleisli m a2 b2 -> Kleisli m (a1, a2) (b1, b2)
  (Kleisli (mf1 :: a1 -> m b1)) *** (Kleisli (mf2 :: a2 -> m b2)) = Kleisli mf12
    where
      mf12 :: (a1, a2) -> m (b1, b2)
      mf12 (x1, x2) = liftA2 (,) (mf1 x1) (mf2 x2)

instance forall m. Comonad m => CatBiFunctor (,) (Cokleisli m) where
  (***) :: Cokleisli m a1 b1 -> Cokleisli m a2 b2 -> Cokleisli m (a1, a2) (b1, b2)
  (Cokleisli (mf1 :: m a1 -> b1)) *** (Cokleisli (mf2 :: m a2 -> b2)) = Cokleisli mf12
    where
      mf12 :: m (a1, a2) -> (b1, b2)
      mf12 x12 = (mf1 $ liftW fst x12, mf2 $ liftW snd x12)

instance forall m. Monad m => CatBiFunctor Either (Kleisli m) where
  (***) :: Kleisli m a1 b1 -> Kleisli m a2 b2 -> Kleisli m (Either a1 a2) (Either b1 b2)
  (Kleisli (mf1 :: a1 -> m b1)) *** (Kleisli (mf2 :: a2 -> m b2)) = Kleisli mf12
    where
      mf12 :: Either a1 a2 -> m (Either b1 b2)
      mf12 x12 = case x12 of
        Left x1 -> fmap Left (mf1 x1)
        Right x2 -> fmap Right (mf2 x2)
