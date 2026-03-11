{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Type.Membership.HList (HList(..)
  , hindex
  , hfoldrWithIndex
  , htraverse
  , htraverseWithIndex
  , hlength) where

import Type.Membership.Internal

hindex :: HList h xs -> Membership xs x -> h x
hindex HNil _ = error "impossible"
hindex (HCons x xs) m = testMembership m (\Refl -> x) (hindex xs)

htraverse :: forall f g h xs. Applicative f => (forall x. g x -> f (h x)) -> HList g xs -> f (HList h xs)
htraverse f = go where
  go :: HList g ts -> f (HList h ts)
  go HNil = pure HNil
  go (HCons h xs) = HCons <$> f h <*> go xs
{-# INLINE htraverse #-}

hlength :: HList h xs -> Int
hlength = go 0 where
  go :: Int -> HList h xs -> Int
  go n HNil = n
  go n (HCons _ xs) = let !n' = n + 1 in go n' xs
{-# INLINE hlength #-}
