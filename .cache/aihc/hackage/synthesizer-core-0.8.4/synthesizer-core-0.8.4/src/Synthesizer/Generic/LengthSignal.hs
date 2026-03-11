{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
module Synthesizer.Generic.LengthSignal where

import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG

import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )
import Data.Tuple.HT (mapSnd, )

import qualified Algebra.Additive       as Additive

import NumericPrelude.Numeric as NP
import NumericPrelude.Base hiding (length, splitAt, )


data T sig = Cons {length :: Int, body :: sig}
   deriving (Show)

fromSignal :: (CutG.Read sig) => sig -> T sig
fromSignal xs  =  Cons (CutG.length xs) xs

toSignal :: T sig -> sig
toSignal  =  body

{- |
Each fmap must preserve the signal length.
-}
instance Functor T where
   fmap f (Cons xl xs) = Cons xl (f xs)

instance (Additive.C a, SigG.Transform sig a) => Additive.C (T (sig a)) where
   zero = mempty
   negate xs = xs{body = SigG.map negate (body xs)}
   (Cons xl xs) + (Cons yl ys) =
      Cons (max xl yl) (SigG.mix xs ys)

instance (Semigroup sig) => Semigroup (T sig) where
   Cons xl xs <> Cons yl ys = Cons (xl+yl) (xs <> ys)

instance (Monoid sig) => Monoid (T sig) where
   mempty = Cons zero mempty
   mappend (Cons xl xs) (Cons yl ys) =
      Cons (xl+yl) (mappend xs ys)

splitAt :: (CutG.Transform sig) => Int -> T sig -> (T sig, T sig)
splitAt n (Cons xl xs) =
   let (ys,zs) = SigG.splitAt n xs
   in  (Cons (min n xl) ys, Cons (max n xl - n) zs)

{- |
It must hold @delay <= length a@.
-}
{-
It is crucial that 'mix' uses the chunk size structure of the second operand.
This way we avoid unnecessary and even infinite look-ahead.
-}
{-# INLINE addShiftedSimple #-}
addShiftedSimple ::
   (Additive.C a, SigG.Transform sig a) =>
   Int -> T (sig a) -> T (sig a) -> T (sig a)
addShiftedSimple del a b =
   uncurry mappend $
   mapSnd (flip (+) b) $
   splitAt del a
