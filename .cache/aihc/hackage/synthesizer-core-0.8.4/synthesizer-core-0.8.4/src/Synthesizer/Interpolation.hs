{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Interpolation (
   T(Cons, margin, func),
   Margin(marginOffset, marginNumber),
   cons,
   number,
   offset,
   PrefixReader,
   getNode,
   fromPrefixReader,
   constant,
   ) where

import qualified Synthesizer.State.Signal  as Sig

import Control.Monad.Trans.State (StateT(StateT), evalStateT, )
import Control.Monad.Trans.Writer (Writer, writer, runWriter, )
import Control.Applicative (Applicative(pure, (<*>)), (<$>), liftA2, )
import Data.Monoid (Sum(Sum), )

import qualified Test.QuickCheck as QC

import NumericPrelude.Numeric
import NumericPrelude.Base




{- | interpolation as needed for resampling -}
data T t y =
  Cons {
    margin :: !Margin,
    func   :: !(t -> Sig.T y -> y)
  }

data Margin =
    Margin {
       marginNumber :: !Int,
          -- ^ interpolation requires a total number of 'number'
       marginOffset :: !Int
          -- ^ interpolation requires 'offset' values before the current
    }
   deriving (Show, Eq)

instance QC.Arbitrary Margin where
   arbitrary = liftA2 Margin (abs <$> QC.arbitrary) (abs <$> QC.arbitrary)


cons :: Int -> Int -> (t -> Sig.T y -> y) -> T t y
cons num off =
   Cons (Margin num off)

number :: T t y -> Int
number = marginNumber . margin

offset :: T t y -> Int
offset = marginOffset . margin



{-* Different kinds of interpolation -}

{-** Hard-wired interpolations -}

{-
Applicative composition of two applicative functors,
namely @Writer@ and @StateT Maybe@.
We could also use (.:) from TypeCompose.
-}
newtype PrefixReader y a =
   PrefixReader (Writer (Sum Int) (StateT (Sig.T y) Maybe a))

instance Functor (PrefixReader y) where
   {-# INLINE fmap #-}
   fmap f (PrefixReader m) =
      PrefixReader (fmap (fmap f) m)

instance Applicative (PrefixReader y) where
   {-# INLINE pure #-}
   {-# INLINE (<*>) #-}
   pure = PrefixReader . pure . pure
   (PrefixReader f) <*> (PrefixReader x) =
       PrefixReader (liftA2 (<*>) f x)


{-# INLINE getNode #-}
getNode :: PrefixReader y y
getNode =
   PrefixReader $ writer (StateT Sig.viewL, Sum 1)

{-# INLINE fromPrefixReader #-}
fromPrefixReader :: String -> Int -> PrefixReader y (t -> y) -> T t y
fromPrefixReader name off (PrefixReader pr) =
   let (parser, Sum count) = runWriter pr
   in  cons count off
          (\t xs ->
              maybe
                 (error (name ++ " interpolation: not enough nodes"))
                 ($t)
                 (evalStateT parser xs))

{-|
Consider the signal to be piecewise constant,
where the leading value is used for filling the interval [0,1).
-}
{-# INLINE constant #-}
constant :: T t y
constant =
   fromPrefixReader "constant" 0 (const <$> getNode)
