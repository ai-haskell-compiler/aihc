{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module Data.Commutative where

import Data.Monoid ( Any (..)
                   , All (..)
                   , First (..)
                   , Last (..)
                   , Sum (..)
                   , Product (..)
                   , mappend
                   )
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)
import qualified Data.Vector as Vector


class Commutative a where
  commute :: a -> a -> a -- ^ Abelian magma - @x `commute` y == y `commute` x@.
                         -- Note that the commutative behaviour should be embedded in the
                         -- instance. For distinguished commutes, more information is needed - like
                         -- a predicate as is the case for @commuteVia@ and @commuteViaM@.

(<~>) :: Commutative a => a -> a -> a
(<~>) = commute

class Commutative a => CommutativeId a where
  cempty :: a -- ^ Identity element - @x `commute` cempty == cempty `commute` x == x@

-- | @flip@ when @False@ - simple & pure "predicative" commute.
commuteVia :: Bool -> (a -> a -> a) -> a -> a -> a
commuteVia p f = if p then f else flip f

-- | Lifted predicative behaviour.
commuteViaF :: Functor f => f Bool -> (a -> a -> a) -> a -> a -> f a
commuteViaF mb f x y = (\b -> if b then f x y else f y x) <$> mb


-- Unit
instance Commutative () where
  commute () () = ()

instance CommutativeId () where
  cempty = ()

-- | Endomorphisms commutative over composition.
-- __Warning__: The @Commutative@ instance uses @unsafePerformIO@ to randomly pick the order.
newtype CommEndo a = CommEndo {appCommEndo :: a -> a}

instance Commutative (CommEndo a) where
  commute (CommEndo f) (CommEndo g) = CommEndo $ pick1 (f . g) (g . f) -- it doesn't matter which is chosen

instance CommutativeId (CommEndo a) where
  cempty = CommEndo id

-- Booleans
instance Commutative Any where
  commute (Any x) (Any y) = Any $ x || y

instance CommutativeId Any where
  cempty = Any False

instance Commutative All where
  commute (All x) (All y) = All $ x && y

instance CommutativeId All where
  cempty = All True

-- Maybe

-- | In the case of two @Just@ values, the commutative instance randomly chooses one of them.
-- __Warning__: The @Commutative@ instance uses @unsafePerformIO@ to randomly pick the order.
newtype OneOf a = OneOf {getOneOf :: Maybe a}
  deriving (Show, Eq)

instance Commutative (OneOf a) where
  commute (OneOf x) (OneOf y) = OneOf $ pick1 (getFirst $ First x `mappend` First y)
                                              (getLast  $ Last x  `mappend` Last y)

instance CommutativeId (OneOf a) where
  cempty = OneOf Nothing

-- Numbers
instance Num a => Commutative (Sum a) where
  commute (Sum x) (Sum y) = Sum $ x + y

instance Num a => CommutativeId (Sum a) where
  cempty = Sum 0

instance Num a => Commutative (Product a) where
  commute (Product x) (Product y) = Product $ x * y

instance Num a => CommutativeId (Product a) where
  cempty = Product 1


pick1 :: a -> a -> a
pick1 l r = let leftOrRight = unsafePerformIO randomIO
            in if leftOrRight then l else r
