module Data.Char.Block where

import Control.Applicative (Applicative, pure, (<*>), liftA2, )
import Data.Traversable (Traversable, traverse, foldMapDefault, )
import Data.Foldable (Foldable, foldMap, )


data Row a = Row {left, right :: a} deriving (Eq, Show)
data Block a = Block {upper, lower :: Row a} deriving (Eq, Show)


instance Functor Row where
   fmap f (Row a b) = Row (f a) (f b)

instance Functor Block where
   fmap f (Block a b) = Block (fmap f a) (fmap f b)


instance Foldable Row where
   foldMap = foldMapDefault

instance Foldable Block where
   foldMap = foldMapDefault


instance Traversable Row where
   traverse f (Row a b) = liftA2 Row (f a) (f b)

instance Traversable Block where
   traverse f (Block a b) = liftA2 Block (traverse f a) (traverse f b)


instance Applicative Row where
   pure a = Row a a
   Row fa fb <*> Row a b = Row (fa a) (fb b)

instance Applicative Block where
   pure a = Block (pure a) (pure a)
   Block fa fb <*> Block a b =
      Block (fa <*> a) (fb <*> b)


filled :: Block Bool -> Char
filled set =
   case set of
      Block (Row False False) (Row False False) -> ' '
      Block (Row False False) (Row False True) -> '\x2597'
      Block (Row False False) (Row True False) -> '\x2596'
      Block (Row False False) (Row True True) -> '\x2584'
      Block (Row False True) (Row False False) -> '\x259D'
      Block (Row False True) (Row False True) -> '\x2590'
      Block (Row False True) (Row True False) -> '\x259E'
      Block (Row False True) (Row True True) -> '\x259F'
      Block (Row True False) (Row False False) -> '\x2598'
      Block (Row True False) (Row False True) -> '\x259A'
      Block (Row True False) (Row True False) -> '\x258C'
      Block (Row True False) (Row True True) -> '\x2599'
      Block (Row True True) (Row False False) -> '\x2580'
      Block (Row True True) (Row False True) -> '\x259C'
      Block (Row True True) (Row True False) -> '\x259B'
      Block (Row True True) (Row True True) -> '\x2588'
