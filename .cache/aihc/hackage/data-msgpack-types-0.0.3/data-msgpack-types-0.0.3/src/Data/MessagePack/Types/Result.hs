{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Safe              #-}
module Data.MessagePack.Types.Result
  ( Result (..)
  ) where

import           Control.Applicative       (Alternative (..), Applicative (..),
                                            (<$>), (<*>))
import           Data.Foldable             (Foldable)
import           Data.Traversable          (Traversable)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import qualified Test.QuickCheck.Gen       as Gen


data Result a
  = Success a
  | Failure String
  deriving (Read, Show, Eq, Functor, Traversable, Foldable)


instance Applicative Result where
  pure = Success

  Success f   <*> x = fmap f x
  Failure msg <*> _ = Failure msg


instance Alternative Result where
  empty = Failure "empty alternative"

  s@Success {} <|> _ = s
  _            <|> r = r


instance Monad Result where
  return = pure

  Success x   >>= f = f x
  Failure msg >>= _ = Failure msg

#if (MIN_VERSION_base(4,13,0))
instance MonadFail Result where
#endif
  fail = Failure


instance Arbitrary a => Arbitrary (Result a) where
  arbitrary = Gen.oneof
    [ Success <$> arbitrary
    , Failure <$> arbitrary
    ]
