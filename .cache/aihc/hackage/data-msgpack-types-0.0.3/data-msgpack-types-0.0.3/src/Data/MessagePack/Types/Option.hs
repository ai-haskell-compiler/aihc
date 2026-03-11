{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
module Data.MessagePack.Types.Option
  ( Option (..)
  ) where

import           Control.Applicative           (Alternative (..),
                                                Applicative (..), (<$>))
import           Control.Monad                 (Monad (..), MonadPlus (..))
import           Data.Data                     (Data)
import           Data.Foldable                 (Foldable)
import           Data.Traversable              (Traversable)
import           Data.Typeable                 (Typeable)
import           Test.QuickCheck.Arbitrary     (Arbitrary (..))
import qualified Test.QuickCheck.Gen           as Gen

import           Data.MessagePack.Types.Class  (MessagePack (..))
import           Data.MessagePack.Types.Object (Object (..))


data Option a
  = None
  | Some a
  deriving (Eq, Ord, Show, Read, Foldable, Functor, Traversable, Data, Typeable)

instance Applicative Option where
  pure = Some

  Some f <*> m = fmap f m
  None   <*> _ = None

instance Monad Option where
  return = Some

  None   >>= _ = None
  Some x >>= f = f x
#if (MIN_VERSION_base(4,13,0))
instance MonadFail Option where
#endif
  fail _ = None

instance Alternative Option where
  empty = None

  None <|> x = x
  x    <|> _ = x

instance MonadPlus Option where
  mzero = empty
  mplus = (<|>)

instance MessagePack a => MessagePack (Option a) where
  toObject None     = ObjectNil
  toObject (Some a) = toObject a

  fromObject ObjectNil = return None
  fromObject x         = Some <$> fromObject x

instance Arbitrary a => Arbitrary (Option a) where
  arbitrary = Gen.oneof
    [ pure None
    , Some <$> arbitrary
    ]
