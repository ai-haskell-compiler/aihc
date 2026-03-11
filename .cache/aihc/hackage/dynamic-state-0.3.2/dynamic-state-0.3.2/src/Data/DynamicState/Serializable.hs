{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.DynamicState.Serializable
-- License     :  GPL2
-- Maintainer  :  zcarterc@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is HashMap ConcreteTypeRep Dynamic with a twist. The Dynamic
-- used can also be ByteString, to make repeated
-- reserialization cheap.
-- A user-provided State-like is used to store this.

module Data.DynamicState.Serializable (
  DynamicState(..),
  getDyn,
  putDyn
  ) where

import Data.Binary
import Data.HashMap.Strict as M
import Data.ConcreteTypeRep
import Data.Typeable
import Data.ByteString.Lazy(ByteString)
import Control.Monad

-- | A Dynamic value, potentially stored serialized
data Dynamic
  = forall a. (Typeable a, Binary a) => Dynamic !a
  | Serial !ByteString

-- | Try to extract a value from the 'Dynamic', returning True if it was decoded from a 'Serial'
fromDynamic :: forall a. (Typeable a, Binary a) => Dynamic -> Maybe (a,Bool)
fromDynamic (Dynamic b) = (,False) <$> cast b
#if __GLASGOW_HASKELL__ < 708
fromDynamic (Serial bs) = (,True) <$> (Just $ decode bs)
#else
fromDynamic (Serial bs) = let b = either (const Nothing) (\(_,_,a) -> Just a) $ decodeOrFail bs in (,True) <$> b
#endif

instance Binary Dynamic where
  put = put . toSerialRep where
    toSerialRep (Dynamic a) = encode a
    toSerialRep (Serial bs) = bs
  get = Serial <$> get

-- | An extensible record, indexed by type, using state to cache deserializtion
newtype DynamicState = DynamicState { unDynamicState :: M.HashMap ConcreteTypeRep Dynamic }
  deriving (Typeable)

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup DynamicState where
  DynamicState a <> DynamicState b = DynamicState (a <> b)
#endif

instance Monoid DynamicState where
#if !MIN_VERSION_base(4,11,0)
  mappend (DynamicState a) (DynamicState b) = DynamicState (mappend a b)
#endif
  mempty = DynamicState mempty

-- | Get a value, inside a State-like monad specified by the first two functions
getDyn :: forall m a. (Typeable a, Binary a, Monad m) => m DynamicState -> (DynamicState -> m ()) -> m (Maybe a)
getDyn get' put' = do
    let ty = cTypeOf (undefined::a)
    dvs <- liftM unDynamicState get'
    case M.lookup ty dvs >>= fromDynamic of
      Just (val,new) -> (when new $ put' $ DynamicState $ M.insert ty (Dynamic val) dvs) >> return (Just val)
      Nothing -> return Nothing
-- | Set a value, inside a State-like monad specified by the first two functions
putDyn :: forall m a. (Typeable a, Binary a, Monad m) => m DynamicState -> (DynamicState -> m ()) -> a -> m ()
putDyn get' put' v = do
    dvs <- liftM unDynamicState get'
    put' $ DynamicState (M.insert (cTypeOf (undefined :: a)) (Dynamic v) dvs)

instance Binary DynamicState where
  put (DynamicState ds) = put (M.toList ds)
  get = DynamicState . M.fromList <$> get

-- TODO: since a 'DynamicState' is now serialisable, it could potentially
-- exist for a long time (days/months?). No operations are provided to remove
-- entries. If these start accumulating a lot of junk,
-- it may be necessary to prune them (perhaps keep track of access date and
-- remove the ones more than a month old?).
