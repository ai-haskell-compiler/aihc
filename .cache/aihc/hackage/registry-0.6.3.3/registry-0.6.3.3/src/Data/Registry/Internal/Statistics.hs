{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module provides a set of statistics over the execution
--  of the registry. This allows to get better insights over the execution
--  or test that the registry is well configured
module Data.Registry.Internal.Statistics where

import Data.Registry.Internal.Types
import Protolude
import Type.Reflection

-- * DATA TYPES

-- | This datatype records:
--    - the created values
--    - the applied functions
--    - the specializations used to create values
data Statistics = Statistics
  { operations :: Operations,
    values :: Values
  }
  deriving (Show)

instance Semigroup Statistics where
  Statistics ops1 vs1 <> Statistics ops2 vs2 =
    Statistics (ops1 <> ops2) (vs1 <> vs2)

instance Monoid Statistics where
  mempty = Statistics mempty mempty
  mappend = (<>)

-- | A list of function applications created
--   when creating a value out of the Registry
type Operations = [AppliedFunction]

-- | List of distinct paths from the root of the value graph to a leaf
type Paths = [[Value]]

-- | A function application with an output value and a list of input values
data AppliedFunction = AppliedFunction
  { _outputValue :: Value,
    _inputValues :: [Value]
  }
  deriving (Show)

-- | Create Statistics from a list of values
initStatistics :: Values -> Statistics
initStatistics vs = mempty {values = vs}

-- | Return the specializations used during the creation of values
valuesSpecializations :: Statistics -> [Specialization]
valuesSpecializations stats =
  case listValues (values stats) of
    [] -> []
    v : vs ->
      case valueSpecialization v of
        Just s -> s : valuesSpecializations stats {values = fromValues vs}
        Nothing -> valuesSpecializations stats {values = fromValues vs}

-- | Return the list of distinct paths from the root of a value graph to leaves
--   of that graph.
--   This can be used to check if a given value was indeed used according to a given
--   specialization
allValuesPaths :: Statistics -> Paths
allValuesPaths stats = do
  v <- listValues $ values stats
  valuePaths v

-- | Return all the paths from a given value to all its dependencies
valuePaths :: Value -> Paths
valuePaths v@(CreatedValue _ _ _ (Dependencies ds)) = do
  d <- ds
  (v :) <$> valuePaths d
valuePaths _ = []

-- | Find the most recently created value of a given type
findMostRecentValue :: forall a. (Typeable a) => Statistics -> Maybe Value
findMostRecentValue stats = findValue (someTypeRep (Proxy :: Proxy a)) (values stats)

-- | Find the created values of a given type
findCreatedValues :: forall a. (Typeable a) => Statistics -> [Value]
findCreatedValues stats = findValues(someTypeRep (Proxy :: Proxy a)) (values stats)
