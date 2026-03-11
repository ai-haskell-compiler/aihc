-- | Internal monad for the resolution algorithm
--
--  - we keep some state for the list of created values
--  - we collect the applied functions as "Operations"
--  - we might exit with a Left value if we can't build a value
module Data.Registry.Internal.Stack where

import Data.Registry.Internal.Statistics
import Data.Registry.Internal.Types
import Protolude

-- | Monadic stack for the resolution algorithm
type Stack a = StateT Statistics (Either Text) a

-- | Return a value from the Stack if possible
runStack :: Stack a -> Either Text a
runStack = runStackWithValues mempty

-- | Return a value from the Stack if possible
runStackWithValues :: Values -> Stack a -> Either Text a
runStackWithValues vs sa = evalStateT sa (initStatistics vs)

-- | Run the stack to get a list of created values
execStack :: Stack a -> Either Text Values
execStack = execStackWithValues mempty

-- | Return the state of the stack after executing the action
--   This returns the list of built values
execStackWithValues :: Values -> Stack a -> Either Text Values
execStackWithValues vs sa = values <$> execStateT sa (initStatistics vs)

-- | Return the list of applied functions after resolution
evalStack :: Stack a -> Either Text Statistics
evalStack = evalStackWithValues mempty

-- | Run the stack to get a the statistics, starting with some initially created values
evalStackWithValues :: Values -> Stack a -> Either Text Statistics
evalStackWithValues vs sa = execStateT sa (initStatistics vs)

-- | Get the current list of values
getValues :: Stack Values
getValues = values <$> get

-- | Get the current list of operations
getOperations :: Stack Operations
getOperations = operations <$> get

-- | Modify the current list of values
modifyValues :: (Values -> Values) -> Stack ()
modifyValues f = modifyStatistics (\s -> s {values = f (values s)})

-- | Modify the current operations
modifyOperations :: (Operations -> Operations) -> Stack ()
modifyOperations f = modifyStatistics (\s -> s {operations = f (operations s)})

-- | Modify the current statistics
modifyStatistics :: (Statistics -> Statistics) -> Stack ()
modifyStatistics = modify

-- | Store a function application in the list of operations
functionApplied :: Value -> [Value] -> Stack ()
functionApplied output inputs = modifyOperations (AppliedFunction output inputs :)
