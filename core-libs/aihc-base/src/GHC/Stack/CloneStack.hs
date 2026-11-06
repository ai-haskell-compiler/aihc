module GHC.Stack.CloneStack (StackSnapshot) where

-- | A snapshot of a GHC stack. AIHC cannot clone a stack, so no function
-- makes a value of this type. The type exists so that the signatures of
-- aihc-heap are the same as the signatures of ghc-heap.
data StackSnapshot = StackSnapshot
