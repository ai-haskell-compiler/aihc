{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE UnboxedTuples  #-}
module PureSAT.Assert (
    assertST,
    -- * Utilities
    throwST,
    raiseST#,
) where

import Control.Exception (AssertionFailed (..), Exception (..))
import GHC.Exts          (State#, raiseIO#, unsafeCoerce#)
import GHC.ST            (ST (..))
import GHC.Stack         (HasCallStack, callStack, prettyCallStack)

assertST :: HasCallStack => String -> Bool -> ST s ()
assertST _   True  = return ()
assertST msg False = throwST (AssertionFailed ("Assertion failed\n" ++ show msg ++ "\n" ++ prettyCallStack callStack))

raiseST# :: a -> State# s -> (# State# s, b #)
raiseST# = unsafeCoerce# raiseIO#

throwST :: Exception e => e -> ST s a
throwST e = ST (raiseST# (toException e))
