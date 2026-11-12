module GHC.Stack.CCS
  ( currentCallStack,
  )
where

import GHC.Base (Monad (..), String)
import GHC.Types (IO)

-- | Get the cost-centre stack of the current thread.
--
-- The aihc runtime has no cost-centre profiling. Thus, the result is always
-- empty, as in GHC without profiling.
currentCallStack :: IO [String]
currentCallStack = return []
