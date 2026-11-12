module GHC.ExecutionStack
  ( Location (..),
    SrcLoc (..),
    getStackTrace,
    showStackTrace,
  )
where

import GHC.ExecutionStack.Internal (Location (..), SrcLoc (..))
import GHC.Internal.Base (Maybe (..), Monad (..), String)
import GHC.Types (IO)

-- | Get the execution stack of the current thread.
--
-- The aihc runtime has no stack trace support. Thus, the result is always
-- 'Nothing', as in GHC without stack trace support.
getStackTrace :: IO (Maybe [Location])
getStackTrace = return Nothing

-- | Get the execution stack of the current thread as text.
--
-- The result is always 'Nothing'. Refer to 'getStackTrace'.
showStackTrace :: IO (Maybe String)
showStackTrace = return Nothing
