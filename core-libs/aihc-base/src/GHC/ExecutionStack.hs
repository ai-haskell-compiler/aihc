-- | Stack traces from the execution stack.
--
-- aihc does not collect execution stack traces. 'getStackTrace' and
-- 'showStackTrace' always give 'Nothing', as they do in GHC when the
-- runtime system has no stack trace support.
module GHC.ExecutionStack
  ( Location (..),
    SrcLoc (..),
    getStackTrace,
    showStackTrace,
  )
where

import GHC.Base (Maybe (..), Monad (..), String)
import GHC.Int (Int)
import GHC.Types (IO)

-- | A location in the source code.
data SrcLoc = SrcLoc
  { sourceFile :: String,
    sourceLine :: Int,
    sourceColumn :: Int
  }

-- | A location in the execution stack.
data Location = Location
  { objectName :: String,
    functionName :: String,
    srcLoc :: Maybe SrcLoc
  }

-- | The execution stack of the current thread.
getStackTrace :: IO (Maybe [Location])
getStackTrace = return Nothing

-- | The execution stack of the current thread as text.
showStackTrace :: IO (Maybe String)
showStackTrace = return Nothing
