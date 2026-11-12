module GHC.ExecutionStack.Internal
  ( Location (..),
    SrcLoc (..),
  )
where

import GHC.Int (Int)
import GHC.Internal.Base (Maybe, String)

-- | A location in the source code.
data SrcLoc = SrcLoc
  { sourceFile :: String,
    sourceLine :: Int,
    sourceColumn :: Int
  }

-- | A location in the program code.
data Location = Location
  { objectName :: String,
    functionName :: String,
    srcLoc :: Maybe SrcLoc
  }
