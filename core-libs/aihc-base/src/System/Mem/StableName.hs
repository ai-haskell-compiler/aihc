module System.Mem.StableName
  ( StableName (..),
    eqStableName,
    hashStableName,
    makeStableName,
  )
where

import GHC.StableName (StableName (..), eqStableName, hashStableName, makeStableName)
