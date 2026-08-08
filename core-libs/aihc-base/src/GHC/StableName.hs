{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.StableName
  ( StableName (..),
    eqStableName,
    hashStableName,
    makeStableName,
  )
where

import Data.Bool (Bool (..), not)
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Internal.Classes (Eq (..))
import GHC.Prim (StableName#, makeStableName#, stableNameToInt#)
import GHC.Prim.PtrEq (eqStableName#)

-- | A name whose identity remains unchanged when the named object moves.
data StableName a = StableName (StableName# a)

eqStableName :: StableName a -> StableName b -> Bool
eqStableName (StableName left) (StableName right) =
  case eqStableName# left right of
    0# -> False
    _ -> True

hashStableName :: StableName a -> Int
hashStableName (StableName name) = I# (stableNameToInt# name)

makeStableName :: a -> IO (StableName a)
makeStableName value =
  IO
    ( \state ->
        case makeStableName# value state of
          (# nextState, name #) -> (# nextState, StableName name #)
    )

instance Eq (StableName a) where
  (==) = eqStableName
  left /= right = not (left == right)
