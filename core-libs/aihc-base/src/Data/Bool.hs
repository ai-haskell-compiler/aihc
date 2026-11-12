module Data.Bool
  ( Bool (False, True),
    (&&),
    not,
    otherwise,
    (||),
    bool,
  )
where

import GHC.Classes (not, (&&), (||))
import GHC.Internal.Base (otherwise)
import GHC.Types (Bool (..))

bool :: a -> a -> Bool -> a
bool false _ False = false
bool _ true True = true
