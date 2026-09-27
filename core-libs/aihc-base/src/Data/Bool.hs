module Data.Bool
  ( Bool (False, True),
    (&&),
    not,
    otherwise,
    (||),
    bool,
  )
where

import GHC.Base (otherwise)
import GHC.Classes (not, (&&), (||))
import GHC.Types (Bool (..))

bool :: a -> a -> Bool -> a
bool false _ False = false
bool _ true True = true
