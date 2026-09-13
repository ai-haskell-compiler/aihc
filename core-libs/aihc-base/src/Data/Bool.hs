module Data.Bool
  ( Bool (False, True),
    (&&),
    not,
    otherwise,
    (||),
  )
where

import GHC.Base (otherwise)
import GHC.Classes (not, (&&), (||))
import GHC.Types (Bool (..))
