-- | GHC declares 'IsLabel' here and re-exports it from
-- @GHC.OverloadedLabels@. @aihc-internal@ depends on @aihc-base@ rather
-- than the other way round, so the declaration lives in
-- @GHC.OverloadedLabels@ and this module re-exports it.
module GHC.Internal.OverloadedLabels
  ( IsLabel (..),
  )
where

import GHC.OverloadedLabels (IsLabel (..))
