-- | GHC declares the custom type errors here and re-exports them from
-- @GHC.TypeError@. @aihc-internal@ depends on @aihc-base@ rather than the
-- other way round, so the declarations live in @GHC.TypeError@ and this
-- module re-exports them.
module GHC.Internal.TypeError
  ( ErrorMessage (..),
    TypeError,
    Assert,
    Unsatisfiable,
    unsatisfiable,
  )
where

import GHC.TypeError (Assert, ErrorMessage (..), TypeError, Unsatisfiable, unsatisfiable)
