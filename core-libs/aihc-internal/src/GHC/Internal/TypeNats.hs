-- | GHC declares the type-level naturals here and re-exports them from
-- @GHC.TypeNats@. @aihc-internal@ depends on @aihc-base@ rather than the
-- other way round, so the declarations live in @GHC.TypeNats@ and this
-- module re-exports them.
module GHC.Internal.TypeNats
  ( Natural,
    Nat,
    KnownNat,
    natVal,
    natVal',
    SNat,
    fromSNat,
  )
where

import GHC.TypeNats (KnownNat, Nat, Natural, SNat, fromSNat, natVal, natVal')
