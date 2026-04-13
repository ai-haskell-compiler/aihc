{- ORACLE_TEST xfail reason="standalone deriving via with context constraint not parsed correctly" -}
{-# LANGUAGE DerivingVia #-}

module StandaloneViaInstance where

deriving via
  (LiftingSelect (ContT r) m)
  instance
    (MonadSelect r' m) =>
    MonadSelect r' (ContT r m)
