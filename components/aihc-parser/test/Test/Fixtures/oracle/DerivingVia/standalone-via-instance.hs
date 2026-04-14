{- ORACLE_TEST pass -}
{-# LANGUAGE DerivingVia #-}

module StandaloneViaInstance where

deriving via
  (LiftingSelect (ContT r) m)
  instance
    (MonadSelect r' m) =>
    MonadSelect r' (ContT r m)
