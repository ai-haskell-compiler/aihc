{- ORACLE_TEST xfail reason="deriving clause with multiple classes and @since haddock not parsed" -}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
module NewtypeWithViaMultipleClasses where

newtype LiftingSelect (t :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) (a :: Type)
  = LiftingSelect (t m a)
  deriving
    ( -- | @since 2.3
      Functor,
      -- | @since 2.3
      Applicative,
      -- | @since 2.3
      Monad
    )
    via (t m)