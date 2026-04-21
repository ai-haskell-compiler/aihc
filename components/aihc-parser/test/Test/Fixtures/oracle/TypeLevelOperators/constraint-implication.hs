{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators, DataKinds #-}
class (ks :: [(Type -> Type -> Type) -> Constraint]) |- (k :: (Type -> Type -> Type) -> Constraint) where
  implies :: Satisfies p ks => (k p => p a b) -> p a b
