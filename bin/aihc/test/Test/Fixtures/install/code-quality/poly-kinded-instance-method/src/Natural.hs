{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Natural where

type (:->) p q = forall a b. p a b -> q a b

class BifunctorFunctor t where
  bifmap :: (p :-> q) -> t p :-> t q
