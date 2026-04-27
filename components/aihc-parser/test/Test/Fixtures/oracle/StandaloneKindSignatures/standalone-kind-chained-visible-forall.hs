{- ORACLE_TEST xfail invisible forall body after visible forall in standalone kind signature is wrapped in extra parens during roundtrip -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}

module StandaloneKindChainedVisibleForall where

import Data.Kind (Type)

type Family :: forall (name :: Name) -> forall (ks :: Params name). ParamsProxy name ks -> forall (args :: Args name ks) -> Exp (Res name ks args)
