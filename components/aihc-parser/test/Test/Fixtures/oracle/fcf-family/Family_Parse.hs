{- ORACLE_TEST
pass
--
Minimal snippet inspired by fcf-family Family.hs parse errors
--}
{-# LANGUAGE GHC2021 #-}

type Name = Symbol

-- params proxy kind placeholder
data ParamsProxy (n :: Name) (ks :: Type)

-- The failing shape: a type family with multiple forall arrows in kind
type Family :: forall (name :: Name) -> forall (ks :: Params name). ParamsProxy name ks -> forall (args :: Args name ks) -> Exp (Res name ks args)

-- Provide placeholders for referenced names so GHC accepts the file
data Args name ks

data Exp r

data Res name ks args
