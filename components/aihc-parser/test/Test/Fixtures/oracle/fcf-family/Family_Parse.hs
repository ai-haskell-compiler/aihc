{- ORACLE_TEST pass -}
-- Minimal snippet inspired by fcf-family Family.hs parse errors
{-# LANGUAGE GHC2021 #-}

type Name = Symbol

-- params proxy kind placeholder
data ParamsProxy (n :: Name) (ks :: Type)

-- Placeholder: keep file GHC-accepted. Original repro needs a forall-in-type shape.
type Family = ()

-- Provide placeholders for referenced names so GHC accepts the file
data Args name ks

data Exp r

data Res name ks args
