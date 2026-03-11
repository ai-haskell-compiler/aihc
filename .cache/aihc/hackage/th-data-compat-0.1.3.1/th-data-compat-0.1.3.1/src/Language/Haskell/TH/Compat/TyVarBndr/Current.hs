module Language.Haskell.TH.Compat.TyVarBndr.Current (
  plainTVspecified,
  unTyVarBndr,
  ) where

import Language.Haskell.TH (Name, TyVarBndr (..), Specificity (SpecifiedSpec))

plainTVspecified :: Name -> TyVarBndr Specificity
plainTVspecified n = PlainTV n SpecifiedSpec

unTyVarBndr :: TyVarBndr a -> (Name, Maybe a)
unTyVarBndr (PlainTV n f) = (n, Just f)
unTyVarBndr (KindedTV n f _k) = (n, Just f)
