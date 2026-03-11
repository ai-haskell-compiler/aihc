module Language.Haskell.TH.Compat.Constraint.Current (
  classP,
  ) where

import Language.Haskell.TH (Q, Name, Type, Pred, conT, appT)

-- | type-class predicate context.
--   definition to keep backward comatiblity.
classP :: Name -> [Q Type] -> Q Pred
classP cla tys = foldl appT (conT cla) tys
