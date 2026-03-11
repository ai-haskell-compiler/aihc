module Language.Haskell.TH.Compat.Reify.Current (
  ParentName,
  unClassOpI, unDataConI, unVarI,
  reifyFixity,
  )where

import Language.Haskell.TH (Info (..), Name, Type, ParentName, Dec, reifyFixity)


-- | Compatible interface to destruct 'ClassOpI'
unClassOpI :: Info -> Maybe (Name, Type, ParentName)
unClassOpI (ClassOpI n t p) = Just (n, t, p)
unClassOpI  _               = Nothing

-- | Compatible interface to destruct 'DataConI'
unDataConI :: Info -> Maybe (Name, Type, ParentName)
unDataConI (DataConI n t p) = Just (n, t, p)
unDataConI  _               = Nothing

-- | Compatible interface to destruct 'VarI'
unVarI :: Info -> Maybe (Name, Type, Maybe Dec)
unVarI (VarI n t md) = Just (n, t, md)
unVarI  _            = Nothing
