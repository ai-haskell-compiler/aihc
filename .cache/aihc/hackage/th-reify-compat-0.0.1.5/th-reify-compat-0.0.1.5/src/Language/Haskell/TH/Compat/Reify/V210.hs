module Language.Haskell.TH.Compat.Reify.V210 (
  ParentName,
  unClassOpI, unDataConI, unVarI,
  reifyFixity,
  )where

import Language.Haskell.TH (Info (..), Name, Type, Dec, Fixity)

-- | ParentName type alias is not defined before template-haskell-2.8
type ParentName = Name

-- | Compatible interface to destruct 'ClassOpI'
unClassOpI :: Info -> Maybe (Name, Type, ParentName)
unClassOpI (ClassOpI n t p _) = Just (n, t, p)
unClassOpI  _                 = Nothing

-- | Compatible interface to destruct 'DataConI'
unDataConI :: Info -> Maybe (Name, Type, ParentName)
unDataConI (DataConI n t p _) = Just (n, t, p)
unDataConI  _                 = Nothing

-- | Compatible interface to destruct 'VarI'
unVarI :: Info -> Maybe (Name, Type, Maybe Dec)
unVarI (VarI n t md _) = Just (n, t, md)
unVarI  _              = Nothing

-- | Compatible interface to get operator fixity before temaplate-haskell-2.11
reifyFixity :: Info -> Maybe Fixity
reifyFixity = d
  where
    d (ClassOpI _ _ _ f)  =  Just f
    d (DataConI _ _ _ f)  =  Just f
    d (VarI _ _ _ f)      =  Just f
    d  _                  =  Nothing
