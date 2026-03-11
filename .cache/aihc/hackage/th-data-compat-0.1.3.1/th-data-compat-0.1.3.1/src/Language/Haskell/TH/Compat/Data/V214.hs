-- The latest version of applying this module impl is template-haskell-2.14
module Language.Haskell.TH.Compat.Data.V214 (
  dataD', unDataD,
  newtypeD', unNewtypeD,
  dataInstD', unDataInstD,
  newtypeInstD', unNewtypeInstD,
  unInstanceD,
  ) where

import Language.Haskell.TH
  (CxtQ, ConQ, TypeQ, DecQ,
   Cxt, Con, Type, Name, TyVarBndr, Kind,
   Dec (DataD, NewtypeD, DataInstD, NewtypeInstD, InstanceD),
   DerivClauseQ, DerivClause (..), Pred,
   dataD, newtypeD, dataInstD, newtypeInstD, derivClause, conT)

import Language.Haskell.TH.Compat.Data.Util (foldAppT_)


derivesFromNames :: [Name] -> [DerivClauseQ]
derivesFromNames ns = [derivClause Nothing $ map conT ns]

unDerivClause :: DerivClause -> [Pred]
unDerivClause (DerivClause _ ps) = ps

-- | Definition against 'dataD',
--   compatible with before temaplate-haskell-2.11
dataD' :: CxtQ -> Name -> [TyVarBndr] -> [ConQ] -> [Name]
       -> DecQ
dataD' cxt n bs cs ds = dataD cxt n bs Nothing cs $ derivesFromNames ds

-- | Compatible interface to destruct 'DataD'
unDataD :: Dec -> Maybe (Cxt, Name, [TyVarBndr], Maybe Kind, [Con], [Type])
unDataD (DataD cxt n bs mk cs ds) = Just (cxt, n, bs, mk, cs, ds >>= unDerivClause)
unDataD  _                        = Nothing

-- | Definition against 'newtypeD',
--   compatible with before temaplate-haskell-2.11
newtypeD' :: CxtQ -> Name -> [TyVarBndr] -> ConQ -> [Name]
          -> DecQ
newtypeD' cxt n bs c ds = newtypeD cxt n bs Nothing c $ derivesFromNames ds

-- | Compatible interface to destruct 'NewtypeD'
unNewtypeD :: Dec -> Maybe (Cxt, Name, [TyVarBndr], Maybe Kind, Con, [Type])
unNewtypeD (NewtypeD cxt n bs mk c ds) = Just (cxt, n, bs, mk, c, ds >>= unDerivClause)
unNewtypeD  _                          = Nothing

-- | Definition against 'dataInstD',
--   compatible with before temaplate-haskell-2.11
dataInstD' :: CxtQ -> Name -> [TypeQ] -> [ConQ] -> [Name]
           -> DecQ
dataInstD' cxt n as cs ds = dataInstD cxt n as Nothing cs $ derivesFromNames ds

-- | Compatible interface to destruct 'DataInstD'
unDataInstD :: Dec -> Maybe (Cxt, Maybe [TyVarBndr], Type, Maybe Kind, [Con], [Type])
unDataInstD (DataInstD cxt n as mk cs ds) = Just (cxt, Nothing, foldAppT_ n as, mk, cs, ds >>= unDerivClause)
unDataInstD  _                            = Nothing

-- | Definition against 'newtypeInstD',
--   compatible with before temaplate-haskell-2.11
newtypeInstD' :: CxtQ -> Name -> [TypeQ] -> ConQ -> [Name]
              -> DecQ
newtypeInstD' cxt n as c ds = newtypeInstD cxt n as Nothing c $ derivesFromNames ds

-- | Compatible interface to destruct 'NewtypeInstD'
unNewtypeInstD :: Dec -> Maybe (Cxt, Maybe [TyVarBndr], Type, Maybe Kind, Con, [Type])
unNewtypeInstD (NewtypeInstD cxt n as mk c ds) = Just (cxt, Nothing, foldAppT_ n as, mk, c, ds >>= unDerivClause)
unNewtypeInstD  _                              = Nothing

-- | Compatible interface to destruct 'InstanceD'
--   No Overlap type is defined before template-haskell-2.11.
unInstanceD :: Dec -> Maybe (Cxt, Type, [Dec])
unInstanceD (InstanceD _ cxt ty decs) = Just (cxt, ty, decs)
unInstanceD  _                        = Nothing
