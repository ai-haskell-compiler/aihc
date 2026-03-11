{-# LANGUAGE CPP, TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Haskell.TH.Extras where

import Control.Monad
import Data.Generics
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 914
import Language.Haskell.TH.Syntax
#endif
import Language.Haskell.TH.Datatype.TyVarBndr

intIs64 :: Bool
intIs64 = toInteger (maxBound :: Int) > 2^(32 :: Integer)

replace :: (a -> Maybe a) -> (a -> a)
replace = ap fromMaybe

composeExprs :: [ExpQ] -> ExpQ
composeExprs [] = [| id |]
composeExprs [f] = f
composeExprs (f:fs) = [| $f . $(composeExprs fs) |]

-- | Determines the name of a data constructor. It's an error if the 'Con' binds more than one name (which
-- happens in the case where you use GADT syntax, and give multiple data constructor names separated by commas
-- in a type signature in the where clause).
nameOfCon :: Con -> Name
nameOfCon (NormalC  name _) = name
nameOfCon (RecC     name _) = name
nameOfCon (InfixC _ name _) = name
nameOfCon (ForallC _ _ con) = nameOfCon con
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
nameOfCon (GadtC [name] _ _)    = name
nameOfCon (GadtC _ _ _)    = error $ "nameOfCon: GadtC: only single constructor names are supported"
nameOfCon (RecGadtC [name] _ _) = name
nameOfCon (RecGadtC _ _ _)    = error $ "nameOfCon: RecGadtC: only single constructor names are supported"
#endif

-- |WARNING: discards binders in GADTs and existentially-quantified constructors
argTypesOfCon :: Con -> [Type]
argTypesOfCon (NormalC  _ args) = map snd args
argTypesOfCon (RecC     _ args) = [t | (_,_,t) <- args]
argTypesOfCon (InfixC x _ y)    = map snd [x,y]
argTypesOfCon (ForallC _ _ con) = argTypesOfCon con
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
argTypesOfCon (GadtC _ args _)    = map snd args
argTypesOfCon (RecGadtC _ args _) = [t | (_,_,t) <- args]
#endif

nameOfBinder :: TyVarBndr_ a -> Name
nameOfBinder = tvName

varsBoundInCon :: Con -> [TyVarBndrSpec]
varsBoundInCon (ForallC bndrs _ con) = bndrs ++ varsBoundInCon con
varsBoundInCon _ = []

namesBoundInPat :: Pat -> [Name]
namesBoundInPat (VarP name)             = [name]
namesBoundInPat (TupP pats)             = pats >>= namesBoundInPat
#if MIN_VERSION_template_haskell(2,18,0)
namesBoundInPat (ConP _ _ pats)         = pats >>= namesBoundInPat
#else
namesBoundInPat (ConP _ pats)           = pats >>= namesBoundInPat
#endif
namesBoundInPat (InfixP p1 _ p2)        = namesBoundInPat p1 ++ namesBoundInPat p2
namesBoundInPat (TildeP pat)            = namesBoundInPat pat
namesBoundInPat (AsP name pat)          = name : namesBoundInPat pat
namesBoundInPat (RecP _ fieldPats)      = map snd fieldPats >>= namesBoundInPat
namesBoundInPat (ListP pats)            = pats >>= namesBoundInPat
namesBoundInPat (SigP pat _)            = namesBoundInPat pat

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 612
namesBoundInPat (BangP pat)             = namesBoundInPat pat
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 700
namesBoundInPat (ViewP _ pat)           = namesBoundInPat pat
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
namesBoundInPat (UnboxedTupP pats)      = pats >>= namesBoundInPat
#endif

namesBoundInPat _                       = []


namesBoundInDec :: Dec -> [Name]
namesBoundInDec (FunD name _)                       = [name]
namesBoundInDec (ValD pat _ _)                      = namesBoundInPat pat

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
namesBoundInDec (DataD _ name _ _ _ _)              = [name]
namesBoundInDec (NewtypeD _ name _ _ _ _)           = [name]
#else
namesBoundInDec (DataD _ name _ _ _)                = [name]
namesBoundInDec (NewtypeD _ name _ _ _)             = [name]
#endif

namesBoundInDec (TySynD name _ _)                   = [name]
namesBoundInDec (ClassD _ name _ _ _)               = [name]
namesBoundInDec (ForeignD (ImportF _ _ _ name _))   = [name]

#if defined(__GLASGOW_HASKELL__)
#if __GLASGOW_HASKELL__ >= 800
namesBoundInDec (OpenTypeFamilyD (TypeFamilyHead name _ _ _))     = [name]
namesBoundInDec (ClosedTypeFamilyD (TypeFamilyHead name _ _ _) _) = [name]
#elif __GLASGOW_HASKELL__ >= 612
namesBoundInDec (FamilyD _ name _ _)                = [name]
#endif
#endif

namesBoundInDec _                                   = []

genericalizeName :: Name -> Name
genericalizeName = mkName . nameBase

-- Genericalize all names defined at the top level, to fix the lunacy introduced in GHC 7.2.
-- Why they should be fresh is beyond me; it really seems absurd because there is no way whatsoever
-- to refer to names known to be bound in [d||] quotes other than to scrounge around inside the
-- generated 'Dec's.
genericalizeDecs :: [Dec] -> [Dec]
genericalizeDecs decs = everywhere (mkT fixName) decs
    where
        -- get all names bound in the decs and make them generic
        -- at every occurrence in decs.
        names = decs >>= namesBoundInDec
        genericalizedNames = [ (n, genericalizeName n) | n <- names]
        fixName = replace (`lookup` genericalizedNames)

headOfType :: Type -> Name
headOfType (ForallT _ _ ty) = headOfType ty
headOfType (VarT name) = name
headOfType (ConT name) = name
headOfType (TupleT n) = tupleTypeName n
headOfType ArrowT = ''(->)
headOfType ListT = ''[]
headOfType (AppT t _) = headOfType t
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 612
headOfType (SigT t _) = headOfType t
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
headOfType (UnboxedTupleT n) = unboxedTupleTypeName n
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
headOfType ty = error $ "headOfType: Unhandled type: " ++ show ty
#endif

occursInType :: Name -> Type -> Bool
occursInType var ty = case ty of
        ForallT bndrs _ ty'
            | any (var ==) (map tvName bndrs)
                -> False
            | otherwise
                -> occursInType var ty'
        VarT name
            | name == var -> True
            | otherwise   -> False
        AppT ty1 ty2 -> occursInType var ty1 || occursInType var ty2
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 612
        SigT ty' _ -> occursInType var ty'
#endif
        _ -> False

-- | Assuming that we're building an instance of the form C (T v_1 ... v_(n-1)) for some GADT T, this function
-- takes a list of the variables v_1 ... v_(n-1) used in the instance head, as well as the result type of some data
-- constructor, say T x_1 ... x_(n-1) x_n, as well as the type t of some argument to it, and substitutes any of
-- x_i (1 <= i <= n-1) occurring in t for the corresponding v_i, taking care to avoid name capture by foralls in t.
substVarsWith
  :: [Name] -- Names of variables used in the instance head in argument order
  -> Type -- Result type of constructor
  -> Type -- Type of argument to the constructor
  -> Type -- Type of argument with variables substituted for instance head variables.
substVarsWith topVars resultType argType = subst Set.empty argType
  where
    topVars' = reverse topVars
    AppT resultType' _indexType = resultType
    subst :: Set Name -> Type -> Type
    subst bs ty = case ty of
      -- Several of the following cases could all be covered by an "x -> x" case, but
      -- I'd rather know if new cases need to be handled specially in future versions
      -- of Template Haskell.
      ForallT bndrs cxt1 t ->
        let bs' = Set.union bs (Set.fromList (map tvName bndrs))
        in ForallT bndrs (map (subst bs') cxt1) (subst bs' t)
      AppT f x -> AppT (subst bs f) (subst bs x)
      SigT t k -> SigT (subst bs t) k
      VarT v -> if Set.member v bs
        then VarT v
        else VarT (findVar v topVars' resultType')
      ConT n -> ConT n
      TupleT k -> TupleT k
      ArrowT -> ArrowT
      ListT -> ListT
      UnboxedTupleT k -> UnboxedTupleT k
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
      InfixT t1 x t2 -> InfixT (subst bs t1) x (subst bs t2)
      ParensT t -> ParensT (subst bs t)
      UInfixT t1 x t2 -> UInfixT (subst bs t1) x (subst bs t2)
      WildCardT -> WildCardT
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 802
      UnboxedSumT k -> UnboxedSumT k
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 710
      EqualityT -> EqualityT
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
      ConstraintT -> ConstraintT
      LitT l -> LitT l
      PromotedConsT -> PromotedConsT
      PromotedNilT -> PromotedNilT
      PromotedT n -> PromotedT n
      PromotedTupleT k -> PromotedTupleT k
      StarT -> StarT
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 904
      PromotedInfixT t1 x t2 -> PromotedInfixT (subst bs t1) x (subst bs t2)
      PromotedUInfixT t1 x t2 -> PromotedUInfixT (subst bs t1) x (subst bs t2)
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 900
      MulArrowT -> MulArrowT
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 900
      ForallVisT xs t -> ForallVisT xs (subst bs t)
#endif
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 808
      ImplicitParamT s t -> ImplicitParamT s (subst bs t)
      AppKindT f x -> AppKindT (subst bs f) (subst bs x)
#endif

    findVar v (tv:_) (AppT _ (VarT v')) | v == v' = tv
    findVar v (_:tvs) (AppT t (VarT _)) = findVar v tvs t
    findVar v _ _ = error $ "substVarsWith: couldn't look up variable substitution for " ++ show v
      ++ " with topVars: " ++ show topVars ++ " resultType: " ++ show resultType ++ " argType: " ++ show argType

-- | Determine the arity of a kind.
-- Starting in template-haskell 2.8.0.0, 'Kind' and 'Type' became synonymous.
kindArity :: Kind -> Int
kindArity k = case k of
  ForallT _ _ t -> kindArity t
  AppT (AppT ArrowT _) t -> 1 + kindArity t
  SigT t _ -> kindArity t
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
  ParensT t -> kindArity t
#endif
  _ -> 0

-- | Given the name of a type constructor, determine its full arity
tyConArity :: Name -> Q Int
tyConArity n = do
  (ts, ka) <- tyConArity' n
  return (length ts + ka)

-- | Given the name of a type constructor, determine a list of type variables bound as parameters by
-- its declaration, and the arity of the kind of type being defined (i.e. how many more arguments would
-- need to be supplied in addition to the bound parameters in order to obtain an ordinary type of kind *).
-- If the supplied 'Name' is anything other than a data or newtype, produces an error.
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 908
tyConArity' :: Name -> Q ([TyVarBndrUnit], Int)
#else
tyConArity' :: Name -> Q ([TyVarBndr BndrVis], Int)
#endif
tyConArity' n = do
  r <- reify n
  return $ case r of
    TyConI (DataD _ _ ts mk _ _) -> (ts, fromMaybe 0 (fmap kindArity mk))
    TyConI (NewtypeD _ _ ts mk _ _) -> (ts, fromMaybe 0 (fmap kindArity mk))
    _ -> error $ "tyConArity': Supplied name reified to something other than a data declaration: " ++ show n

-- | Determine the constructors bound by a data or newtype declaration. Errors out if supplied with another
-- sort of declaration.
decCons :: Dec -> [Con]
decCons d = case d of
  DataD _ _ _ _ cs _ -> cs
  NewtypeD _ _ _ _ c _ -> [c]
  _ -> error "decCons: Declaration found was not a data or newtype declaration."

-- | Determine the arity of a data constructor.
conArity :: Con -> Int
conArity c = case c of
  NormalC _ ts -> length ts
  RecC _ ts -> length ts
  InfixC _ _ _ -> 2
  ForallC _ _ c' -> conArity c'
  GadtC _ ts _ -> length ts
  RecGadtC _ ts _ -> length ts
