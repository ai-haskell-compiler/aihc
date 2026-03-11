{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Control.Isomorphism.Partial.TH
  ( constructorIso
  , defineIsomorphisms
  ) where

import           Control.Monad
import           Data.Char                          (toLower)
import           Data.List                          (find)
import           Language.Haskell.TH

import           Control.Isomorphism.Partial.Unsafe (Iso (Iso))

gadtError :: a
gadtError = error "Control.Isomorphism.Partial.TH: GADTs currently not supported."
{-# NOINLINE gadtError #-}

-- | Extract the name of a constructor, e.g. ":" or "Just".
conName :: Con -> Name
conName (NormalC name _)   =   name
conName (RecC name _)      =   name
conName (InfixC _ name _)  =   name
conName (ForallC _ _ con)  =   conName con
conName (GadtC {})         =   gadtError
conName (RecGadtC {})      =   gadtError

-- | Extract the types of the constructor's fields.
conFields :: Con -> [Type]
conFields (NormalC _ fields)  =   map snd fields
conFields (RecC _ fields)     =   map (\(_, _, t) -> t) fields
conFields (InfixC lhs _ rhs)  =   map snd [lhs, rhs]
conFields (ForallC _ _ con)   =   conFields con
conFields (GadtC {})          =   gadtError
conFields (RecGadtC {})       =   gadtError

-- Data dec information
data DecInfo flag = DecInfo Type [TyVarBndr flag] [Con]

-- | Extract data or newtype declaration information
decInfo :: Dec -> Q (DecInfo BndrVis)
decInfo (DataD    _ name tyVars _ cs _) =  return $ DecInfo (ConT name) tyVars cs
decInfo (NewtypeD _ name tyVars _ c _) =  return $ DecInfo (ConT name) tyVars [c]
decInfo _ = fail "partial isomorphisms can only be derived for constructors of data type or newtype declarations."

-- | Convert tyVarBndr to type
tyVarBndrToType :: TyVarBndr BndrVis -> Type
tyVarBndrToType (PlainTV n _)   = VarT n
tyVarBndrToType (KindedTV n _ k) = SigT (VarT n) k

-- | Create Iso type for specified type and conctructor fields (Iso (a, b) (CustomType a b c))
isoType :: Type -> [TyVarBndr BndrVis] -> [Type] -> Q Type
isoType typ tyVarBndrs fields = do
    isoCon <- [t| Iso |]
    return $ ForallT (map specified tyVarBndrs) [] $ isoCon `AppT` isoArgs fields `AppT` applyAll typ (map tyVarBndrToType tyVarBndrs)
    where
      specified (PlainTV name _) = PlainTV name SpecifiedSpec
      specified (KindedTV name _ kind) = KindedTV name SpecifiedSpec kind

isoArgs :: [Type] -> Type
isoArgs []     = TupleT 0
isoArgs [x]    = x
isoArgs (x:xs) = AppT (AppT (TupleT 2) x) (isoArgs xs)

-- | Apply all types to supplied type
applyAll :: Type -> [Type] -> Type
applyAll = foldl AppT

-- | Construct a partial isomorphism expression for a constructor,
-- given the constructor's name.
constructorIso :: Name -> ExpQ
constructorIso name = do
  DataConI n _ d    <-  reify name
  TyConI dec        <-  reify d
  DecInfo _ _ cs    <-  decInfo dec
  let Just con      =   find (\c -> n == conName c) cs
  isoFromCon (wildcard cs) con

wildcard :: [Con] -> [MatchQ]
wildcard cs =  [match wildP (normalB [| Nothing |]) [] | length cs > 1]

-- | Converts a constructor name (starting with an upper-case
--   letter) into a function name (starting with a lower-case
--   letter).
rename :: Name -> Name
rename n = mkName $ case nameBase n of 
                      c:cs -> toLower c : cs
                      a -> a

defineIsomorphisms :: Name -> Q [Dec]
defineIsomorphisms d = do
  TyConI dec  <-  reify d
  DecInfo typ tyVarBndrs cs          <-  decInfo dec
  join `fmap` mapM (defFromCon (wildcard cs) typ tyVarBndrs) cs

-- | Constructs a partial isomorphism definition for a
--   constructor, given information about the constructor.
--   The name of the partial isomorphisms is constructed by
--   spelling the constructor name with an initial lower-case
--   letter.
defFromCon :: [MatchQ] -> Type -> [TyVarBndr BndrVis] -> Con -> DecsQ
defFromCon matches t tyVarBndrs con = do
    let funName = rename $ conName con
    sig <- SigD funName `fmap` isoType t tyVarBndrs (conFields con)
    fun <- funD funName [ clause [] (normalB (isoFromCon matches con)) [] ]
    return [sig, fun]

-- | Constructs a partial isomorphism expression for a
--   constructor, given information about the constructor.
isoFromCon :: [MatchQ] -> Con -> ExpQ
isoFromCon matches con = do
  let c     =   conName con
  let fs    =   conFields con
  let n     =   length fs
  (ps, vs)  <-  genPE n
  v         <-  newName "x"
  let f     =   lamE [nested tupP ps]
                  [| Just $(foldl appE (conE c) vs) |]
  let g     =   lamE [varP v]
                  (caseE (varE v) $
                    match (conP c ps) (normalB [| Just $(nested tupE vs) |]) [] : matches)
  [| Iso $f $g |]

genPE :: Int -> Q ([PatQ], [ExpQ])
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)

nested :: ([t] -> t) -> [t] -> t
nested tup []      =  tup []
nested _   [x]     =  x
nested tup (x:xs)  =  tup [x, nested tup xs]
