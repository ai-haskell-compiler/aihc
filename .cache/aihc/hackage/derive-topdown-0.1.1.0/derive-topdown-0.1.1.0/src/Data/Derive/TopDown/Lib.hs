{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.Lib
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.Lib where 

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Data.Generics
import GHC.Exts
import Language.Haskell.TH.ExpandSyns (expandSynsWith,noWarnTypeFamilies,expandSyns)
import Data.List (nub)
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative
import Control.Monad
import Data.Derive.TopDown.Types
import Language.Haskell.TH.Datatype (
    ConstructorInfo(..),
    DatatypeInfo(..),
    reifyDatatype
    )

noWarnExpandSynsWith :: Type -> Q Type
noWarnExpandSynsWith = expandSynsWith noWarnTypeFamilies

{-|
  Get the type variable name.
-}
getVarName :: Type -> [Name]
getVarName (VarT n) = [n]
getVarName _ = []

{-|
  Get the type variable names.
-}
getAllVarNames :: Data a => a -> [Name]
getAllVarNames = everything (++) (mkQ [] getVarName)

substitute :: (Type, Type) -> Type -> Type
substitute (VarT m, t) x@(VarT n) = if n == m 
                                    then t
                                    else x
substitute (VarT _, _) x = x
substitute (t, _) x = error $ "cannot substitute " ++ show t ++ " with " ++ show x

substituteVar :: (Type, Type) -> Type -> Type
substituteVar s = everywhere (mkT (substitute s))

substituteVars :: [(Type, Type)] -> Type -> Type
substituteVars ss y = foldr substituteVar y ss

substituteVarsTypes :: [(Type, Type)] -> [Type] -> [Type]
substituteVarsTypes ms ts = [substituteVars ms y| y <- ts]

{-|
  Is the type a type family
-}
isTypeFamily :: TypeName -> Q Bool
isTypeFamily tn = do
                info <- reify tn
                case info of
                  FamilyI (OpenTypeFamilyD _) _     -> return True
                  FamilyI (ClosedTypeFamilyD _ _) _ -> return True 
                  _                                 -> return False

isDataNewtype :: TypeName -> Q Bool
isDataNewtype tn = do
                info <- reify tn
                case info of
                  TyConI (DataD _ _ _ _ _ _)    -> return True
                  TyConI (NewtypeD _ _ _ _ _ _) -> return True 
                  _                             -> return False

{-
  For type appications like @(k a b)@, @Either Int a@, we always need to 
  get the left most type in such cases
-}
getLeftMostType :: Type -> Type
getLeftMostType (AppT t1 _) = getLeftMostType t1
getLeftMostType (ParensT t)  = getLeftMostType t
getLeftMostType t            = t

isLeftMostAppTTypeFamily :: Type -> Q Bool
isLeftMostAppTTypeFamily (getLeftMostType -> ConT n) = isTypeFamily n
isLeftMostAppTTypeFamily _                           = return False

isLeftMostAppTTypeVar :: Type -> Q Bool
isLeftMostAppTTypeVar (getLeftMostType -> VarT _) = return True
isLeftMostAppTTypeVar _                           = return False

-- not sure how to handle ArrowT with deriving yet
isLeftMostAppTArrowT :: Type -> Bool
isLeftMostAppTArrowT (getLeftMostType -> ArrowT)  = True
#if __GLASGOW_HASKELL__ >= 900
isLeftMostAppTArrowT (getLeftMostType -> MulArrowT)  = True
#endif
isLeftMostAppTArrowT _ = False

isLeftMostBuildInContextType :: Type -> Bool
isLeftMostBuildInContextType (getLeftMostType -> TupleT _)  = True
isLeftMostBuildInContextType (getLeftMostType -> ListT)     = True
isLeftMostBuildInContextType _ = False


isLeftMostAppTDataNewtype :: Type -> Q Bool
isLeftMostAppTDataNewtype (getLeftMostType -> ConT n) = isDataNewtype n
isLeftMostAppTDataNewtype _                           = return False

{-| 
  Get type variable name
-}
#if __GLASGOW_HASKELL__ >= 900
getTVBName :: TyVarBndr a -> Name
getTVBName (PlainTV name _)    = name
getTVBName (KindedTV name _ _) = name
#else
getTVBName :: TyVarBndr -> Name
getTVBName (PlainTV name)    = name
getTVBName (KindedTV name _) = name
#endif

{-| After unapplying left most cannot be AppT and AppKindT, but can be InfixT or others -}
unappTy :: Type -> [Type]
unappTy (AppT t1 t2) = unappTy t1 ++ [t2]
#if __GLASGOW_HASKELL__ >= 808
unappTy (AppKindT ty _) = unappTy ty
#endif
unappTy t = [t]

getConstrArgs :: Type -> [Type]
getConstrArgs = tail . unappTy

#if __GLASGOW_HASKELL__ >= 900
voidTyVarBndrFlag :: TyVarBndr flag -> TyVarBndr ()
voidTyVarBndrFlag (PlainTV n _) = PlainTV n ()
voidTyVarBndrFlag (KindedTV n _ k) = KindedTV n () k
#else
voidTyVarBndrFlag :: TyVarBndr -> TyVarBndr
voidTyVarBndrFlag = id
#endif


isHigherOrderClass :: ClassName -> Q Bool
isHigherOrderClass cn = do
    cla <- reify cn
    case cla of
        ClassI (ClassD _ _ vars _ _) _ 
            -> case head vars of
#if __GLASGOW_HASKELL__ >= 900
                  KindedTV _ _ k -> do 
#else
                  KindedTV _ k -> do 
#endif
                              if k == StarT
                                then return False
                                else return True
                  _ -> error $ "Cannot reify kind of class " ++ show cn
        _ -> error $ show cn ++ " is not a class"

getGadtCon :: Con -> [Con]
getGadtCon g@(GadtC _ _ _) = [g]
getGadtCon g@(RecGadtC _ _ _) = [g]
getGadtCon _ = []

getAllGadtCons :: Data a => a -> [Con]
getAllGadtCons = everything (++) (mkQ [] getGadtCon)

isGadt :: [Con] -> Bool
isGadt cons = not $ null $ concatMap getAllGadtCons cons


constrInfoGadtC :: ConstructorInfo -> Con
constrInfoGadtC = undefined

-- ^ Get all fields of constructors
getAllConsFields :: [Con] -> [Type]
getAllConsFields cons = nub $ concatMap getAllConFields cons

getAllConFields :: Con -> [Type]
getAllConFields (NormalC _ bts          ) = map snd bts
getAllConFields (RecC    _ vbts         ) = map (\(_, _, x) -> x) vbts
getAllConFields (InfixC   bt1   _    bt2) = [snd bt1] ++ [snd bt2]
getAllConFields (ForallC  tvb   _  con)   = let ns = map (getTVBName. voidTyVarBndrFlag) tvb
                                              in getAllConFields (replaceVarInForallTypeTrans ns con)
-- https://gitlab.haskell.org/ghc/ghc/-/issues/13885#note_476439
getAllConFields (GadtC    _ _ _  ) = error "Should not use this to get fields of GADT"
getAllConFields (RecGadtC _ _ _  ) = error "Should not use this to get fields of GADT"

{-| data T a1 a2 = Con1 a1 | Con2 a2 ...
 return [a1, a2], [Con1 a1, Con2 a2]
-}
#if __GLASGOW_HASKELL__ >= 900
getTyVarCons :: TypeName -> Q ([TyVarBndr ()], [Con])
#else
getTyVarCons :: TypeName -> Q ([TyVarBndr], [Con])
#endif
getTyVarCons name = do
            info <- reify name
            case info of
              TyConI dec -> 
                case dec of
                  DataD    _ _ tvbs _ cons _  -> return (map voidTyVarBndrFlag tvbs, cons)
                  NewtypeD _ _ tvbs _ con  _  -> return (map voidTyVarBndrFlag tvbs, [con])
                  TySynD   _ _ _        -> error $ show name ++ " is a type synonym and `TypeSynonymInstances' is not supported.\n"
                      ++ "If you did not derive it then this is a bug, please report this bug to the author of `derive-topdown' package."
                  x -> do
                      error $ pprint (x :: Dec) ++ " is not a data or newtype definition."
              PrimTyConI _ _ _ -> return ([], [])
              x -> error $ show x ++ " is not supported"

#if __GLASGOW_HASKELL__ >= 900
getTyVarFields :: TypeName -> Q ([TyVarBndr ()], [Type])
#else
getTyVarFields :: TypeName -> Q ([TyVarBndr], [Type])
#endif
getTyVarFields name = do
            info <- reify name
            case info of
              TyConI dec -> 
                case dec of
                  DataD    _ _ tvbs _ cons _ ->
                    -- GADT needs to rebind type variables
                    -- See https://gitlab.haskell.org/ghc/ghc/-/issues/13885
                    if isGadt cons
                      then do
                        t <- reifyDatatype name
                        let vars = datatypeVars t
                        let fields = concatMap constructorFields (datatypeCons t)
                        return (vars, fields)
                      else do
                        return $ (map voidTyVarBndrFlag tvbs, getAllConsFields cons)
                  NewtypeD _ _ tvbs _ con  _  -> return (map voidTyVarBndrFlag tvbs, getAllConsFields [con])
                  TySynD   _ _ _        -> error $ show name ++ " is a type synonym and `TypeSynonymInstances' is not supported.\n"
                      ++ "If you did not derive it then this is a bug, please report this bug to the author of `derive-topdown' package."
                  x -> do
                      error $ pprint (x :: Dec) ++ " is not a data or newtype definition."
              _ -> error $ "Cannot generate instances for " ++ show name

getTypeConstructor :: Type -> Type
getTypeConstructor (AppT a1 _) = getTypeConstructor a1
getTypeConstructor a = a

reifyTypeParameters :: Name -> Q [Name]
reifyTypeParameters tn = do 
                info <- reify tn
                case info of
                  TyConI (DataD _ _ tvb _ _ _)    -> return $ map getTVBName tvb
                  TyConI (NewtypeD _ _ tvb _ _ _) -> return $ map getTVBName tvb
                  _                             -> error "impossible case in reifyTypeParameters"
              
data DecTyType = Data | Newtype | TypeSyn | BuiltIn deriving (Show, Enum, Eq)

decType :: Name -> Q DecTyType
decType name = do
         info <- reify name
         case info of
           TyConI dec -> case dec of
              DataD _ _ _ _ _ _   -> return Data
              NewtypeD _ _ _ _ _ _ -> return Newtype
              TySynD _ _ _ -> return TypeSyn
              _ -> error $ "not a type declaration: " ++ show name
           PrimTyConI _ _ _ -> return BuiltIn
           _ ->  error $ "not a type declaration: " ++ show name 


getTypeNames :: Type -> [Name]
getTypeNames (ForallT _ _ t) = getTypeNames t
getTypeNames (ConT n) = [n]
getTypeNames (AppT t1 t2) = getTypeNames t1 ++ getTypeNames t2
getTypeNames _ = []

third :: (a, b, c) -> c
third (_,_,c) = c

expandSynsAndGetTypeNames :: [Type] -> Q [TypeName]
expandSynsAndGetTypeNames ts = do
                          ts' <- mapM noWarnExpandSynsWith ts
                          return $ concatMap getTypeNames ts'

getCompositeTypeNames :: Con -> Q [TypeName]
getCompositeTypeNames (NormalC _ bts) = expandSynsAndGetTypeNames (map snd bts)
getCompositeTypeNames (RecC _ vbts) = expandSynsAndGetTypeNames (map third vbts)
getCompositeTypeNames (InfixC st1 _ st2) = expandSynsAndGetTypeNames (map snd [st1 , st2])
getCompositeTypeNames (ForallC _ _ con) = getCompositeTypeNames con
getCompositeTypeNames (GadtC _ bangtype _) = expandSynsAndGetTypeNames (map snd bangtype)
getCompositeTypeNames (RecGadtC _ bangtypes _) = expandSynsAndGetTypeNames (map third bangtypes)

{-
Here, I just replace forall type into Any type since in the deriving clause generation
process, we cannot really do anything about the quantified type vars. 
if @data C b = C (forall a. Show a => a) b@ need to derive Eq, it will failed anyway. 
if user needs to derive @Show@ for @C@ the type @a@ does not matter here. We just need 
@b@ in the context
-}
replace_var_in_forall_type :: [Name] -> Type -> Type
replace_var_in_forall_type ns v@(VarT n) = if n `elem` ns then ConT ''Any else v
replace_var_in_forall_type _ v = v

replaceVarInForallTypeTrans :: Data a => [Name] -> a -> a
replaceVarInForallTypeTrans ns = everywhere (mkT (replace_var_in_forall_type ns))

reset_forall_vars :: Type -> Type
reset_forall_vars (ForallT bs _ t) = let bns = map (getTVBName.voidTyVarBndrFlag) bs
                                         in replaceVarInForallTypeTrans bns t
#if __GLASGOW_HASKELL__ >= 810
reset_forall_vars (ForallVisT bs t) = let bns = map getTVBName bs
                                         in replaceVarInForallTypeTrans bns t 
#endif
reset_forall_vars v = v

replaceForallTWithAny :: Type -> Type
replaceForallTWithAny = everywhere (mkT reset_forall_vars)