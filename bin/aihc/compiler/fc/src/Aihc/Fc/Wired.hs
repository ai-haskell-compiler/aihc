{-# LANGUAGE OverloadedStrings #-}

-- | Wired GHC.Types names for System FC.
module Aihc.Fc.Wired
  ( wiredGhcTypes,
    typeSynonym,
    typeConstructor,
    functionArrowConstructor,
    runtimeRepConstructor,
    levityConstructor,
    liftedRepName,
    unliftedRepName,
    equalityRep,
    ghcTypesModule,
    unboxedTupleArity,
    unboxedTupleDecl,
    maxUnboxedTupleArity,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T

ghcTypesModule :: Text
ghcTypesModule = "GHC.Types"

-- | The empty tuple representation erases equality evidence.
equalityRep :: PackageId -> Type
equalityRep package =
  TyApp
    (TyCon (wiredGhcTypes package "TupleRep" SortDataConstructor))
    (TyApp (TyCon (wiredGhcTypes package "[]" SortDataConstructor)) (TyCon (runtimeRepConstructor package)))

wiredGhcTypes :: PackageId -> Text -> Sort -> Name
wiredGhcTypes package name sort =
  Name name sort (OriginTop package ghcTypesModule)

typeSynonym :: PackageId -> Type
typeSynonym package =
  TyCon (wiredGhcTypes package "Type" SortSynonym)

typeConstructor :: PackageId -> Name
typeConstructor package =
  wiredGhcTypes package "TYPE" SortTypeConstructor

-- | The function arrow as a type constructor. A saturated application of
-- it is the function type.
functionArrowConstructor :: PackageId -> Name
functionArrowConstructor package =
  wiredGhcTypes package "(->)" SortTypeConstructor

runtimeRepConstructor :: PackageId -> Name
runtimeRepConstructor package =
  wiredGhcTypes package "RuntimeRep" SortTypeConstructor

levityConstructor :: PackageId -> Name
levityConstructor package =
  wiredGhcTypes package "Levity" SortTypeConstructor

liftedRepName :: PackageId -> Name
liftedRepName package =
  wiredGhcTypes package "LiftedRep" SortSynonym

unliftedRepName :: PackageId -> Name
unliftedRepName package =
  wiredGhcTypes package "UnliftedRep" SortSynonym

-- | The largest unboxed tuple that @GHC.Types@ declares.
maxUnboxedTupleArity :: Int
maxUnboxedTupleArity = 64

-- | The package and the arity of an unboxed tuple type or constructor of
-- @GHC.Types@, such as @Tuple3#@. Only the arities from 2 to
-- 'maxUnboxedTupleArity' are given: 'unboxedTupleDecl' builds those.
unboxedTupleArity :: Name -> Maybe (PackageId, Int)
unboxedTupleArity name =
  case nameOrigin name of
    OriginTop package moduleName
      | moduleName == ghcTypesModule,
        nameSort name `elem` [SortTypeConstructor, SortDataConstructor],
        Just digits <- T.stripSuffix "#" =<< T.stripPrefix "Tuple" (nameText name),
        Right (arity, "") <- T.decimal digits,
        arity >= 2,
        arity <= maxUnboxedTupleArity ->
          Just (package, arity)
    _ -> Nothing

-- | The declaration of an unboxed tuple of an arity, as @GHC.Types@
-- declares it:
--
-- > data TupleN# (a1 :: TYPE r1) ... (aN :: TYPE rN) = (# a1, ..., aN #)
--
-- A module that does not use a tuple has no header for it. A pass can
-- make a larger tuple, for example when a worker returns the fields of a
-- component of its result, so the passes use this declaration for such a
-- tuple.
unboxedTupleDecl :: PackageId -> Int -> TypeDecl
unboxedTupleDecl package arity =
  TypeDecl
    { typeVis = Private,
      typeName = tyCon,
      typeBinders = reps <> values,
      typeResult = TyApp (TyCon (typeConstructor package)) tupleRep,
      typeRoles = replicate (2 * arity) Representational,
      typeCons =
        [ ConDecl
            { conVis = Private,
              conName = con,
              conType = foldr TyForAll fields (reps <> values),
              conRepresentation = UnboxedTupleConstructor,
              conStrictFields = []
            }
        ]
    }
  where
    count = T.pack (show arity)
    tyCon = wiredGhcTypes package ("Tuple" <> count <> "#") SortTypeConstructor
    con = wiredGhcTypes package ("Tuple" <> count <> "#") SortDataConstructor
    variable prefix index = Name (prefix <> T.pack (show index)) SortTypeVariable (OriginLocal (Unique 0))
    repVariables = [variable "r" index | index <- [1 .. arity]]
    valueVariables = [variable "a" index | index <- [1 .. arity]]
    runtimeRep = TyCon (runtimeRepConstructor package)
    reps = [Binder name runtimeRep | name <- repVariables]
    values = [Binder name (TyApp (TyCon (typeConstructor package)) (TyVar rep)) | (name, rep) <- zip valueVariables repVariables]
    cons = TyCon (wiredGhcTypes package ":" SortDataConstructor)
    nil = TyApp (TyCon (wiredGhcTypes package "[]" SortDataConstructor)) runtimeRep
    tupleRep = TyApp (TyCon (wiredGhcTypes package "TupleRep" SortDataConstructor)) (foldr (TyApp . TyApp (TyApp cons runtimeRep) . TyVar) nil repVariables)
    result = foldl TyApp (TyCon tyCon) (map TyVar (repVariables <> valueVariables))
    -- Each arrow but the last returns a function, which is lifted.
    fields = foldr field result (zip3 [1 :: Int ..] repVariables valueVariables)
    field (index, rep, value) = TyFun (TyVar rep) (if index == arity then tupleRep else TyCon (liftedRepName package)) (TyVar value)
