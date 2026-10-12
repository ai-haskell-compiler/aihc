{-# LANGUAGE ViewPatterns #-}

-- | The modules that the top names of a System FC program come from.
module Aihc.Fc.Origins
  ( programOrigins,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Resolve (PackageId)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | The package and module of each top name in a program. The printer
-- gives a scope to each of them, and the lint loads them.
programOrigins :: Program -> Set (PackageId, Text)
programOrigins program =
  importsOrigins (programImports program) <> foldMap declOrigins (programDecls program)

importsOrigins :: Imports -> Set (PackageId, Text)
importsOrigins imports =
  foldMap (\(name, ty) -> nameOriginPair name <> typeOrigins ty) (Map.toList (importHeaders imports))
    <> foldMap (\(name, ty) -> nameOriginPair name <> typeOrigins ty) (Map.toList (importSynonyms imports))
    <> foldMap (\(name, declaration) -> nameOriginPair name <> axiomOrigins declaration) (Map.toList (importAxioms imports))
    <> foldMap (\(name, ty) -> nameOriginPair name <> typeOrigins ty) (Map.toList (importBinders imports))
    <> foldMap (foldMap nameOriginPair) (Map.elems (importDataCons imports))

axiomOrigins :: AxiomDecl -> Set (PackageId, Text)
axiomOrigins declaration =
  foldMap binderOrigins (axiomBinders declaration)
    <> typeOrigins (axiomLeft declaration)
    <> typeOrigins (axiomRight declaration)

declOrigins :: Decl -> Set (PackageId, Text)
declOrigins decl =
  case decl of
    DeclType typeDecl ->
      nameOriginPair (typeName typeDecl)
        <> foldMap binderOrigins (typeBinders typeDecl)
        <> typeOrigins (typeResult typeDecl)
        <> foldMap conOrigins (typeCons typeDecl)
    DeclSynonym synonymDecl ->
      nameOriginPair (synName synonymDecl)
        <> foldMap binderOrigins (synBinders synonymDecl)
        <> typeOrigins (synResult synonymDecl)
        <> typeOrigins (synBody synonymDecl)
    DeclAxiom axiomDecl ->
      nameOriginPair (axiomName axiomDecl)
        <> foldMap binderOrigins (axiomBinders axiomDecl)
        <> typeOrigins (axiomLeft axiomDecl)
        <> typeOrigins (axiomRight axiomDecl)
    DeclVal valDecl ->
      nameOriginPair (valName valDecl)
        <> typeOrigins (valType valDecl)
        <> exprOrigins (valBody valDecl)
    DeclRule ruleDecl ->
      foldMap binderOrigins (ruleTypeBinders ruleDecl <> ruleBinders ruleDecl)
        <> typeOrigins (ruleType ruleDecl)
        <> exprOrigins (ruleLhs ruleDecl)
        <> exprOrigins (ruleRhs ruleDecl)

foreignImportDependencyOrigins :: ForeignImportDependency -> Set (PackageId, Text)
foreignImportDependencyOrigins dependency =
  case dependency of
    ForeignAxiom name -> nameOriginPair name
    ForeignConstructor name -> nameOriginPair name

conOrigins :: ConDecl -> Set (PackageId, Text)
conOrigins constructor =
  nameOriginPair (conName constructor) <> typeOrigins (conType constructor)

binderOrigins :: Binder -> Set (PackageId, Text)
binderOrigins binder = typeOrigins (binderType binder)

exprOrigins :: Expr -> Set (PackageId, Text)
exprOrigins expr =
  case expr of
    ExVar name -> nameOriginPair name
    ExLit literal ty -> literalOrigins literal <> typeOrigins ty
    ExApp function argument -> exprOrigins function <> exprOrigins argument
    ExTyApp function ty -> exprOrigins function <> typeOrigins ty
    ExLam binder body -> binderOrigins binder <> exprOrigins body
    ExTyLam binder body -> binderOrigins binder <> exprOrigins body
    ExLet bind body -> bindOrigins bind <> exprOrigins body
    ExRec binds body -> foldMap bindOrigins binds <> exprOrigins body
    ExCase scrutinee binder (NE.toList -> alts) ->
      exprOrigins scrutinee <> foldMap binderOrigins binder <> foldMap altOrigins alts
    ExCoercion proof -> coercionOrigins proof
    ExAbsurd scrutinee resultType -> exprOrigins scrutinee <> typeOrigins resultType
    ExCast inner coercion -> exprOrigins inner <> coercionOrigins coercion
    ExForeignCall call types arguments ->
      nameOriginPair (foreignCallName call)
        <> foldMap foreignImportDependencyOrigins (foreignCallDependencies call)
        <> typeOrigins (foreignCallType call)
        <> foldMap typeOrigins types
        <> foldMap exprOrigins arguments

bindOrigins :: Bind -> Set (PackageId, Text)
bindOrigins bind = binderOrigins (bindBinder bind) <> exprOrigins (bindRhs bind)

altOrigins :: Alt -> Set (PackageId, Text)
altOrigins alternative =
  altConOrigins (altCon alternative)
    <> foldMap binderOrigins (altTypeBinders alternative)
    <> foldMap binderOrigins (altBinders alternative)
    <> exprOrigins (altRhs alternative)

altConOrigins :: AltCon -> Set (PackageId, Text)
altConOrigins alternative =
  case alternative of
    AltData name -> nameOriginPair name
    AltLit literal -> literalOrigins literal
    AltDefault -> mempty

literalOrigins :: Literal -> Set (PackageId, Text)
literalOrigins literal =
  case literal of
    LitInt representation _ -> typeOrigins representation
    LitChar representation _ -> typeOrigins representation
    LitAddr representation _ -> typeOrigins representation

coercionOrigins :: Coercion -> Set (PackageId, Text)
coercionOrigins coercion =
  case coercion of
    CoVar name -> nameOriginPair name
    CoRefl ty -> typeOrigins ty
    CoSym inner -> coercionOrigins inner
    CoTrans left right -> coercionOrigins left <> coercionOrigins right
    CoApp function argument -> coercionOrigins function <> coercionOrigins argument
    CoNth _ proof -> coercionOrigins proof
    CoFun domain range -> coercionOrigins domain <> coercionOrigins range
    CoForAll binder body -> binderOrigins binder <> coercionOrigins body
    CoTyConApp name arguments -> nameOriginPair name <> foldMap coercionOrigins arguments
    CoAxiom name arguments -> nameOriginPair name <> foldMap typeOrigins arguments

typeOrigins :: Type -> Set (PackageId, Text)
typeOrigins ty =
  case ty of
    TyVar name -> nameOriginPair name
    TyCon name -> nameOriginPair name
    TyLit kindName _ -> nameOriginPair kindName
    TyApp function argument -> typeOrigins function <> typeOrigins argument
    TyFun r1 r2 argument result ->
      typeOrigins r1 <> typeOrigins r2 <> typeOrigins argument <> typeOrigins result
    TyForAll binder body -> binderOrigins binder <> typeOrigins body
    TyEq left right -> typeOrigins left <> typeOrigins right

nameOriginPair :: Name -> Set (PackageId, Text)
nameOriginPair name =
  case nameOrigin name of
    OriginTop package moduleName' -> Set.singleton (package, moduleName')
    OriginLocal {} -> mempty
