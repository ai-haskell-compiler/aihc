-- | Share the equal parts of a program.
--
-- The desugarer builds every occurrence of a name, and every type it
-- annotates, as a fresh heap object. A program that has to stay in memory
-- until the backend takes it is much smaller when every equal name and
-- every equal type is one object, so this pass rebuilds a program with
-- each distinct name and type allocated once. The result is equal to the
-- input; only the heap layout changes.
module Aihc.Fc.Share
  ( shareProgram,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Control.Monad.Trans.State.Strict (State, evalState, gets, modify')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

data ShareState = ShareState
  { shareNames :: !(Map Name (Int, Name)),
    shareTypes :: !(Map TypeKey (Int, Type)),
    shareNextId :: !Int
  }

-- | A type as its constructor and the identities of its shared parts. Two
-- types are equal exactly when their keys are equal, because equal parts
-- are already one object with one identity. So a lookup compares a few
-- integers and never walks a type.
data TypeKey
  = KeyVar !Int
  | KeyCon !Int
  | KeyApp !Int !Int
  | KeyFun !Int !Int !Int !Int
  | KeyForAll !Int !Int !Int
  | KeyEq !Int !Int
  | KeyLit !Int !TyLit
  deriving (Eq, Ord)

type Share = State ShareState

shareProgram :: Program -> Program
shareProgram program =
  evalState
    ( do
        imports <- shareImports (programImports program)
        decls <- mapM shareDecl (programDecls program)
        pure
          Program
            { programScopes = programScopes program,
              programImports = imports,
              programDecls = decls
            }
    )
    (ShareState Map.empty Map.empty 0)

-- | The one object that stands for every name equal to this one.
shareName :: Name -> Share Name
shareName name = snd <$> shareNameKeyed name

shareNameKeyed :: Name -> Share (Int, Name)
shareNameKeyed name = do
  known <- gets (Map.lookup name . shareNames)
  case known of
    Just shared -> pure shared
    Nothing -> do
      identity <- freshIdentity
      modify' (\state -> state {shareNames = Map.insert name (identity, name) (shareNames state)})
      pure (identity, name)

freshIdentity :: Share Int
freshIdentity = do
  identity <- gets shareNextId
  modify' (\state -> state {shareNextId = identity + 1})
  pure identity

-- | The one object that stands for every type equal to this one.
shareType :: Type -> Share Type
shareType ty = snd <$> shareTypeKeyed ty

-- | The parts are shared first, so an equal type that is already known is
-- found by the identities of its parts.
shareTypeKeyed :: Type -> Share (Int, Type)
shareTypeKeyed ty =
  case ty of
    TyVar name -> do
      (n, name') <- shareNameKeyed name
      intern (KeyVar n) (TyVar name')
    TyCon name -> do
      (n, name') <- shareNameKeyed name
      intern (KeyCon n) (TyCon name')
    TyApp function argument -> do
      (f, function') <- shareTypeKeyed function
      (a, argument') <- shareTypeKeyed argument
      intern (KeyApp f a) (TyApp function' argument')
    TyFun argumentRep resultRep argument result -> do
      (ar, argumentRep') <- shareTypeKeyed argumentRep
      (rr, resultRep') <- shareTypeKeyed resultRep
      (a, argument') <- shareTypeKeyed argument
      (r, result') <- shareTypeKeyed result
      intern (KeyFun ar rr a r) (TyFun argumentRep' resultRep' argument' result')
    TyForAll (Binder binderName binderType) body -> do
      (n, binderName') <- shareNameKeyed binderName
      (k, binderType') <- shareTypeKeyed binderType
      (b, body') <- shareTypeKeyed body
      intern (KeyForAll n k b) (TyForAll (Binder binderName' binderType') body')
    TyEq left right -> do
      (l, left') <- shareTypeKeyed left
      (r, right') <- shareTypeKeyed right
      intern (KeyEq l r) (TyEq left' right')
    TyLit kindName literal -> do
      (n, kindName') <- shareNameKeyed kindName
      intern (KeyLit n literal) (TyLit kindName' literal)
  where
    intern key rebuilt = do
      known <- gets (Map.lookup key . shareTypes)
      case known of
        Just shared -> pure shared
        Nothing -> do
          identity <- freshIdentity
          modify' (\state -> state {shareTypes = Map.insert key (identity, rebuilt) (shareTypes state)})
          pure (identity, rebuilt)

shareBinder :: Binder -> Share Binder
shareBinder binder = Binder <$> shareName (binderName binder) <*> shareType (binderType binder)

shareImports :: Imports -> Share Imports
shareImports imports =
  Imports
    <$> shareTypeMap (importHeaders imports)
    <*> shareTypeMap (importSynonyms imports)
    <*> (Map.fromList <$> mapM (\(name, axiom) -> (,) <$> shareName name <*> shareAxiomDecl axiom) (Map.toList (importAxioms imports)))
    <*> shareTypeMap (importBinders imports)
    <*> (Map.fromList <$> mapM (\(name, representation) -> (,representation) <$> shareName name) (Map.toList (importConRepresentations imports)))
    <*> (Map.fromList <$> mapM (\(name, fields) -> (,fields) <$> shareName name) (Map.toList (importConStrictFields imports)))
    <*> (Map.fromList <$> mapM (\(name, constructors) -> (,) <$> shareName name <*> mapM shareName constructors) (Map.toList (importDataCons imports)))
  where
    shareTypeMap table =
      Map.fromList <$> mapM (\(name, ty) -> (,) <$> shareName name <*> shareType ty) (Map.toList table)

shareDecl :: Decl -> Share Decl
shareDecl decl =
  case decl of
    DeclType typeDecl ->
      DeclType
        <$> ( TypeDecl (typeVis typeDecl)
                <$> shareName (typeName typeDecl)
                <*> mapM shareBinder (typeBinders typeDecl)
                <*> shareType (typeResult typeDecl)
                <*> pure (typeRoles typeDecl)
                <*> mapM shareConDecl (typeCons typeDecl)
            )
    DeclSynonym synonym ->
      DeclSynonym
        <$> ( SynonymDecl (synVis synonym)
                <$> shareName (synName synonym)
                <*> mapM shareBinder (synBinders synonym)
                <*> shareType (synResult synonym)
                <*> shareType (synBody synonym)
            )
    DeclAxiom axiom -> DeclAxiom <$> shareAxiomDecl axiom
    DeclVal value ->
      DeclVal
        <$> ( ValDecl (valVis value)
                <$> shareName (valName value)
                <*> shareType (valType value)
                <*> shareExpr (valBody value)
                <*> pure (valInline value)
            )
    DeclRule rule ->
      DeclRule
        <$> ( RuleDecl (ruleName rule) (ruleActivation rule)
                <$> mapM shareBinder (ruleTypeBinders rule)
                <*> mapM shareBinder (ruleBinders rule)
                <*> shareType (ruleType rule)
                <*> shareExpr (ruleLhs rule)
                <*> shareExpr (ruleRhs rule)
            )

shareConDecl :: ConDecl -> Share ConDecl
shareConDecl con = ConDecl (conVis con) <$> shareName (conName con) <*> shareType (conType con) <*> pure (conRepresentation con) <*> pure (conStrictFields con)

shareAxiomDecl :: AxiomDecl -> Share AxiomDecl
shareAxiomDecl axiom =
  AxiomDecl (axiomVis axiom)
    <$> shareName (axiomName axiom)
    <*> mapM shareBinder (axiomBinders axiom)
    <*> pure (axiomRole axiom)
    <*> shareType (axiomLeft axiom)
    <*> shareType (axiomRight axiom)

shareExpr :: Expr -> Share Expr
shareExpr expr =
  case expr of
    ExVar name -> ExVar <$> shareName name
    ExLit literal -> ExLit <$> shareLiteral literal
    ExApp function argument -> ExApp <$> shareExpr function <*> shareExpr argument
    ExTyApp function ty -> ExTyApp <$> shareExpr function <*> shareType ty
    ExLam binder body -> ExLam <$> shareBinder binder <*> shareExpr body
    ExTyLam binder body -> ExTyLam <$> shareBinder binder <*> shareExpr body
    ExLet bind body -> ExLet <$> shareBind bind <*> shareExpr body
    ExRec binds body -> ExRec <$> mapM shareBind binds <*> shareExpr body
    ExCase scrutinee binder ty alts ->
      ExCase <$> shareExpr scrutinee <*> shareBinder binder <*> shareType ty <*> mapM shareAlt alts
    ExCast body coercion -> ExCast <$> shareExpr body <*> shareCoercion coercion
    ExCoercion coercion -> ExCoercion <$> shareCoercion coercion
    ExForeignCall call tys arguments ->
      ExForeignCall <$> shareForeignCall call <*> mapM shareType tys <*> mapM shareExpr arguments

shareBind :: Bind -> Share Bind
shareBind bind = Bind <$> shareBinder (bindBinder bind) <*> shareExpr (bindRhs bind)

shareAlt :: Alt -> Share Alt
shareAlt alt =
  Alt
    <$> shareAltCon (altCon alt)
    <*> mapM shareBinder (altTypeBinders alt)
    <*> mapM shareBinder (altBinders alt)
    <*> shareExpr (altRhs alt)

shareAltCon :: AltCon -> Share AltCon
shareAltCon con =
  case con of
    AltData name -> AltData <$> shareName name
    AltLit literal -> AltLit <$> shareLiteral literal
    AltDefault -> pure AltDefault

shareLiteral :: Literal -> Share Literal
shareLiteral literal =
  case literal of
    LitInt ty value -> LitInt <$> shareType ty <*> pure value
    LitChar ty value -> LitChar <$> shareType ty <*> pure value
    LitAddr ty value -> LitAddr <$> shareType ty <*> pure value

shareCoercion :: Coercion -> Share Coercion
shareCoercion coercion =
  case coercion of
    CoVar name -> CoVar <$> shareName name
    CoRefl ty -> CoRefl <$> shareType ty
    CoSym inner -> CoSym <$> shareCoercion inner
    CoTrans left right -> CoTrans <$> shareCoercion left <*> shareCoercion right
    CoApp left right -> CoApp <$> shareCoercion left <*> shareCoercion right
    CoFun left right -> CoFun <$> shareCoercion left <*> shareCoercion right
    CoForAll binder body -> CoForAll <$> shareBinder binder <*> shareCoercion body
    CoNth index inner -> CoNth index <$> shareCoercion inner
    CoTyConApp name arguments -> CoTyConApp <$> shareName name <*> mapM shareCoercion arguments
    CoAxiom name tys -> CoAxiom <$> shareName name <*> mapM shareType tys

shareForeignCall :: ForeignCall -> Share ForeignCall
shareForeignCall call =
  ForeignCall
    <$> shareName (foreignCallName call)
    <*> pure (foreignCallConvention call)
    <*> mapM shareDependency (foreignCallDependencies call)
    <*> shareType (foreignCallType call)
  where
    shareDependency dependency =
      case dependency of
        ForeignAxiom name -> ForeignAxiom <$> shareName name
        ForeignConstructor name -> ForeignConstructor <$> shareName name
