-- | Share equal parts of a unit's new interface facts.
--
-- Apply this pass before the merge with imported facts and the export
-- projection. Each module then retains the same objects for imported
-- facts. A new copy of those facts would increase both time and memory.
-- The result equals the input. Only the heap layout changes.
module Aihc.Tc.Share
  ( shareTcInterface,
  )
where

import Aihc.Tc.Annotations (TcForeignImportAnnotation (..), TcForeignImportInfo (..), TcForeignMarshal (..), TcForeignTarget (..))
import Aihc.Tc.Env
import Aihc.Tc.Interface
import Aihc.Tc.Types
import Control.Monad (mapAndUnzipM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Share equal parts of the facts that one unit declares.
shareTcInterface :: TcInterface -> TcInterface
shareTcInterface interface =
  case runShare (shareInterface interface) emptyState of
    Result shared _ -> shared

-- The tables of the parts seen so far. A part is looked up after its own
-- parts are shared, and each shared part has an identity. So a table is
-- keyed by the constructor of a part and the identities of its parts: a
-- lookup compares a few integers and never walks a type. Each distinct
-- part ends up in a table once, and the tables die with the pass.
data ShareState = ShareState
  { stateTyCons :: !(Map TyCon (Int, TyCon)),
    stateTyVars :: !(Map TyVarKey (Int, TyVarId)),
    stateTypes :: !(Map TypeKey (Int, TcType)),
    statePreds :: !(Map PredKey (Int, Pred)),
    stateSchemes :: !(Map SchemeKey TypeScheme),
    stateNextId :: !Int
  }

-- | A type variable as its unique, the identity of its kind, and its name.
-- The unique comes first, because it is the cheapest part to compare.
data TyVarKey = TyVarKey !Unique !Int !Text
  deriving (Eq, Ord)

-- | A type as its constructor and the identities of its shared parts. Two
-- types are equal exactly when their keys are equal, because equal parts
-- are already one object with one identity.
data TypeKey
  = KeyTyVar !Int
  | KeyMetaTv !Unique
  | KeyTyCon !Int ![Int]
  | KeyArrow
  | KeyFun !Int !Int
  | KeyForAll !Int !Int
  | KeyQual ![Int] !Int
  | KeyApp !Int !Int
  | KeyLit !TyLit
  | KeyKindedTyCon !Int ![Int]
  deriving (Eq, Ord)

-- | A predicate as its constructor and the identities of its shared parts.
data PredKey
  = KeyClass !Int ![Int]
  | KeyEq !Int !Int
  | KeyQuantified ![Int] ![Int] !Int
  | KeyIParam !Text !Int
  | KeyIrred !Int
  deriving (Eq, Ord)

-- | A scheme as the identities of its inferred binders, specified binders,
-- predicates, and body.
data SchemeKey = SchemeKey ![Int] ![Int] ![Int] !Int
  deriving (Eq, Ord)

emptyState :: ShareState
emptyState = ShareState Map.empty Map.empty Map.empty Map.empty Map.empty 0

-- | A strict state monad: every result is evaluated as it is produced, so
-- the rebuilt interface holds no thunk that could keep a table alive.
newtype Share a = Share {runShare :: ShareState -> Result a}

data Result a = Result !a !ShareState

instance Functor Share where
  fmap f (Share run) =
    Share
      ( \state ->
          case run state of
            Result value state' -> Result (f value) state'
      )

instance Applicative Share where
  pure value = Share (Result value)
  Share runF <*> Share runX =
    Share
      ( \state ->
          case runF state of
            Result f state' ->
              case runX state' of
                Result x state'' -> Result (f x) state''
      )

instance Monad Share where
  Share run >>= continue =
    Share
      ( \state ->
          case run state of
            Result value state' -> runShare (continue value) state'
      )

-- | The one object for a part and its identity: the table's, or this one
-- once recorded with a new identity.
intern :: (Ord key) => (ShareState -> Map key (Int, part)) -> (Map key (Int, part) -> ShareState -> ShareState) -> key -> part -> Share (Int, part)
intern select store key part =
  Share
    ( \state ->
        case Map.lookup key (select state) of
          Just known -> Result known state
          Nothing ->
            let identity = stateNextId state
                recorded = (identity, part)
             in Result recorded ((store (Map.insert key recorded (select state)) state) {stateNextId = identity + 1})
    )

-- Sharing of the parts, bottom up.

shareTyCon :: TyCon -> Share TyCon
shareTyCon tyCon = snd <$> shareTyConKeyed tyCon

shareTyConKeyed :: TyCon -> Share (Int, TyCon)
shareTyConKeyed tyCon = intern stateTyCons (\table state -> state {stateTyCons = table}) tyCon tyCon

shareTyVar :: TyVarId -> Share TyVarId
shareTyVar variable = snd <$> shareTyVarKeyed variable

shareTyVarKeyed :: TyVarId -> Share (Int, TyVarId)
shareTyVarKeyed variable = do
  (kindId, kind) <- shareTypeKeyed (tvKind variable)
  let unique = tvUnique variable
      name = tvName variable
  intern stateTyVars (\table state -> state {stateTyVars = table}) (TyVarKey unique kindId name) (mkTyVarId name unique kind)

shareType :: TcType -> Share TcType
shareType ty = snd <$> shareTypeKeyed ty

shareTypeKeyed :: TcType -> Share (Int, TcType)
shareTypeKeyed ty =
  case ty of
    TcTyVar variable -> do
      (v, variable') <- shareTyVarKeyed variable
      record (KeyTyVar v) (TcTyVar variable')
    TcMetaTv unique -> record (KeyMetaTv unique) ty
    TcTyCon tyCon arguments -> do
      (c, tyCon') <- shareTyConKeyed tyCon
      (as, arguments') <- mapAndUnzipM shareTypeKeyed arguments
      record (KeyTyCon c as) (TcTyCon tyCon' arguments')
    TcKindedTyCon tyCon kindArguments -> do
      (c, tyCon') <- shareTyConKeyed tyCon
      (ks, kindArguments') <- mapAndUnzipM shareTypeKeyed kindArguments
      record (KeyKindedTyCon c ks) (TcKindedTyCon tyCon' kindArguments')
    TcArrowTy -> record KeyArrow ty
    TcTyLit literal -> record (KeyLit literal) ty
    TcFunTy argument result -> do
      (a, argument') <- shareTypeKeyed argument
      (r, result') <- shareTypeKeyed result
      record (KeyFun a r) (TcFunTy argument' result')
    TcForAllTy variable body -> do
      (v, variable') <- shareTyVarKeyed variable
      (b, body') <- shareTypeKeyed body
      record (KeyForAll v b) (TcForAllTy variable' body')
    TcQualTy predicates body -> do
      (ps, predicates') <- mapAndUnzipM sharePredKeyed predicates
      (b, body') <- shareTypeKeyed body
      record (KeyQual ps b) (TcQualTy predicates' body')
    TcAppTy function argument -> do
      (f, function') <- shareTypeKeyed function
      (a, argument') <- shareTypeKeyed argument
      record (KeyApp f a) (TcAppTy function' argument')
  where
    record = intern stateTypes (\table state -> state {stateTypes = table})

sharePred :: Pred -> Share Pred
sharePred predicate = snd <$> sharePredKeyed predicate

sharePredKeyed :: Pred -> Share (Int, Pred)
sharePredKeyed predicate =
  case predicate of
    ClassPred tyCon arguments -> do
      (c, tyCon') <- shareTyConKeyed tyCon
      (as, arguments') <- mapAndUnzipM shareTypeKeyed arguments
      record (KeyClass c as) (ClassPred tyCon' arguments')
    EqPred left right -> do
      (l, left') <- shareTypeKeyed left
      (r, right') <- shareTypeKeyed right
      record (KeyEq l r) (EqPred left' right')
    QuantifiedPred variables antecedents consequent -> do
      (vs, variables') <- mapAndUnzipM shareTyVarKeyed variables
      (as, antecedents') <- mapAndUnzipM sharePredKeyed antecedents
      (c, consequent') <- sharePredKeyed consequent
      record (KeyQuantified vs as c) (QuantifiedPred variables' antecedents' consequent')
    IParamPred name payload -> do
      (p, payload') <- shareTypeKeyed payload
      record (KeyIParam name p) (IParamPred name payload')
    IrredPred constraint -> do
      (c, constraint') <- shareTypeKeyed constraint
      record (KeyIrred c) (IrredPred constraint')
  where
    record = intern statePreds (\table state -> state {statePreds = table})

shareScheme :: TypeScheme -> Share TypeScheme
shareScheme (Scheme inferred specified predicates body) = do
  (is, inferred') <- mapAndUnzipM shareTyVarKeyed inferred
  (ss, specified') <- mapAndUnzipM shareTyVarKeyed specified
  (ps, predicates') <- mapAndUnzipM sharePredKeyed predicates
  (b, body') <- shareTypeKeyed body
  let key = SchemeKey is ss ps b
      rebuilt = Scheme inferred' specified' predicates' body'
  Share
    ( \state ->
        case Map.lookup key (stateSchemes state) of
          Just known -> Result known state
          Nothing -> Result rebuilt state {stateSchemes = Map.insert key rebuilt (stateSchemes state)}
    )

-- The interface and its facts.

shareInterface :: TcInterface -> Share TcInterface
shareInterface interface =
  TcInterface
    <$> traverse shareScheme (tcInterfaceTermMap interface)
    <*> traverse shareTyConInfo (tcInterfaceTyConMap interface)
    <*> traverse shareDataTypeInfo (tcInterfaceDataTypeMap interface)
    <*> traverse shareClassInfo (tcInterfaceClassMap interface)
    <*> traverse shareInstanceInfo (tcInterfaceInstanceMap interface)
    <*> traverse shareDataFamilyInstanceInfo (tcInterfaceDataFamilyInstanceMap interface)
    <*> traverse shareTypeFamilyInstanceInfo (tcInterfaceTypeFamilyInstanceMap interface)
    <*> traverse sharePatSynInfo (tcInterfacePatSynMap interface)
    <*> traverse shareForeignImportInfo (tcInterfaceForeignImportMap interface)
    <*> pure (tcInterfaceUniqueBound interface)

shareTyConInfo :: TyConInfo -> Share TyConInfo
shareTyConInfo info = do
  tyCon <- shareTyCon (tciTyCon info)
  kindScheme <- shareScheme (tciKindScheme info)
  synonym <- traverse shareTypeSynonymInfo (tciTypeSynonym info)
  pure info {tciTyCon = tyCon, tciKindScheme = kindScheme, tciTypeSynonym = synonym}

shareTypeSynonymInfo :: TypeSynonymInfo -> Share TypeSynonymInfo
shareTypeSynonymInfo info =
  TypeSynonymInfo <$> mapM shareTyVar (tsiParams info) <*> traverse shareType (tsiBody info)

shareDataTypeInfo :: DataTypeInfo -> Share DataTypeInfo
shareDataTypeInfo info = do
  tyCon <- shareTyCon (dtiTyCon info)
  tyVars <- mapM shareTyVar (dtiTyVars info)
  resultKind <- shareType (dtiResultKind info)
  constructors <- mapM shareDataConInfo (dtiConstructors info)
  pure info {dtiTyCon = tyCon, dtiTyVars = tyVars, dtiResultKind = resultKind, dtiConstructors = constructors}

shareDataConInfo :: DataConInfo -> Share DataConInfo
shareDataConInfo info = do
  univTyVars <- mapM shareTyVar (dciUnivTyVars info)
  exTyVars <- mapM shareTyVar (dciExTyVars info)
  theta <- mapM sharePred (dciTheta info)
  fields <- mapM shareField (dciFields info)
  resTy <- shareType (dciResTy info)
  pure info {dciUnivTyVars = univTyVars, dciExTyVars = exTyVars, dciTheta = theta, dciFields = fields, dciResTy = resTy}
  where
    shareField field = do
      ty <- shareType (dcfiType field)
      rep <- shareFieldRep (dcfiRep field)
      pure field {dcfiType = ty, dcfiRep = rep}
    shareFieldRep rep =
      case rep of
        RepStored ty strict -> RepStored <$> shareType ty <*> pure strict
        RepUnpack key leaves -> RepUnpack key <$> mapM shareFieldRep leaves
        RepCast tyCon arguments inner ->
          RepCast <$> shareTyCon tyCon <*> mapM shareType arguments <*> shareFieldRep inner

shareClassInfo :: ClassInfo -> Share ClassInfo
shareClassInfo info = do
  tyCon <- shareTyCon (ciTyCon info)
  kindTyVars <- mapM shareTyVar (ciKindTyVars info)
  tyVars <- mapM shareTyVar (ciTyVars info)
  superClassTypes <- mapM shareType (ciSuperClassTypes info)
  methods <- mapM shareNamedScheme (ciMethods info)
  defaultSignatures <- mapM shareNamedScheme (ciDefaultSignatures info)
  associatedTypes <- mapM shareAssociatedTypeInfo (ciAssociatedTypes info)
  pure
    info
      { ciTyCon = tyCon,
        ciKindTyVars = kindTyVars,
        ciTyVars = tyVars,
        ciSuperClassTypes = superClassTypes,
        ciMethods = methods,
        ciDefaultSignatures = defaultSignatures,
        ciAssociatedTypes = associatedTypes
      }
  where
    shareNamedScheme (name, scheme) = (,) name <$> shareScheme scheme

shareAssociatedTypeInfo :: AssociatedTypeInfo -> Share AssociatedTypeInfo
shareAssociatedTypeInfo info = do
  tyCon <- shareTyCon (atiTyCon info)
  defaultEquation <- traverse shareTypeFamilyInstanceInfo (atiDefault info)
  pure info {atiTyCon = tyCon, atiDefault = defaultEquation}

shareInstanceInfo :: InstanceInfo -> Share InstanceInfo
shareInstanceInfo info = do
  classTyCon <- shareTyCon (iiClass info)
  dictType <- shareType (iiDictType info)
  tyVars <- mapM shareTyVar (iiTyVars info)
  context <- mapM sharePred (iiContext info)
  headTypes <- mapM shareType (iiHead info)
  pure info {iiClass = classTyCon, iiDictType = dictType, iiTyVars = tyVars, iiContext = context, iiHead = headTypes}

shareDataFamilyInstanceInfo :: DataFamilyInstanceInfo -> Share DataFamilyInstanceInfo
shareDataFamilyInstanceInfo info = do
  familyType <- shareType (dfiiFamilyType info)
  tyVars <- mapM shareTyVar (dfiiTyVars info)
  representation <- shareTyCon (dfiiRepresentationTyCon info)
  constructors <- mapM shareDataConInfo (dfiiConstructors info)
  pure info {dfiiFamilyType = familyType, dfiiTyVars = tyVars, dfiiRepresentationTyCon = representation, dfiiConstructors = constructors}

shareTypeFamilyInstanceInfo :: TypeFamilyInstanceInfo -> Share TypeFamilyInstanceInfo
shareTypeFamilyInstanceInfo info = do
  tyVars <- mapM shareTyVar (tfiiTyVars info)
  left <- shareType (tfiiLeft info)
  right <- shareType (tfiiRight info)
  pure info {tfiiTyVars = tyVars, tfiiLeft = left, tfiiRight = right}

sharePatSynInfo :: PatSynInfo -> Share PatSynInfo
sharePatSynInfo info = do
  scheme <- shareScheme (psiScheme info)
  reqTheta <- mapM sharePred (psiReqTheta info)
  provTheta <- mapM sharePred (psiProvTheta info)
  pure info {psiScheme = scheme, psiReqTheta = reqTheta, psiProvTheta = provTheta}

shareForeignImportInfo :: TcForeignImportInfo -> Share TcForeignImportInfo
shareForeignImportInfo info =
  case info of
    TcForeignPrimImport -> pure TcForeignPrimImport
    TcForeignCCallImport safety plan -> do
      arguments <- mapM shareMarshal (tcForeignArguments plan)
      result <- shareMarshal (tcForeignResult plan)
      target <- case tcForeignTarget plan of
        TcForeignWrapper pointer -> TcForeignWrapper <$> shareMarshal pointer
        TcForeignFunctionAddress pointer -> TcForeignFunctionAddress <$> shareMarshal pointer
        other -> pure other
      pure (TcForeignCCallImport safety plan {tcForeignArguments = arguments, tcForeignResult = result, tcForeignTarget = target})
  where
    shareMarshal marshal = do
      sourceType <- shareType (tcForeignSourceType marshal)
      primitiveType <- shareType (tcForeignPrimitiveType marshal)
      pure marshal {tcForeignSourceType = sourceType, tcForeignPrimitiveType = primitiveType}
