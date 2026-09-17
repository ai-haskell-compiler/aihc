-- | Share the equal parts of type interfaces.
--
-- The type checker builds every type it records as a fresh heap object,
-- and the artifact decoder does the same for every type it reads. An
-- interface lives for the rest of an install (its dependents check against
-- it, and the package artifacts are assembled from it at the end), so it
-- is much smaller when each distinct type variable, type, predicate, type
-- scheme, and type constructor is one object. This pass rebuilds
-- interfaces so that they share every equal part. The result is equal to
-- the input; only the heap layout changes.
module Aihc.Tc.Share
  ( shareTcInterfaces,
  )
where

import Aihc.Tc
  ( AssociatedTypeInfo (..),
    ClassInfo (..),
    DataConFieldInfo (..),
    DataConInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    PatSynInfo (..),
    Pred (..),
    TcInterface (..),
    TcType (..),
    TyCon,
    TyConInfo (..),
    TyVarId,
    TypeFamilyInstanceInfo (..),
    TypeScheme (..),
    tvKind,
    tvName,
    tvUnique,
  )
import Aihc.Tc.Annotations (TcForeignImportAnnotation (..), TcForeignImportInfo (..), TcForeignMarshal (..))
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Aihc.Tc.Types (mkTyVarId, traverseScheme)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | Share the parts of several interfaces with one another, for interfaces
-- that live together: the modules of one unit, or of one package.
shareTcInterfaces :: [TcInterface] -> [TcInterface]
shareTcInterfaces interfaces =
  case runShare (mapM shareInterface interfaces) emptyState of
    Result shared _ -> shared

-- The tables of the parts seen so far. A part is looked up after its own
-- parts are shared, so an equal part compares against parts that are the
-- same objects, and each distinct part ends up in a table once. The keys
-- are the shared parts themselves: a table costs its spine and nothing
-- else, and it dies with the pass.
data ShareState = ShareState
  { stateTyCons :: !(Map TyCon TyCon),
    stateTyVars :: !(Map TyVarId TyVarId),
    stateTypes :: !(Map TcType TcType),
    statePreds :: !(Map Pred Pred),
    stateSchemes :: !(Map TypeScheme TypeScheme)
  }

emptyState :: ShareState
emptyState = ShareState Map.empty Map.empty Map.empty Map.empty Map.empty

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

-- | The one object for a part: the table's, or this one once recorded.
intern :: (Ord part) => (ShareState -> Map part part) -> (Map part part -> ShareState -> ShareState) -> part -> Share part
intern select store part =
  Share
    ( \state ->
        case Map.lookup part (select state) of
          Just known -> Result known state
          Nothing -> Result part (store (Map.insert part part (select state)) state)
    )

-- Sharing of the parts, bottom up.

shareTyCon :: TyCon -> Share TyCon
shareTyCon = intern stateTyCons (\table state -> state {stateTyCons = table})

shareTyVar :: TyVarId -> Share TyVarId
shareTyVar variable = do
  kind <- shareType (tvKind variable)
  intern stateTyVars (\table state -> state {stateTyVars = table}) (mkTyVarId (tvName variable) (tvUnique variable) kind)

shareType :: TcType -> Share TcType
shareType ty = do
  rebuilt <-
    case ty of
      TcTyVar variable -> TcTyVar <$> shareTyVar variable
      TcMetaTv unique -> pure (TcMetaTv unique)
      TcTyCon tyCon arguments -> TcTyCon <$> shareTyCon tyCon <*> mapM shareType arguments
      TcArrowTy -> pure TcArrowTy
      TcTyLit {} -> pure ty
      TcFunTy argument result -> TcFunTy <$> shareType argument <*> shareType result
      TcForAllTy variable body -> TcForAllTy <$> shareTyVar variable <*> shareType body
      TcQualTy predicates body -> TcQualTy <$> mapM sharePred predicates <*> shareType body
      TcAppTy function argument -> TcAppTy <$> shareType function <*> shareType argument
  intern stateTypes (\table state -> state {stateTypes = table}) rebuilt

sharePred :: Pred -> Share Pred
sharePred predicate = do
  rebuilt <-
    case predicate of
      ClassPred tyCon arguments -> ClassPred <$> shareTyCon tyCon <*> mapM shareType arguments
      EqPred left right -> EqPred <$> shareType left <*> shareType right
      QuantifiedPred variables antecedents consequent ->
        QuantifiedPred <$> mapM shareTyVar variables <*> mapM sharePred antecedents <*> sharePred consequent
      IParamPred name payload -> IParamPred name <$> shareType payload
      IrredPred constraint -> IrredPred <$> shareType constraint
  intern statePreds (\table state -> state {statePreds = table}) rebuilt

shareScheme :: TypeScheme -> Share TypeScheme
shareScheme scheme = do
  rebuilt <- traverseScheme shareTyVar sharePred shareType scheme
  intern stateSchemes (\table state -> state {stateSchemes = table}) rebuilt

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
      pure field {dcfiType = ty}

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
  dictType <- shareType (iiDictType info)
  tyVars <- mapM shareTyVar (iiTyVars info)
  context <- mapM sharePred (iiContext info)
  headTypes <- mapM shareType (iiHead info)
  pure info {iiDictType = dictType, iiTyVars = tyVars, iiContext = context, iiHead = headTypes}

shareDataFamilyInstanceInfo :: DataFamilyInstanceInfo -> Share DataFamilyInstanceInfo
shareDataFamilyInstanceInfo info = do
  familyType <- shareType (dfiiFamilyType info)
  tyVars <- mapM shareTyVar (dfiiTyVars info)
  representation <- shareTyCon (dfiiRepresentationTyCon info)
  pure info {dfiiFamilyType = familyType, dfiiTyVars = tyVars, dfiiRepresentationTyCon = representation}

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
      pure (TcForeignCCallImport safety plan {tcForeignArguments = arguments, tcForeignResult = result})
  where
    shareMarshal marshal = do
      sourceType <- shareType (tcForeignSourceType marshal)
      primitiveType <- shareType (tcForeignPrimitiveType marshal)
      pure marshal {tcForeignSourceType = sourceType, tcForeignPrimitiveType = primitiveType}
