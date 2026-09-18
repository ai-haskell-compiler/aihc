module Aihc.Cli.TypeArtifact
  ( TypeArtifact (..),
    decodeTypeArtifact,
    encodeTypeArtifact,
    encodeTypeArtifactParts,
    encodeTypeInterface,
  )
where

import Aihc.Cli.Cbor (cborArray, cborInt, cborText, cborWord, getArrayLength, getInt, getText, getWord, (<*!>))
import Aihc.Cli.InterfaceParts (InterfacePart (..), PartIndex (..), interfaceParts)
import Aihc.Cli.InterfaceTyCons (interfaceTyCons)
import Aihc.Resolve (PackageId (..), ResolutionNamespace (..))
import Aihc.Tc
  ( AssociatedTypeInfo (..),
    ClassInfo (..),
    DataConFieldInfo (..),
    DataConFieldUnpack (..),
    DataConInfo (..),
    DataConSourceForm (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    FunDep (..),
    InstanceInfo (..),
    Pred (..),
    TcInterface (..),
    TcTermKey (..),
    TcType (..),
    TyCon,
    TyConFlavor (..),
    TyConInfo (..),
    TyVarId (..),
    TypeFamilyInstanceInfo (..),
    TypeScheme (..),
    Unique (..),
    tcInterfaceClasses,
    tcInterfaceDataFamilyInstances,
    tcInterfaceDataTypes,
    tcInterfaceForeignImports,
    tcInterfaceFromLists,
    tcInterfaceInstances,
    tcInterfacePatSyns,
    tcInterfaceTerms,
    tcInterfaceTyCons,
    tcInterfaceTypeFamilyInstances,
    tvKind,
    tyConArity,
    tyConName,
  )
import Aihc.Tc.Annotations (TcForeignAbiType (..), TcForeignCApi (..), TcForeignCApiKind (..), TcForeignEffect (..), TcForeignImportAnnotation (..), TcForeignImportInfo (..), TcForeignMarshal (..), TcForeignSafety (..), TcForeignTarget (..))
import Aihc.Tc.Env (CType (..), PatSynDirection (..), PatSynInfo (..), TypeSynonymInfo (..))
import Aihc.Tc.Types (TyLit (..), mkTyConWithNamespace, mkTyVarId, tyConModuleName, tyConNamespace, tyConPackageId)
import Control.Monad (replicateM, unless, when, (<$!>))
import Data.Array (Array, listArray, (!))
import Data.Binary.Get qualified as Get
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)

data TypeArtifact = TypeArtifact
  { typeArtifactModuleName :: !Text,
    typeArtifactInstanceProviders :: !(Map Text [(PackageId, Text)]),
    typeArtifactInterface :: TcInterface
  }
  deriving (Show)

-- | What a decoder resolves a reference against: the type constructors of
-- the artifact and the parts numbered before the one being read.
data PartTable = PartTable
  { tableTyCons :: !(Array Int TyCon),
    tableParts :: !(IntMap InterfacePart)
  }

encodeTypeArtifact :: TypeArtifact -> BL.ByteString
encodeTypeArtifact = fst . encodeTypeArtifactParts

encodeTypeArtifactParts :: TypeArtifact -> (BL.ByteString, BL.ByteString)
encodeTypeArtifactParts artifact =
  ( Builder.toLazyByteString $
      cborArray 5
        <> cborText "aihc-type"
        <> cborText (typeArtifactModuleName artifact)
        <> encodeList encodeModuleProviders (Map.toAscList (typeArtifactInstanceProviders artifact))
        <> Builder.lazyByteString interfaceBytes,
    interfaceBytes
  )
  where
    interfaceBytes = encodeTypeInterface (typeArtifactInterface artifact)
    encodeModuleProviders (name, providers) = cborArray 2 <> cborText name <> encodeList encodeProvider providers
    encodeProvider (packageId, moduleName) = cborArray 2 <> putPackageId packageId <> cborText moduleName

encodeTypeInterface :: TcInterface -> BL.ByteString
encodeTypeInterface interface =
  let tyCons = Set.toAscList (interfaceTyCons interface)
      tyConTable = Map.fromList (zip tyCons [0 ..])
      (parts, index) = interfaceParts tyConTable interface
   in Builder.toLazyByteString
        ( encodeList putTyConDefinition tyCons
            <> encodeList (putPart index) parts
            <> putInterface index interface
        )

decodeTypeArtifact :: BL.ByteString -> Either String TypeArtifact
decodeTypeArtifact bytes =
  case Get.runGetOrFail getArtifact bytes of
    Left (_, _, message) -> Left message
    Right (remaining, _, artifact)
      | BL.null remaining -> Right artifact
      | otherwise -> Left "invalid trailing data"

getArtifact :: Get.Get TypeArtifact
getArtifact = do
  expectArray 5
  expectText "aihc-type"
  typeArtifactModuleName <- getText
  typeArtifactInstanceProviders <- Map.fromList <$!> getList getModuleProviders
  tyCons <- getList getTyConDefinition
  let tyConTable = listArray (0, length tyCons - 1) tyCons
  interfaceBytes <- Get.getRemainingLazyByteString
  typeArtifactInterface <- either fail pure (runInterface tyConTable interfaceBytes)
  pure TypeArtifact {typeArtifactModuleName, typeArtifactInstanceProviders, typeArtifactInterface}
  where
    getModuleProviders = expectArray 2 >> ((,) <$!> getText <*!> getList getProvider)
    getProvider = expectArray 2 >> ((,) <$!> getPackageId <*!> getText)

-- | The interface part of an artifact. It is decoded on its own because the
-- type constructor table comes before it.
runInterface :: Array Int TyCon -> BL.ByteString -> Either String TcInterface
runInterface tyCons bytes =
  case Get.runGetOrFail (getPartTable tyCons >>= getInterface) bytes of
    Left (_, _, message) -> Left message
    Right (_, _, interface) -> Right interface

putInterface :: PartIndex -> TcInterface -> Builder.Builder
putInterface table interface =
  cborArray 9
    <> encodeList (putTerm table) (tcInterfaceTerms interface)
    <> encodeList (putTyConInfo table) (tcInterfaceTyCons interface)
    <> encodeList (putDataTypeInfo table) (tcInterfaceDataTypes interface)
    <> encodeList (putClassInfo table) (tcInterfaceClasses interface)
    <> encodeList (putInstanceInfo table) (tcInterfaceInstances interface)
    <> encodeList (putDataFamilyInstanceInfo table) (tcInterfaceDataFamilyInstances interface)
    <> encodeList (putTypeFamilyInstanceInfo table) (tcInterfaceTypeFamilyInstances interface)
    <> encodeList (putPatSynInfo table) (tcInterfacePatSyns interface)
    <> encodeList (putForeignImport table) (tcInterfaceForeignImports interface)

getInterface :: PartTable -> Get.Get TcInterface
getInterface table = do
  length' <- getArrayLength
  terms <- getList (getTerm table)
  tyCons <- getList (getTyConInfo table)
  dataTypes <- getList (getDataTypeInfo table)
  classes <- getList (getClassInfo table)
  instances <- getList (getInstanceInfo table)
  dataFamilyInstances <- getList (getDataFamilyInstanceInfo table)
  typeFamilyInstances <-
    if length' >= 7
      then getList (getTypeFamilyInstanceInfo table)
      else pure []
  patSyns <-
    if length' >= 8
      then getList (getPatSynInfo table)
      else pure []
  foreignImports <-
    if length' >= 9
      then getList (getForeignImport table)
      else pure []
  when (length' < 6 || length' > 9) $
    fail ("unsupported type interface array length: " <> show length')
  pure (tcInterfaceFromLists terms tyCons dataTypes classes instances dataFamilyInstances typeFamilyInstances patSyns foreignImports)

putForeignImport :: PartIndex -> (TcTermKey, TcForeignImportInfo) -> Builder.Builder
putForeignImport table (key, info) = cborArray 2 <> putTermKey key <> putForeignImportInfo table info

getForeignImport :: PartTable -> Get.Get (TcTermKey, TcForeignImportInfo)
getForeignImport table = expectArray 2 >> ((,) <$!> getTermKey <*!> getForeignImportInfo table)

putForeignImportInfo :: PartIndex -> TcForeignImportInfo -> Builder.Builder
putForeignImportInfo table info =
  case info of
    TcForeignPrimImport -> cborArray 1 <> cborWord 0
    TcForeignCCallImport safety plan ->
      cborArray 3 <> cborWord 1 <> putForeignSafety safety <> putForeignPlan table plan

getForeignImportInfo :: PartTable -> Get.Get TcForeignImportInfo
getForeignImportInfo table = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (1, 0) -> pure TcForeignPrimImport
    (3, 1) -> TcForeignCCallImport <$!> getForeignSafety <*!> getForeignPlan table
    _ -> fail "unsupported foreign import info"

putForeignSafety :: TcForeignSafety -> Builder.Builder
putForeignSafety safety =
  cborWord $
    case safety of
      TcForeignSafe -> 0
      TcForeignUnsafe -> 1
      TcForeignInterruptible -> 2

getForeignSafety :: Get.Get TcForeignSafety
getForeignSafety = do
  tag <- getWord
  case tag of
    0 -> pure TcForeignSafe
    1 -> pure TcForeignUnsafe
    2 -> pure TcForeignInterruptible
    _ -> fail "unsupported foreign safety"

putForeignPlan :: PartIndex -> TcForeignImportAnnotation -> Builder.Builder
putForeignPlan table plan =
  cborArray 6
    <> encodeList (putForeignMarshal table) (tcForeignArguments plan)
    <> putForeignMarshal table (tcForeignResult plan)
    <> putForeignEffect (tcForeignEffect plan)
    <> cborText (tcForeignSymbol plan)
    <> putForeignTarget (tcForeignTarget plan)
    <> encodeList putForeignCApi (maybeToList (tcForeignCApi plan))

getForeignPlan :: PartTable -> Get.Get TcForeignImportAnnotation
getForeignPlan table = do
  expectArray 6
  tcForeignArguments <- getList (getForeignMarshal table)
  tcForeignResult <- getForeignMarshal table
  tcForeignEffect <- getForeignEffect
  tcForeignSymbol <- getText
  tcForeignTarget <- getForeignTarget
  tcForeignCApi <- listToMaybe <$!> getList getForeignCApi
  pure TcForeignImportAnnotation {tcForeignArguments, tcForeignResult, tcForeignEffect, tcForeignSymbol, tcForeignTarget, tcForeignCApi}

-- | How a @capi@ import reaches its entity.  The header is encoded as a list
-- so that an absent header needs no separate tag.
putForeignCApi :: TcForeignCApi -> Builder.Builder
putForeignCApi capi =
  cborArray 2
    <> encodeList cborText (maybeToList (tcForeignCApiHeader capi))
    <> cborWord (case tcForeignCApiKind capi of TcForeignCApiFunction -> 0; TcForeignCApiValue -> 1)

getForeignCApi :: Get.Get TcForeignCApi
getForeignCApi = do
  expectArray 2
  tcForeignCApiHeader <- listToMaybe <$!> getList getText
  tag <- getWord
  tcForeignCApiKind <-
    case tag of
      0 -> pure TcForeignCApiFunction
      1 -> pure TcForeignCApiValue
      _ -> fail "unsupported capi foreign import kind"
  pure TcForeignCApi {tcForeignCApiHeader, tcForeignCApiKind}

putForeignMarshal :: PartIndex -> TcForeignMarshal -> Builder.Builder
putForeignMarshal table marshal =
  cborArray 5
    <> putType table (tcForeignSourceType marshal)
    <> putType table (tcForeignPrimitiveType marshal)
    <> encodeList cborText (tcForeignConstructors marshal)
    <> putForeignAbiType (tcForeignAbiType marshal)
    <> putMaybe putCType (tcForeignCType marshal)

getForeignMarshal :: PartTable -> Get.Get TcForeignMarshal
getForeignMarshal table = do
  expectArray 5
  tcForeignSourceType <- getType table
  tcForeignPrimitiveType <- getType table
  tcForeignConstructors <- getList getText
  tcForeignAbiType <- getForeignAbiType
  tcForeignCType <- getMaybe getCType
  pure TcForeignMarshal {tcForeignSourceType, tcForeignPrimitiveType, tcForeignConstructors, tcForeignAbiType, tcForeignCType}

putCType :: CType -> Builder.Builder
putCType cType = cborArray 2 <> putMaybe cborText (cTypeHeader cType) <> cborText (cTypeName cType)

getCType :: Get.Get CType
getCType = expectArray 2 >> (CType <$!> getMaybe getText <*!> getText)

putForeignEffect :: TcForeignEffect -> Builder.Builder
putForeignEffect effect =
  cborWord $
    case effect of
      TcForeignPure -> 0
      TcForeignRealWorld -> 1

getForeignEffect :: Get.Get TcForeignEffect
getForeignEffect = do
  tag <- getWord
  case tag of
    0 -> pure TcForeignPure
    1 -> pure TcForeignRealWorld
    _ -> fail "unsupported foreign effect"

putForeignTarget :: TcForeignTarget -> Builder.Builder
putForeignTarget target =
  cborWord $
    case target of
      TcForeignCall -> 0
      TcForeignAddress -> 1

getForeignTarget :: Get.Get TcForeignTarget
getForeignTarget = do
  tag <- getWord
  case tag of
    0 -> pure TcForeignCall
    1 -> pure TcForeignAddress
    _ -> fail "unsupported foreign target"

putForeignAbiType :: TcForeignAbiType -> Builder.Builder
putForeignAbiType abiType =
  cborWord $
    case abiType of
      TcForeignInt -> 0
      TcForeignInt8 -> 1
      TcForeignInt16 -> 2
      TcForeignInt32 -> 3
      TcForeignInt64 -> 4
      TcForeignWord -> 5
      TcForeignWord8 -> 6
      TcForeignWord16 -> 7
      TcForeignWord32 -> 8
      TcForeignWord64 -> 9
      TcForeignAddr -> 10
      TcForeignVoid -> 11
      TcForeignFloat -> 12
      TcForeignDouble -> 13

getForeignAbiType :: Get.Get TcForeignAbiType
getForeignAbiType = do
  tag <- getWord
  case tag of
    0 -> pure TcForeignInt
    1 -> pure TcForeignInt8
    2 -> pure TcForeignInt16
    3 -> pure TcForeignInt32
    4 -> pure TcForeignInt64
    5 -> pure TcForeignWord
    6 -> pure TcForeignWord8
    7 -> pure TcForeignWord16
    8 -> pure TcForeignWord32
    9 -> pure TcForeignWord64
    10 -> pure TcForeignAddr
    11 -> pure TcForeignVoid
    12 -> pure TcForeignFloat
    13 -> pure TcForeignDouble
    _ -> fail "unsupported foreign ABI type"

putPatSynInfo :: PartIndex -> PatSynInfo -> Builder.Builder
putPatSynInfo table info =
  cborArray 8
    <> cborText (psiName info)
    <> putOrigin (psiOrigin info)
    <> cborWord (fromIntegral (psiArity info))
    <> encodeList cborText (psiFields info)
    <> putPatSynDirection (psiDirection info)
    <> putTypeScheme table (psiScheme info)
    <> encodeList (putPred table) (psiReqTheta info)
    <> encodeList (putPred table) (psiProvTheta info)

getPatSynInfo :: PartTable -> Get.Get PatSynInfo
getPatSynInfo table = do
  expectArray 8
  psiName <- getText
  psiOrigin <- getOrigin
  psiArity <- fromIntegral <$!> getWord
  psiFields <- getList getText
  psiDirection <- getPatSynDirection
  psiScheme <- getTypeScheme table
  psiReqTheta <- getList (getPred table)
  psiProvTheta <- getList (getPred table)
  pure PatSynInfo {psiName, psiOrigin, psiArity, psiFields, psiDirection, psiScheme, psiReqTheta, psiProvTheta}

putPatSynDirection :: PatSynDirection -> Builder.Builder
putPatSynDirection direction =
  cborWord $
    case direction of
      PatSynUnidirectionalInfo -> 0
      PatSynImplicitBidirectionalInfo -> 1
      PatSynExplicitBidirectionalInfo -> 2

getPatSynDirection :: Get.Get PatSynDirection
getPatSynDirection = do
  tag <- getWord
  case tag of
    0 -> pure PatSynUnidirectionalInfo
    1 -> pure PatSynImplicitBidirectionalInfo
    2 -> pure PatSynExplicitBidirectionalInfo
    _ -> fail "unsupported pattern synonym direction"

putTerm :: PartIndex -> (TcTermKey, TypeScheme) -> Builder.Builder
putTerm table (key, scheme) = cborArray 2 <> putTermKey key <> putTypeScheme table scheme

getTerm :: PartTable -> Get.Get (TcTermKey, TypeScheme)
getTerm table = expectArray 2 >> ((,) <$!> getTermKey <*!> getTypeScheme table)

putTermKey :: TcTermKey -> Builder.Builder
putTermKey key = case key of
  TcTermLocal unique -> cborArray 2 <> cborWord 0 <> cborInt unique
  TcTermGlobal (PackageId packageId) moduleName identifier -> cborArray 4 <> cborWord 1 <> cborText packageId <> cborText moduleName <> cborText identifier

getTermKey :: Get.Get TcTermKey
getTermKey = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (2, 0) -> TcTermLocal <$!> getInt
    (4, 1) -> (TcTermGlobal . PackageId <$!> getText) <*!> getText <*!> getText
    _ -> fail "unsupported term key"

putTypeScheme :: PartIndex -> TypeScheme -> Builder.Builder
putTypeScheme index scheme = putNumber "type scheme" (Map.lookup scheme (partIndexSchemes index))

getTypeScheme :: PartTable -> Get.Get TypeScheme
getTypeScheme table = getWord >>= partScheme (tableParts table)

putTyVar :: PartIndex -> TyVarId -> Builder.Builder
putTyVar index variable = putNumber "type variable" (Map.lookup variable (partIndexTyVars index))

getTyVar :: PartTable -> Get.Get TyVarId
getTyVar table = getWord >>= partTyVar (tableParts table)

putUnique :: Unique -> Builder.Builder
putUnique (Unique value) = cborInt value

getUnique :: Get.Get Unique
getUnique = Unique <$!> getInt

putTyConDefinition :: TyCon -> Builder.Builder
putTyConDefinition tyCon =
  cborArray 5
    <> putPackageId (tyConPackageId tyCon)
    <> cborText (tyConModuleName tyCon)
    <> putResolutionNamespace (tyConNamespace tyCon)
    <> cborText (tyConName tyCon)
    <> cborInt (tyConArity tyCon)

getTyConDefinition :: Get.Get TyCon
getTyConDefinition = do
  expectArray 5
  packageId <- getPackageId
  moduleName <- getText
  namespace <- getResolutionNamespace
  mkTyConWithNamespace namespace packageId moduleName <$!> getText <*!> getInt

putTyCon :: PartIndex -> TyCon -> Builder.Builder
putTyCon index tyCon = putNumber "type constructor" (Map.lookup tyCon (partIndexTyCons index))

getTyCon :: PartTable -> Get.Get TyCon
getTyCon table = (tableTyCons table !) . fromIntegral <$!> getWord

putResolutionNamespace :: ResolutionNamespace -> Builder.Builder
putResolutionNamespace namespace =
  cborWord $
    case namespace of
      ResolutionNamespaceTerm -> 0
      ResolutionNamespaceType -> 1
      ResolutionNamespaceModule -> 2

getResolutionNamespace :: Get.Get ResolutionNamespace
getResolutionNamespace = do
  tag <- getWord
  case tag of
    0 -> pure ResolutionNamespaceTerm
    1 -> pure ResolutionNamespaceType
    2 -> pure ResolutionNamespaceModule
    _ -> fail "unsupported resolution namespace"

putPackageId :: PackageId -> Builder.Builder
putPackageId (PackageId identity) = cborText identity

getPackageId :: Get.Get PackageId
getPackageId = PackageId <$!> getText

putType :: PartIndex -> TcType -> Builder.Builder
putType index ty = putNumber "type" (Map.lookup ty (partIndexTypes index))

getType :: PartTable -> Get.Get TcType
getType table = getWord >>= partType (tableParts table)

putPred :: PartIndex -> Pred -> Builder.Builder
putPred index predicate = putNumber "predicate" (Map.lookup predicate (partIndexPreds index))

getPred :: PartTable -> Get.Get Pred
getPred table = getWord >>= partPred (tableParts table)

-- | The number of a part, as the encoder writes it in place of the part.
-- Every part an interface holds is numbered before the body is written,
-- so a missing number means the numbering pass in
-- "Aihc.Cli.InterfaceParts" no longer reaches a field that this module
-- writes.
putNumber :: String -> Maybe Word64 -> Builder.Builder
putNumber what = maybe (error ("missing interface " <> what <> " number")) cborWord

-- | The table of parts: each is built from the parts before it, so one
-- pass over the table is enough to read it.
putPart :: PartIndex -> InterfacePart -> Builder.Builder
putPart index part = case part of
  PartTyVar variable ->
    cborArray 4 <> cborWord 0 <> cborText (tvName variable) <> putUnique (tvUnique variable) <> putType index (tvKind variable)
  PartType ty -> case ty of
    TcTyVar variable -> sum1 1 (putTyVar index variable)
    TcMetaTv unique -> sum1 2 (putUnique unique)
    TcTyCon tyCon arguments -> sum2 3 (putTyCon index tyCon) (encodeList (putType index) arguments)
    TcFunTy argument result -> sum2 4 (putType index argument) (putType index result)
    TcForAllTy variable body -> sum2 5 (putTyVar index variable) (putType index body)
    TcQualTy predicates body -> sum2 6 (encodeList (putPred index) predicates) (putType index body)
    TcAppTy function argument -> sum2 7 (putType index function) (putType index argument)
    TcArrowTy -> cborArray 1 <> cborWord 8
    -- A literal is written as its sort and its value in text. A natural
    -- has no bound, so decimal text carries it where a CBOR integer
    -- could not.
    TcTyLit literal -> sum2 15 (cborWord (tyLitSortTag literal)) (cborText (tyLitPayload literal))
  PartPred predicate -> case predicate of
    ClassPred tyCon arguments -> sum2 9 (putTyCon index tyCon) (encodeList (putType index) arguments)
    EqPred left right -> sum2 10 (putType index left) (putType index right)
    QuantifiedPred variables antecedents consequent ->
      cborArray 4 <> cborWord 11 <> encodeList (putTyVar index) variables <> encodeList (putPred index) antecedents <> putPred index consequent
    IParamPred name payload -> sum2 12 (cborText name) (putType index payload)
    IrredPred constraint -> sum1 14 (putType index constraint)
  PartScheme (Scheme inferred specified predicates body) ->
    cborArray 5 <> cborWord 13 <> encodeList (putTyVar index) inferred <> encodeList (putTyVar index) specified <> encodeList (putPred index) predicates <> putType index body

-- | Which sort of literal, and its value as text. A natural is decimal.
tyLitSortTag :: TyLit -> Word64
tyLitSortTag literal =
  case literal of
    TyLitNat {} -> 0
    TyLitSymbol {} -> 1
    TyLitChar {} -> 2

tyLitPayload :: TyLit -> Text
tyLitPayload literal =
  case literal of
    TyLitNat value -> T.pack (show value)
    TyLitSymbol value -> value
    TyLitChar value -> T.singleton value

getTyLit :: Word64 -> Get.Get TyLit
getTyLit sort = do
  payload <- getText
  case sort of
    0 -> case reads (T.unpack payload) of
      [(value, "")] -> pure $! TyLitNat value
      _ -> fail "malformed type-level natural literal"
    1 -> pure $! TyLitSymbol payload
    2 -> case T.unpack payload of
      [value] -> pure $! TyLitChar value
      _ -> fail "malformed type-level character literal"
    _ -> fail "unsupported type-level literal sort"

getPartTable :: Array Int TyCon -> Get.Get PartTable
getPartTable tyCons = do
  count <- getArrayLength
  PartTable tyCons <$!> readParts 0 count IntMap.empty
  where
    readParts position count parts
      | position >= count = pure parts
      | otherwise = do
          part <- getPart tyCons parts
          readParts (position + 1) count (IntMap.insert position part parts)

getPart :: Array Int TyCon -> IntMap InterfacePart -> Get.Get InterfacePart
getPart tyCons parts = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (4, 0) -> do
      name <- getText
      unique <- getUnique
      kind <- refType
      pure $! PartTyVar (mkTyVarId name unique kind)
    (2, 1) -> typePart (TcTyVar <$!> refTyVar)
    (2, 2) -> typePart (TcMetaTv <$!> getUnique)
    (3, 3) -> typePart (TcTyCon <$!> refTyCon <*!> getList refType)
    (3, 4) -> typePart (TcFunTy <$!> refType <*!> refType)
    (3, 5) -> typePart (TcForAllTy <$!> refTyVar <*!> refType)
    (3, 6) -> typePart (TcQualTy <$!> getList refPred <*!> refType)
    (3, 7) -> typePart (TcAppTy <$!> refType <*!> refType)
    (1, 8) -> pure (PartType TcArrowTy)
    (3, 9) -> predPart (ClassPred <$!> refTyCon <*!> getList refType)
    (3, 10) -> predPart (EqPred <$!> refType <*!> refType)
    (4, 11) -> predPart (QuantifiedPred <$!> getList refTyVar <*!> getList refPred <*!> refPred)
    (3, 12) -> predPart (IParamPred <$!> getText <*!> refType)
    (2, 14) -> predPart (IrredPred <$!> refType)
    (3, 15) -> typePart (TcTyLit <$!> (getWord >>= getTyLit))
    (5, 13) -> PartScheme <$!> (Scheme <$!> getList refTyVar <*!> getList refTyVar <*!> getList refPred <*!> refType)
    _ -> fail "unsupported interface part"
  where
    typePart getValue = PartType <$!> getValue
    predPart getValue = PartPred <$!> getValue
    refType = getWord >>= partType parts
    refTyVar = getWord >>= partTyVar parts
    refPred = getWord >>= partPred parts
    refTyCon = (tyCons !) . fromIntegral <$!> getWord

partType :: IntMap InterfacePart -> Word64 -> Get.Get TcType
partType parts number = case IntMap.lookup (fromIntegral number) parts of
  Just (PartType ty) -> pure ty
  _ -> fail "invalid interface type reference"

partTyVar :: IntMap InterfacePart -> Word64 -> Get.Get TyVarId
partTyVar parts number = case IntMap.lookup (fromIntegral number) parts of
  Just (PartTyVar variable) -> pure variable
  _ -> fail "invalid interface type variable reference"

partPred :: IntMap InterfacePart -> Word64 -> Get.Get Pred
partPred parts number = case IntMap.lookup (fromIntegral number) parts of
  Just (PartPred predicate) -> pure predicate
  _ -> fail "invalid interface predicate reference"

partScheme :: IntMap InterfacePart -> Word64 -> Get.Get TypeScheme
partScheme parts number = case IntMap.lookup (fromIntegral number) parts of
  Just (PartScheme scheme) -> pure scheme
  _ -> fail "invalid interface type scheme reference"

putTyConInfo :: PartIndex -> TyConInfo -> Builder.Builder
putTyConInfo table info = cborArray 7 <> cborText (tciName info) <> cborInt (tciArity info) <> putTyCon table (tciTyCon info) <> putTypeScheme table (tciKindScheme info) <> putTyConFlavor (tciFlavor info) <> putMaybe (putTypeSynonymInfo table) (tciTypeSynonym info) <> putMaybe (encodeList cborInt) (tciInjectivity info)

getTyConInfo :: PartTable -> Get.Get TyConInfo
getTyConInfo table = do
  expectArray 7
  tciName <- getText
  tciArity <- getInt
  tciTyCon <- getTyCon table
  tciKindScheme <- getTypeScheme table
  tciFlavor <- getTyConFlavor
  tciTypeSynonym <- getMaybe (getTypeSynonymInfo table)
  tciInjectivity <- getMaybe (getList getInt)
  pure TyConInfo {tciName, tciArity, tciTyCon, tciKindScheme, tciFlavor, tciTypeSynonym, tciInjectivity}

putTypeSynonymInfo :: PartIndex -> TypeSynonymInfo -> Builder.Builder
putTypeSynonymInfo table info = cborArray 2 <> encodeList (putTyVar table) (tsiParams info) <> putMaybe (putType table) (tsiBody info)

getTypeSynonymInfo :: PartTable -> Get.Get TypeSynonymInfo
getTypeSynonymInfo table = expectArray 2 >> (TypeSynonymInfo <$!> getList (getTyVar table) <*!> getMaybe (getType table))

putDataTypeInfo :: PartIndex -> DataTypeInfo -> Builder.Builder
putDataTypeInfo table info = cborArray 8 <> cborText (dtiName info) <> putTyCon table (dtiTyCon info) <> encodeList (putTyVar table) (dtiTyVars info) <> putType table (dtiResultKind info) <> putTyConFlavor (dtiFlavor info) <> encodeList (putDataConInfo table) (dtiConstructors info) <> encodeList putBool (dtiNominalRoles info) <> putMaybe putCType (dtiCType info)

getDataTypeInfo :: PartTable -> Get.Get DataTypeInfo
getDataTypeInfo table = do
  expectArray 8
  dtiName <- getText
  dtiTyCon <- getTyCon table
  dtiTyVars <- getList (getTyVar table)
  dtiResultKind <- getType table
  dtiFlavor <- getTyConFlavor
  dtiConstructors <- getList (getDataConInfo table)
  dtiNominalRoles <- getList getBool
  dtiCType <- getMaybe getCType
  pure DataTypeInfo {dtiName, dtiTyCon, dtiTyVars, dtiResultKind, dtiFlavor, dtiConstructors, dtiNominalRoles, dtiCType}

putDataConInfo :: PartIndex -> DataConInfo -> Builder.Builder
putDataConInfo table info =
  cborArray 8
    <> cborText (dciName info)
    <> putOrigin (dciOrigin info)
    <> encodeList (putTyVar table) (dciUnivTyVars info)
    <> encodeList (putTyVar table) (dciExTyVars info)
    <> encodeList (putPred table) (dciTheta info)
    <> encodeList (putDataConFieldInfo table) (dciFields info)
    <> putType table (dciResTy info)
    <> putDataConSourceForm (dciSourceForm info)

getDataConInfo :: PartTable -> Get.Get DataConInfo
getDataConInfo table = do
  expectArray 8
  dciName <- getText
  dciOrigin <- getOrigin
  dciUnivTyVars <- getList (getTyVar table)
  dciExTyVars <- getList (getTyVar table)
  dciTheta <- getList (getPred table)
  dciFields <- getList (getDataConFieldInfo table)
  dciResTy <- getType table
  dciSourceForm <- getDataConSourceForm
  pure DataConInfo {dciName, dciOrigin, dciUnivTyVars, dciExTyVars, dciTheta, dciFields, dciResTy, dciSourceForm}

putDataConFieldInfo :: PartIndex -> DataConFieldInfo -> Builder.Builder
putDataConFieldInfo table info = cborArray 5 <> putMaybe cborText (dcfiLabel info) <> putType table (dcfiType info) <> putBool (dcfiStrict info) <> putBool (dcfiLazy info) <> putDataConFieldUnpack (dcfiUnpack info)

getDataConFieldInfo :: PartTable -> Get.Get DataConFieldInfo
getDataConFieldInfo table = do
  expectArray 5
  dcfiLabel <- getMaybe getText
  dcfiType <- getType table
  dcfiStrict <- getBool
  dcfiLazy <- getBool
  dcfiUnpack <- getDataConFieldUnpack
  pure DataConFieldInfo {dcfiLabel, dcfiType, dcfiStrict, dcfiLazy, dcfiUnpack}

putClassInfo :: PartIndex -> ClassInfo -> Builder.Builder
putClassInfo table info =
  cborArray 11
    <> cborText (ciName info)
    <> putTyCon table (ciTyCon info)
    <> putMaybe putTextOrigin (ciOrigin info)
    <> encodeList (putTyVar table) (ciKindTyVars info)
    <> encodeList (putTyVar table) (ciTyVars info)
    <> encodeList (putType table) (ciSuperClassTypes info)
    <> encodeList (putNamedScheme table) (ciMethods info)
    <> encodeList cborText (ciDefaultMethods info)
    <> encodeList (putNamedScheme table) (ciDefaultSignatures info)
    <> encodeList (putAssociatedTypeInfo table) (ciAssociatedTypes info)
    <> encodeList putFunDep (ciFunDeps info)

putFunDep :: FunDep -> Builder.Builder
putFunDep dependency =
  cborArray 2
    <> encodeList cborInt (fdDeterminers dependency)
    <> encodeList cborInt (fdDetermined dependency)

getFunDep :: Get.Get FunDep
getFunDep = do
  expectArray 2
  fdDeterminers <- getList getInt
  fdDetermined <- getList getInt
  pure FunDep {fdDeterminers, fdDetermined}

putAssociatedTypeInfo :: PartIndex -> AssociatedTypeInfo -> Builder.Builder
putAssociatedTypeInfo table info =
  cborArray 3
    <> putTyCon table (atiTyCon info)
    <> encodeList (putMaybe cborInt) (atiClassParams info)
    <> putMaybe (putTypeFamilyInstanceInfo table) (atiDefault info)

getAssociatedTypeInfo :: PartTable -> Get.Get AssociatedTypeInfo
getAssociatedTypeInfo table = do
  expectArray 3
  atiTyCon <- getTyCon table
  atiClassParams <- getList (getMaybe getInt)
  atiDefault <- getMaybe (getTypeFamilyInstanceInfo table)
  pure AssociatedTypeInfo {atiTyCon, atiClassParams, atiDefault}

getClassInfo :: PartTable -> Get.Get ClassInfo
getClassInfo table = do
  expectArray 11
  ciName <- getText
  ciTyCon <- getTyCon table
  ciOrigin <- getMaybe getTextOrigin
  ciKindTyVars <- getList (getTyVar table)
  ciTyVars <- getList (getTyVar table)
  ciSuperClassTypes <- getList (getType table)
  ciMethods <- getList (getNamedScheme table)
  ciDefaultMethods <- getList getText
  ciDefaultSignatures <- getList (getNamedScheme table)
  ciAssociatedTypes <- getList (getAssociatedTypeInfo table)
  ciFunDeps <- getList getFunDep
  pure ClassInfo {ciName, ciTyCon, ciOrigin, ciKindTyVars, ciTyVars, ciSuperClassTypes, ciMethods, ciDefaultMethods, ciDefaultSignatures, ciAssociatedTypes, ciFunDeps}

putInstanceInfo :: PartIndex -> InstanceInfo -> Builder.Builder
putInstanceInfo table info =
  cborArray 7
    <> cborText (iiClassName info)
    <> cborText (iiDictName info)
    <> putTextOrigin (iiDictOrigin info)
    <> putType table (iiDictType info)
    <> encodeList (putTyVar table) (iiTyVars info)
    <> encodeList (putPred table) (iiContext info)
    <> encodeList (putType table) (iiHead info)

getInstanceInfo :: PartTable -> Get.Get InstanceInfo
getInstanceInfo table = do
  expectArray 7
  iiClassName <- getText
  iiDictName <- getText
  iiDictOrigin <- getTextOrigin
  iiDictType <- getType table
  iiTyVars <- getList (getTyVar table)
  iiContext <- getList (getPred table)
  iiHead <- getList (getType table)
  pure InstanceInfo {iiClassName, iiDictName, iiDictOrigin, iiDictType, iiTyVars, iiContext, iiHead}

putDataFamilyInstanceInfo :: PartIndex -> DataFamilyInstanceInfo -> Builder.Builder
putDataFamilyInstanceInfo table info =
  cborArray 8
    <> cborText (dfiiFamilyName info)
    <> putType table (dfiiFamilyType info)
    <> encodeList (putTyVar table) (dfiiTyVars info)
    <> putTyCon table (dfiiRepresentationTyCon info)
    <> cborText (dfiiAxiomName info)
    <> encodeList cborText (dfiiConstructorNames info)
    <> encodeList (putDataConInfo table) (dfiiConstructors info)
    <> putBool (dfiiIsNewtype info)

getDataFamilyInstanceInfo :: PartTable -> Get.Get DataFamilyInstanceInfo
getDataFamilyInstanceInfo table = do
  expectArray 8
  dfiiFamilyName <- getText
  dfiiFamilyType <- getType table
  dfiiTyVars <- getList (getTyVar table)
  dfiiRepresentationTyCon <- getTyCon table
  dfiiAxiomName <- getText
  dfiiConstructorNames <- getList getText
  dfiiConstructors <- getList (getDataConInfo table)
  dfiiIsNewtype <- getBool
  pure DataFamilyInstanceInfo {dfiiFamilyName, dfiiFamilyType, dfiiTyVars, dfiiRepresentationTyCon, dfiiAxiomName, dfiiConstructorNames, dfiiConstructors, dfiiIsNewtype}

putTypeFamilyInstanceInfo :: PartIndex -> TypeFamilyInstanceInfo -> Builder.Builder
putTypeFamilyInstanceInfo table info =
  cborArray 7
    <> cborText (tfiiFamilyName info)
    <> cborText (tfiiAxiomName info)
    <> putOrigin (tfiiOrigin info)
    <> encodeList (putTyVar table) (tfiiTyVars info)
    <> putType table (tfiiLeft info)
    <> putType table (tfiiRight info)
    <> putBool (tfiiClosed info)

getTypeFamilyInstanceInfo :: PartTable -> Get.Get TypeFamilyInstanceInfo
getTypeFamilyInstanceInfo table = do
  expectArray 7
  tfiiFamilyName <- getText
  tfiiAxiomName <- getText
  tfiiOrigin <- getOrigin
  tfiiTyVars <- getList (getTyVar table)
  tfiiLeft <- getType table
  tfiiRight <- getType table
  tfiiClosed <- getBool
  pure TypeFamilyInstanceInfo {tfiiFamilyName, tfiiAxiomName, tfiiOrigin, tfiiTyVars, tfiiLeft, tfiiRight, tfiiClosed}

putOrigin :: (PackageId, Text) -> Builder.Builder
putOrigin (packageId, moduleName) = cborArray 2 <> putPackageId packageId <> cborText moduleName

getOrigin :: Get.Get (PackageId, Text)
getOrigin = expectArray 2 >> ((,) <$!> getPackageId <*!> getText)

putTextOrigin :: (Text, Text) -> Builder.Builder
putTextOrigin (packageId, moduleName) = cborArray 2 <> cborText packageId <> cborText moduleName

getTextOrigin :: Get.Get (Text, Text)
getTextOrigin = expectArray 2 >> ((,) <$!> getText <*!> getText)

putNamedScheme :: PartIndex -> (Text, TypeScheme) -> Builder.Builder
putNamedScheme table (name, scheme) = cborArray 2 <> cborText name <> putTypeScheme table scheme

getNamedScheme :: PartTable -> Get.Get (Text, TypeScheme)
getNamedScheme table = expectArray 2 >> ((,) <$!> getText <*!> getTypeScheme table)

encodeList :: (value -> Builder.Builder) -> [value] -> Builder.Builder
encodeList encode values = cborArray (length values) <> foldMap encode values

getList :: Get.Get value -> Get.Get [value]
getList getValue = getArrayLength >>= (`replicateM` getValue)

putMaybe :: (value -> Builder.Builder) -> Maybe value -> Builder.Builder
putMaybe encode value = case value of
  Nothing -> cborArray 1 <> cborWord 0
  Just item -> cborArray 2 <> cborWord 1 <> encode item

getMaybe :: Get.Get value -> Get.Get (Maybe value)
getMaybe getValue = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (1, 0) -> pure Nothing
    (2, 1) -> Just <$!> getValue
    _ -> fail "unsupported optional value"

putBool :: Bool -> Builder.Builder
putBool value = cborWord (if value then 1 else 0)

getBool :: Get.Get Bool
getBool = do
  value <- getWord
  case value of
    0 -> pure False
    1 -> pure True
    _ -> fail "unsupported Boolean value"

putTyConFlavor :: TyConFlavor -> Builder.Builder
putTyConFlavor flavor = cborWord $ case flavor of
  ClassTyCon -> 0
  DataTyCon -> 1
  DataFamilyTyCon -> 2
  NewtypeTyCon -> 3
  SynonymTyCon -> 4
  TypeFamilyTyCon -> 5

getTyConFlavor :: Get.Get TyConFlavor
getTyConFlavor = getTagged "type constructor flavor" [(0, ClassTyCon), (1, DataTyCon), (2, DataFamilyTyCon), (3, NewtypeTyCon), (4, SynonymTyCon), (5, TypeFamilyTyCon)]

putDataConSourceForm :: DataConSourceForm -> Builder.Builder
putDataConSourceForm sourceForm = cborWord $ case sourceForm of
  PrefixDataCon -> 0
  InfixDataCon -> 1
  RecordDataCon -> 2
  SyntaxDataCon -> 3

getDataConSourceForm :: Get.Get DataConSourceForm
getDataConSourceForm = getTagged "constructor source form" [(0, PrefixDataCon), (1, InfixDataCon), (2, RecordDataCon), (3, SyntaxDataCon)]

putDataConFieldUnpack :: DataConFieldUnpack -> Builder.Builder
putDataConFieldUnpack unpack = cborWord $ case unpack of
  NoFieldUnpack -> 0
  UnpackField -> 1
  NoUnpackField -> 2

getDataConFieldUnpack :: Get.Get DataConFieldUnpack
getDataConFieldUnpack = getTagged "field unpack mode" [(0, NoFieldUnpack), (1, UnpackField), (2, NoUnpackField)]

getTagged :: String -> [(Word64, value)] -> Get.Get value
getTagged label values = do
  tag <- getWord
  case lookup tag values of
    Just value -> pure value
    Nothing -> fail ("unsupported " <> label)

sum1 :: Word64 -> Builder.Builder -> Builder.Builder
sum1 tag first = cborArray 2 <> cborWord tag <> first

sum2 :: Word64 -> Builder.Builder -> Builder.Builder -> Builder.Builder
sum2 tag first second = cborArray 3 <> cborWord tag <> first <> second

expectArray :: Int -> Get.Get ()
expectArray expected = do
  actual <- getArrayLength
  unless (actual == expected) (fail "unexpected CBOR array length")

expectText :: Text -> Get.Get ()
expectText expected = do
  actual <- getText
  unless (actual == expected) (fail "unexpected artifact kind")
