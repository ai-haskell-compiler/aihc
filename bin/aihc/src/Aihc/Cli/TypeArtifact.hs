module Aihc.Cli.TypeArtifact
  ( TypeArtifact (..),
    decodeTypeArtifact,
    encodeTypeArtifact,
    encodeTypeInterface,
  )
where

import Aihc.Resolve (PackageId (..))
import Aihc.Tc
  ( ClassInfo (..),
    DataConFieldInfo (..),
    DataConFieldUnpack (..),
    DataConInfo (..),
    DataConSourceForm (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    Kind (..),
    Levity (..),
    Pred (..),
    RuntimeRep (..),
    TcInterface (..),
    TcTermKey (..),
    TcType (..),
    TyCon,
    TyConFlavor (..),
    TyConInfo (..),
    TyVarId (..),
    TypeScheme (..),
    Unique (..),
    VecCount (..),
    VecElem (..),
    tvKind,
    tyConArity,
    tyConKindScheme,
    tyConName,
  )
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Aihc.Tc.Types (mkTyConWithOriginScheme, setTyVarKind, tyConModuleName, tyConPackageId)
import Control.Monad (replicateM, unless)
import Data.Binary.Get qualified as Get
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)

data TypeArtifact = TypeArtifact
  { typeArtifactModuleName :: !Text,
    typeArtifactInputHashes :: ![(Text, Text)],
    typeArtifactInterface :: !TcInterface
  }
  deriving (Show)

encodeTypeArtifact :: TypeArtifact -> BL.ByteString
encodeTypeArtifact artifact =
  Builder.toLazyByteString $
    cborArray 5
      <> cborText "aihc-type"
      <> cborWord 4
      <> cborText (typeArtifactModuleName artifact)
      <> encodeList encodeHash (typeArtifactInputHashes artifact)
      <> putInterface (typeArtifactInterface artifact)
  where
    encodeHash (name, digest) = cborArray 2 <> cborText name <> cborText digest

encodeTypeInterface :: TcInterface -> BL.ByteString
encodeTypeInterface = Builder.toLazyByteString . putInterface

decodeTypeArtifact :: BS.ByteString -> Either String TypeArtifact
decodeTypeArtifact bytes =
  case Get.runGetOrFail getArtifact (BL.fromStrict bytes) of
    Left (_, _, message) -> Left message
    Right (remaining, _, artifact)
      | BL.null remaining -> Right artifact
      | otherwise -> Left "invalid trailing data"

getArtifact :: Get.Get TypeArtifact
getArtifact = do
  expectArray 5
  expectText "aihc-type"
  expectWord 4
  typeArtifactModuleName <- getText
  typeArtifactInputHashes <- getList getHash
  typeArtifactInterface <- getInterface
  pure TypeArtifact {typeArtifactModuleName, typeArtifactInputHashes, typeArtifactInterface}
  where
    getHash = expectArray 2 >> ((,) <$> getText <*> getText)

putInterface :: TcInterface -> Builder.Builder
putInterface interface =
  cborArray 6
    <> encodeList putTerm (tcInterfaceTerms interface)
    <> encodeList putTyConInfo (tcInterfaceTyCons interface)
    <> encodeList putDataTypeInfo (tcInterfaceDataTypes interface)
    <> encodeList putClassInfo (tcInterfaceClasses interface)
    <> encodeList putInstanceInfo (tcInterfaceInstances interface)
    <> encodeList putDataFamilyInstanceInfo (tcInterfaceDataFamilyInstances interface)

getInterface :: Get.Get TcInterface
getInterface = do
  expectArray 6
  tcInterfaceTerms <- getList getTerm
  tcInterfaceTyCons <- getList getTyConInfo
  tcInterfaceDataTypes <- getList getDataTypeInfo
  tcInterfaceClasses <- getList getClassInfo
  tcInterfaceInstances <- getList getInstanceInfo
  tcInterfaceDataFamilyInstances <- getList getDataFamilyInstanceInfo
  pure TcInterface {tcInterfaceTerms, tcInterfaceTyCons, tcInterfaceDataTypes, tcInterfaceClasses, tcInterfaceInstances, tcInterfaceDataFamilyInstances}

putTerm :: (TcTermKey, TypeScheme) -> Builder.Builder
putTerm (key, scheme) = cborArray 2 <> putTermKey key <> putTypeScheme scheme

getTerm :: Get.Get (TcTermKey, TypeScheme)
getTerm = expectArray 2 >> ((,) <$> getTermKey <*> getTypeScheme)

putTermKey :: TcTermKey -> Builder.Builder
putTermKey key = case key of
  TcTermLocal unique -> cborArray 2 <> cborWord 0 <> cborInt unique
  TcTermGlobal (PackageId packageId) moduleName identifier -> cborArray 4 <> cborWord 1 <> cborText packageId <> cborText moduleName <> cborText identifier

getTermKey :: Get.Get TcTermKey
getTermKey = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (2, 0) -> TcTermLocal <$> getInt
    (4, 1) -> (TcTermGlobal . PackageId <$> getText) <*> getText <*> getText
    _ -> fail "unsupported term key"

putTypeScheme :: TypeScheme -> Builder.Builder
putTypeScheme (ForAll variables predicates body) = cborArray 3 <> encodeList putTyVar variables <> encodeList putPred predicates <> putType body

getTypeScheme :: Get.Get TypeScheme
getTypeScheme = expectArray 3 >> (ForAll <$> getList getTyVar <*> getList getPred <*> getType)

putTyVar :: TyVarId -> Builder.Builder
putTyVar variable = cborArray 3 <> cborText (tvName variable) <> putUnique (tvUnique variable) <> putKind (tvKind variable)

getTyVar :: Get.Get TyVarId
getTyVar = do
  expectArray 3
  name <- getText
  unique <- getUnique
  kind <- getKind
  pure (setTyVarKind kind (TyVarId name unique))

putUnique :: Unique -> Builder.Builder
putUnique (Unique value) = cborInt value

getUnique :: Get.Get Unique
getUnique = Unique <$> getInt

putTyCon :: TyCon -> Builder.Builder
putTyCon tyCon =
  cborArray 5
    <> putPackageId (tyConPackageId tyCon)
    <> cborText (tyConModuleName tyCon)
    <> cborText (tyConName tyCon)
    <> cborInt (tyConArity tyCon)
    <> putTypeScheme (tyConKindScheme tyCon)

getTyCon :: Get.Get TyCon
getTyCon = do
  expectArray 5
  mkTyConWithOriginScheme <$> getPackageId <*> getText <*> getText <*> getInt <*> getTypeScheme

putPackageId :: PackageId -> Builder.Builder
putPackageId (PackageId identity) = cborText identity

getPackageId :: Get.Get PackageId
getPackageId = PackageId <$> getText

putKind :: Kind -> Builder.Builder
putKind kind = case kind of
  KTYPE representation -> sum1 0 (putRuntimeRep representation)
  KConstraint -> sum0 1
  KRuntimeRep -> sum0 2
  KLevity -> sum0 3
  KVecCount -> sum0 4
  KVecElem -> sum0 5
  KFun argument result -> sum2 6 (putKind argument) (putKind result)
  KMeta unique -> sum1 7 (putUnique unique)

getKind :: Get.Get Kind
getKind = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (2, 0) -> KTYPE <$> getRuntimeRep
    (1, 1) -> pure KConstraint
    (1, 2) -> pure KRuntimeRep
    (1, 3) -> pure KLevity
    (1, 4) -> pure KVecCount
    (1, 5) -> pure KVecElem
    (3, 6) -> KFun <$> getKind <*> getKind
    (2, 7) -> KMeta <$> getUnique
    _ -> fail "unsupported kind"

putRuntimeRep :: RuntimeRep -> Builder.Builder
putRuntimeRep representation = case representation of
  VecRep count element -> sum2 0 (putVecCount count) (putVecElem element)
  TupleRep fields -> sum1 1 (encodeList putRuntimeRep fields)
  SumRep fields -> sum1 2 (encodeList putRuntimeRep fields)
  BoxedRep levity -> sum1 3 (putLevity levity)
  IntRep -> sum0 4
  Int8Rep -> sum0 5
  Int16Rep -> sum0 6
  Int32Rep -> sum0 7
  Int64Rep -> sum0 8
  WordRep -> sum0 9
  Word8Rep -> sum0 10
  Word16Rep -> sum0 11
  Word32Rep -> sum0 12
  Word64Rep -> sum0 13
  AddrRep -> sum0 14
  FloatRep -> sum0 15
  DoubleRep -> sum0 16
  RuntimeRepVar unique -> sum1 17 (putUnique unique)
  RuntimeRepMeta unique -> sum1 18 (putUnique unique)

getRuntimeRep :: Get.Get RuntimeRep
getRuntimeRep = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (3, 0) -> VecRep <$> getVecCount <*> getVecElem
    (2, 1) -> TupleRep <$> getList getRuntimeRep
    (2, 2) -> SumRep <$> getList getRuntimeRep
    (2, 3) -> BoxedRep <$> getLevity
    (1, 4) -> pure IntRep
    (1, 5) -> pure Int8Rep
    (1, 6) -> pure Int16Rep
    (1, 7) -> pure Int32Rep
    (1, 8) -> pure Int64Rep
    (1, 9) -> pure WordRep
    (1, 10) -> pure Word8Rep
    (1, 11) -> pure Word16Rep
    (1, 12) -> pure Word32Rep
    (1, 13) -> pure Word64Rep
    (1, 14) -> pure AddrRep
    (1, 15) -> pure FloatRep
    (1, 16) -> pure DoubleRep
    (2, 17) -> RuntimeRepVar <$> getUnique
    (2, 18) -> RuntimeRepMeta <$> getUnique
    _ -> fail "unsupported runtime representation"

putType :: TcType -> Builder.Builder
putType ty = case ty of
  TcTyVar variable -> sum1 0 (putTyVar variable)
  TcMetaTv unique -> sum1 1 (putUnique unique)
  TcTyCon tyCon arguments -> sum2 2 (putTyCon tyCon) (encodeList putType arguments)
  TcFunTy argument result -> sum2 3 (putType argument) (putType result)
  TcForAllTy variable body -> sum2 4 (putTyVar variable) (putType body)
  TcQualTy predicates body -> sum2 5 (encodeList putPred predicates) (putType body)
  TcAppTy function argument -> sum2 6 (putType function) (putType argument)
  TcBuiltinTyCon name arity arguments -> cborArray 4 <> cborWord 7 <> cborText name <> cborInt arity <> encodeList putType arguments

getType :: Get.Get TcType
getType = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (2, 0) -> TcTyVar <$> getTyVar
    (2, 1) -> TcMetaTv <$> getUnique
    (3, 2) -> TcTyCon <$> getTyCon <*> getList getType
    (3, 3) -> TcFunTy <$> getType <*> getType
    (3, 4) -> TcForAllTy <$> getTyVar <*> getType
    (3, 5) -> TcQualTy <$> getList getPred <*> getType
    (3, 6) -> TcAppTy <$> getType <*> getType
    (4, 7) -> TcBuiltinTyCon <$> getText <*> getInt <*> getList getType
    _ -> fail "unsupported type"

putPred :: Pred -> Builder.Builder
putPred predicate = case predicate of
  ClassPred tyCon arguments -> sum2 0 (putTyCon tyCon) (encodeList putType arguments)
  EqPred left right -> sum2 1 (putType left) (putType right)

getPred :: Get.Get Pred
getPred = do
  expectArray 3
  tag <- getWord
  case tag of
    0 -> ClassPred <$> getTyCon <*> getList getType
    1 -> EqPred <$> getType <*> getType
    _ -> fail "unsupported predicate"

putTyConInfo :: TyConInfo -> Builder.Builder
putTyConInfo info = cborArray 5 <> cborText (tciName info) <> cborInt (tciArity info) <> putTyCon (tciTyCon info) <> putTyConFlavor (tciFlavor info) <> putMaybe putTypeSynonymInfo (tciTypeSynonym info)

getTyConInfo :: Get.Get TyConInfo
getTyConInfo = do
  expectArray 5
  tciName <- getText
  tciArity <- getInt
  tciTyCon <- getTyCon
  tciFlavor <- getTyConFlavor
  tciTypeSynonym <- getMaybe getTypeSynonymInfo
  pure TyConInfo {tciName, tciArity, tciTyCon, tciFlavor, tciTypeSynonym}

putTypeSynonymInfo :: TypeSynonymInfo -> Builder.Builder
putTypeSynonymInfo info = cborArray 2 <> encodeList putTyVar (tsiParams info) <> putMaybe putType (tsiBody info)

getTypeSynonymInfo :: Get.Get TypeSynonymInfo
getTypeSynonymInfo = expectArray 2 >> (TypeSynonymInfo <$> getList getTyVar <*> getMaybe getType)

putDataTypeInfo :: DataTypeInfo -> Builder.Builder
putDataTypeInfo info = cborArray 6 <> cborText (dtiName info) <> putTyCon (dtiTyCon info) <> encodeList putTyVar (dtiTyVars info) <> putKind (dtiResultKind info) <> putTyConFlavor (dtiFlavor info) <> encodeList putDataConInfo (dtiConstructors info)

getDataTypeInfo :: Get.Get DataTypeInfo
getDataTypeInfo = do
  expectArray 6
  dtiName <- getText
  dtiTyCon <- getTyCon
  dtiTyVars <- getList getTyVar
  dtiResultKind <- getKind
  dtiFlavor <- getTyConFlavor
  dtiConstructors <- getList getDataConInfo
  pure DataTypeInfo {dtiName, dtiTyCon, dtiTyVars, dtiResultKind, dtiFlavor, dtiConstructors}

putDataConInfo :: DataConInfo -> Builder.Builder
putDataConInfo info =
  cborArray 8
    <> cborText (dciName info)
    <> putOrigin (dciOrigin info)
    <> encodeList putTyVar (dciUnivTyVars info)
    <> encodeList putTyVar (dciExTyVars info)
    <> encodeList putPred (dciTheta info)
    <> encodeList putDataConFieldInfo (dciFields info)
    <> putType (dciResTy info)
    <> putDataConSourceForm (dciSourceForm info)

getDataConInfo :: Get.Get DataConInfo
getDataConInfo = do
  expectArray 8
  dciName <- getText
  dciOrigin <- getOrigin
  dciUnivTyVars <- getList getTyVar
  dciExTyVars <- getList getTyVar
  dciTheta <- getList getPred
  dciFields <- getList getDataConFieldInfo
  dciResTy <- getType
  dciSourceForm <- getDataConSourceForm
  pure DataConInfo {dciName, dciOrigin, dciUnivTyVars, dciExTyVars, dciTheta, dciFields, dciResTy, dciSourceForm}

putDataConFieldInfo :: DataConFieldInfo -> Builder.Builder
putDataConFieldInfo info = cborArray 5 <> putMaybe cborText (dcfiLabel info) <> putType (dcfiType info) <> putBool (dcfiStrict info) <> putBool (dcfiLazy info) <> putDataConFieldUnpack (dcfiUnpack info)

getDataConFieldInfo :: Get.Get DataConFieldInfo
getDataConFieldInfo = do
  expectArray 5
  dcfiLabel <- getMaybe getText
  dcfiType <- getType
  dcfiStrict <- getBool
  dcfiLazy <- getBool
  dcfiUnpack <- getDataConFieldUnpack
  pure DataConFieldInfo {dcfiLabel, dcfiType, dcfiStrict, dcfiLazy, dcfiUnpack}

putClassInfo :: ClassInfo -> Builder.Builder
putClassInfo info =
  cborArray 8
    <> cborText (ciName info)
    <> putTyCon (ciTyCon info)
    <> putMaybe putTextOrigin (ciOrigin info)
    <> encodeList putTyVar (ciTyVars info)
    <> encodeList putType (ciSuperClassTypes info)
    <> encodeList putNamedScheme (ciMethods info)
    <> encodeList cborText (ciDefaultMethods info)
    <> encodeList putNamedScheme (ciDefaultSignatures info)

getClassInfo :: Get.Get ClassInfo
getClassInfo = do
  expectArray 8
  ciName <- getText
  ciTyCon <- getTyCon
  ciOrigin <- getMaybe getTextOrigin
  ciTyVars <- getList getTyVar
  ciSuperClassTypes <- getList getType
  ciMethods <- getList getNamedScheme
  ciDefaultMethods <- getList getText
  ciDefaultSignatures <- getList getNamedScheme
  pure ClassInfo {ciName, ciTyCon, ciOrigin, ciTyVars, ciSuperClassTypes, ciMethods, ciDefaultMethods, ciDefaultSignatures}

putInstanceInfo :: InstanceInfo -> Builder.Builder
putInstanceInfo info =
  cborArray 7
    <> cborText (iiClassName info)
    <> cborText (iiDictName info)
    <> putMaybe putTextOrigin (iiDictOrigin info)
    <> putType (iiDictType info)
    <> encodeList putTyVar (iiTyVars info)
    <> encodeList putPred (iiContext info)
    <> encodeList putType (iiHead info)

getInstanceInfo :: Get.Get InstanceInfo
getInstanceInfo = do
  expectArray 7
  iiClassName <- getText
  iiDictName <- getText
  iiDictOrigin <- getMaybe getTextOrigin
  iiDictType <- getType
  iiTyVars <- getList getTyVar
  iiContext <- getList getPred
  iiHead <- getList getType
  pure InstanceInfo {iiClassName, iiDictName, iiDictOrigin, iiDictType, iiTyVars, iiContext, iiHead}

putDataFamilyInstanceInfo :: DataFamilyInstanceInfo -> Builder.Builder
putDataFamilyInstanceInfo info =
  cborArray 7
    <> cborText (dfiiFamilyName info)
    <> putType (dfiiFamilyType info)
    <> encodeList putTyVar (dfiiTyVars info)
    <> putTyCon (dfiiRepresentationTyCon info)
    <> cborText (dfiiAxiomName info)
    <> encodeList cborText (dfiiConstructorNames info)
    <> putBool (dfiiIsNewtype info)

getDataFamilyInstanceInfo :: Get.Get DataFamilyInstanceInfo
getDataFamilyInstanceInfo = do
  expectArray 7
  dfiiFamilyName <- getText
  dfiiFamilyType <- getType
  dfiiTyVars <- getList getTyVar
  dfiiRepresentationTyCon <- getTyCon
  dfiiAxiomName <- getText
  dfiiConstructorNames <- getList getText
  dfiiIsNewtype <- getBool
  pure DataFamilyInstanceInfo {dfiiFamilyName, dfiiFamilyType, dfiiTyVars, dfiiRepresentationTyCon, dfiiAxiomName, dfiiConstructorNames, dfiiIsNewtype}

putOrigin :: (PackageId, Text) -> Builder.Builder
putOrigin (packageId, moduleName) = cborArray 2 <> putPackageId packageId <> cborText moduleName

getOrigin :: Get.Get (PackageId, Text)
getOrigin = expectArray 2 >> ((,) <$> getPackageId <*> getText)

putTextOrigin :: (Text, Text) -> Builder.Builder
putTextOrigin (packageId, moduleName) = cborArray 2 <> cborText packageId <> cborText moduleName

getTextOrigin :: Get.Get (Text, Text)
getTextOrigin = expectArray 2 >> ((,) <$> getText <*> getText)

putNamedScheme :: (Text, TypeScheme) -> Builder.Builder
putNamedScheme (name, scheme) = cborArray 2 <> cborText name <> putTypeScheme scheme

getNamedScheme :: Get.Get (Text, TypeScheme)
getNamedScheme = expectArray 2 >> ((,) <$> getText <*> getTypeScheme)

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
    (2, 1) -> Just <$> getValue
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

getTyConFlavor :: Get.Get TyConFlavor
getTyConFlavor = getTagged "type constructor flavor" [(0, ClassTyCon), (1, DataTyCon), (2, DataFamilyTyCon), (3, NewtypeTyCon), (4, SynonymTyCon)]

putDataConSourceForm :: DataConSourceForm -> Builder.Builder
putDataConSourceForm sourceForm = cborWord $ case sourceForm of
  PrefixDataCon -> 0
  InfixDataCon -> 1
  RecordDataCon -> 2

getDataConSourceForm :: Get.Get DataConSourceForm
getDataConSourceForm = getTagged "constructor source form" [(0, PrefixDataCon), (1, InfixDataCon), (2, RecordDataCon)]

putDataConFieldUnpack :: DataConFieldUnpack -> Builder.Builder
putDataConFieldUnpack unpack = cborWord $ case unpack of
  NoFieldUnpack -> 0
  UnpackField -> 1
  NoUnpackField -> 2

getDataConFieldUnpack :: Get.Get DataConFieldUnpack
getDataConFieldUnpack = getTagged "field unpack mode" [(0, NoFieldUnpack), (1, UnpackField), (2, NoUnpackField)]

putLevity :: Levity -> Builder.Builder
putLevity levity = cborWord $ case levity of
  Lifted -> 0
  Unlifted -> 1

getLevity :: Get.Get Levity
getLevity = getTagged "levity" [(0, Lifted), (1, Unlifted)]

putVecCount :: VecCount -> Builder.Builder
putVecCount count = cborWord $ case count of
  Vec2 -> 0
  Vec4 -> 1
  Vec8 -> 2
  Vec16 -> 3
  Vec32 -> 4
  Vec64 -> 5

getVecCount :: Get.Get VecCount
getVecCount = getTagged "vector count" [(0, Vec2), (1, Vec4), (2, Vec8), (3, Vec16), (4, Vec32), (5, Vec64)]

putVecElem :: VecElem -> Builder.Builder
putVecElem element = cborWord $ case element of
  Int8ElemRep -> 0
  Int16ElemRep -> 1
  Int32ElemRep -> 2
  Int64ElemRep -> 3
  Word8ElemRep -> 4
  Word16ElemRep -> 5
  Word32ElemRep -> 6
  Word64ElemRep -> 7
  FloatElemRep -> 8
  DoubleElemRep -> 9

getVecElem :: Get.Get VecElem
getVecElem =
  getTagged
    "vector element"
    [ (0, Int8ElemRep),
      (1, Int16ElemRep),
      (2, Int32ElemRep),
      (3, Int64ElemRep),
      (4, Word8ElemRep),
      (5, Word16ElemRep),
      (6, Word32ElemRep),
      (7, Word64ElemRep),
      (8, FloatElemRep),
      (9, DoubleElemRep)
    ]

getTagged :: String -> [(Word64, value)] -> Get.Get value
getTagged label values = do
  tag <- getWord
  case lookup tag values of
    Just value -> pure value
    Nothing -> fail ("unsupported " <> label)

sum0 :: Word64 -> Builder.Builder
sum0 tag = cborArray 1 <> cborWord tag

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

expectWord :: Word64 -> Get.Get ()
expectWord expected = do
  actual <- getWord
  unless (actual == expected) (fail "unsupported schema version")

cborArray :: Int -> Builder.Builder
cborArray = cborMajor 4 . fromIntegral

cborText :: Text -> Builder.Builder
cborText value = cborMajor 3 (fromIntegral (BS.length bytes)) <> Builder.byteString bytes
  where
    bytes = TE.encodeUtf8 value

cborInt :: Int -> Builder.Builder
cborInt value
  | value >= 0 = cborMajor 0 (fromIntegral value)
  | otherwise = cborMajor 1 (fromIntegral (-1 - value))

cborWord :: Word64 -> Builder.Builder
cborWord = cborMajor 0

cborMajor :: Word8 -> Word64 -> Builder.Builder
cborMajor major value
  | value < 24 = Builder.word8 (major * 32 + fromIntegral value)
  | value <= 255 = Builder.word8 (major * 32 + 24) <> Builder.word8 (fromIntegral value)
  | value <= 65535 = Builder.word8 (major * 32 + 25) <> Builder.word16BE (fromIntegral value)
  | value <= 4294967295 = Builder.word8 (major * 32 + 26) <> Builder.word32BE (fromIntegral value)
  | otherwise = Builder.word8 (major * 32 + 27) <> Builder.word64BE value

getArrayLength :: Get.Get Int
getArrayLength = fromIntegral <$> getMajor 4

getText :: Get.Get Text
getText = do
  length' <- getMajor 3
  TE.decodeUtf8 <$> Get.getByteString (fromIntegral length')

getInt :: Get.Get Int
getInt = do
  initial <- Get.lookAhead Get.getWord8
  let major = initial `shiftR` 5
  value <- getMajor major
  case major of
    0 -> pure (fromIntegral value)
    1 -> pure (-1 - fromIntegral value)
    _ -> fail "unexpected CBOR integer"

getWord :: Get.Get Word64
getWord = getMajor 0

getMajor :: Word8 -> Get.Get Word64
getMajor expected = do
  initial <- Get.getWord8
  let major = initial `shiftR` 5
      info = initial `mod` 32
  unless (major == expected) (fail "unexpected CBOR major type")
  case info of
    value | value < 24 -> pure (fromIntegral value)
    24 -> fromIntegral <$> Get.getWord8
    25 -> fromIntegral <$> Get.getWord16be
    26 -> fromIntegral <$> Get.getWord32be
    27 -> Get.getWord64be
    _ -> fail "unsupported CBOR length"
