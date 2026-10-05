-- | The binary format of System FC programs.
--
-- The compiler writes a System FC program to a @core@ file in this format,
-- and reads it back for a whole-program build. The text format of
-- "Aihc.Fc.Pretty" is for people: @aihc-dev fc-print@ shows a @core@ file
-- in it.
--
-- A file holds a header and four sections, as a sequence of CBOR items:
--
-- 1. The texts. Each distinct text is in the file one time.
-- 2. The names. Each distinct name is in the file one time, and refers to
--    its texts by their indices.
-- 3. The types. Each distinct type is in the file one time, after the
--    types that it contains, and refers to them by their indices.
-- 4. The program: its scopes, its imports, and its declarations. An
--    expression or a coercion is a tree that refers to names and types by
--    their indices.
--
-- Thus the decoder makes each distinct name and type one object, and a
-- decoded program is fully evaluated.
module Aihc.Fc.Binary
  ( encodeProgram,
    decodeProgram,
    readProgramFile,
    writeProgramFile,
  )
where

import Aihc.Cbor (cborBytes, cborInt, cborInteger, cborText, cborWord, getBytes, getInt, getInteger, getText, getWord)
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.Monad (unless, (<$!>))
import Control.Monad.Trans.State.Strict (State, gets, modify', runState)
import Data.Array (Array, bounds, listArray, (!))
import Data.Binary.Get qualified as Get
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, ord)
import Data.IntMap.Strict qualified as IntMap
import Data.Ix (inRange)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | The text at the start of each file.
formatMagic :: Text
formatMagic = "aihc-system-fc"

-- | The version of the format. Change it when the layout changes.
formatVersion :: Int
formatVersion = 1

-- | Write a program to a file in the binary format.
writeProgramFile :: FilePath -> Program -> IO ()
writeProgramFile path program = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encodeProgram program)

-- | Read a program from a file in the binary format. An error names the
-- file.
readProgramFile :: FilePath -> IO (Either Text Program)
readProgramFile path = do
  bytes <- BS.readFile path
  pure $ case decodeProgram bytes of
    Left message -> Left (T.pack path <> ": " <> message)
    Right program -> Right program

-- * Encoding

-- | A type with each part replaced by its index in the tables.
data TypeKey
  = KeyVar !Int
  | KeyCon !Int
  | KeyApp !Int !Int
  | KeyFun !Int !Int !Int !Int
  | KeyForAll !Int !Int !Int
  | KeyEq !Int !Int
  | KeyLit !Int !TyLit
  deriving (Eq, Ord)

data Tables = Tables
  { tableTexts :: !(Map Text Int),
    tableTextCount :: !Int,
    tableTextItems :: !Builder.Builder,
    tableNames :: !(Map Name Int),
    tableNameCount :: !Int,
    tableNameItems :: !Builder.Builder,
    tableTypes :: !(Map TypeKey Int),
    tableTypeCount :: !Int,
    tableTypeItems :: !Builder.Builder
  }

type Encode = State Tables

-- | The bytes of a program.
encodeProgram :: Program -> BL.ByteString
encodeProgram program =
  Builder.toLazyByteString $
    cborText formatMagic
      <> cborInt formatVersion
      <> cborInt (tableTextCount tables)
      <> tableTextItems tables
      <> cborInt (tableNameCount tables)
      <> tableNameItems tables
      <> cborInt (tableTypeCount tables)
      <> tableTypeItems tables
      <> body
  where
    (body, tables) = runState (encodeBody program) emptyTables
    emptyTables = Tables Map.empty 0 mempty Map.empty 0 mempty Map.empty 0 mempty

encodeBody :: Program -> Encode Builder.Builder
encodeBody program = do
  scopes <- encodeList encodeScope (scopeEntries (programScopes program))
  imports <- encodeImports (programImports program)
  decls <- encodeList encodeDecl (programDecls program)
  pure (scopes <> imports <> decls)
  where
    encodeScope (scopeId, PackageId package, moduleName) = do
      packageIndex <- textIndex package
      moduleIndex <- textIndex moduleName
      pure (cborInt scopeId <> cborInt packageIndex <> cborInt moduleIndex)

encodeList :: (value -> Encode Builder.Builder) -> [value] -> Encode Builder.Builder
encodeList encode values = do
  items <- mapM encode values
  pure (cborInt (length values) <> mconcat items)

textIndex :: Text -> Encode Int
textIndex text = do
  known <- gets (Map.lookup text . tableTexts)
  case known of
    Just index -> pure index
    Nothing -> do
      index <- gets tableTextCount
      modify' $ \tables ->
        tables
          { tableTexts = Map.insert text index (tableTexts tables),
            tableTextCount = index + 1,
            tableTextItems = tableTextItems tables <> cborText text
          }
      pure index

nameIndex :: Name -> Encode Int
nameIndex name = do
  known <- gets (Map.lookup name . tableNames)
  case known of
    Just index -> pure index
    Nothing -> do
      text <- textIndex (nameText name)
      origin <-
        case nameOrigin name of
          OriginLocal (Unique unique) -> pure (cborWord 0 <> cborInt unique)
          OriginTop (PackageId package) moduleName -> do
            packageIndex <- textIndex package
            moduleIndex <- textIndex moduleName
            pure (cborWord 1 <> cborInt packageIndex <> cborInt moduleIndex)
      index <- gets tableNameCount
      modify' $ \tables ->
        tables
          { tableNames = Map.insert name index (tableNames tables),
            tableNameCount = index + 1,
            tableNameItems = tableNameItems tables <> cborInt text <> cborWord (sortTag (nameSort name)) <> origin
          }
      pure index

-- | The index of a type. The parts of the type get their indices first, so
-- a type is in the table after its parts.
typeIndex :: Type -> Encode Int
typeIndex ty = do
  key <-
    case ty of
      TyVar name -> KeyVar <$> nameIndex name
      TyCon name -> KeyCon <$> nameIndex name
      TyApp function argument -> KeyApp <$> typeIndex function <*> typeIndex argument
      TyFun r1 r2 argument result -> KeyFun <$> typeIndex r1 <*> typeIndex r2 <*> typeIndex argument <*> typeIndex result
      TyForAll binder body -> KeyForAll <$> nameIndex (binderName binder) <*> typeIndex (binderType binder) <*> typeIndex body
      TyEq left right -> KeyEq <$> typeIndex left <*> typeIndex right
      TyLit kindName literal -> (`KeyLit` literal) <$> nameIndex kindName
  known <- gets (Map.lookup key . tableTypes)
  case known of
    Just index -> pure index
    Nothing -> do
      item <- encodeTypeKey key
      index <- gets tableTypeCount
      modify' $ \tables ->
        tables
          { tableTypes = Map.insert key index (tableTypes tables),
            tableTypeCount = index + 1,
            tableTypeItems = tableTypeItems tables <> item
          }
      pure index

encodeTypeKey :: TypeKey -> Encode Builder.Builder
encodeTypeKey key =
  case key of
    KeyVar name -> pure (cborWord 0 <> cborInt name)
    KeyCon name -> pure (cborWord 1 <> cborInt name)
    KeyApp function argument -> pure (cborWord 2 <> cborInt function <> cborInt argument)
    KeyFun r1 r2 argument result -> pure (cborWord 3 <> cborInt r1 <> cborInt r2 <> cborInt argument <> cborInt result)
    KeyForAll name kind body -> pure (cborWord 4 <> cborInt name <> cborInt kind <> cborInt body)
    KeyEq left right -> pure (cborWord 5 <> cborInt left <> cborInt right)
    KeyLit kindName literal -> do
      payload <-
        case literal of
          TyLitNat value -> pure (cborWord 0 <> cborInteger value)
          TyLitSymbol text -> (\index -> cborWord 1 <> cborInt index) <$> textIndex text
          TyLitChar character -> pure (cborWord 2 <> cborWord (fromIntegral (ord character)))
      pure (cborWord 6 <> cborInt kindName <> payload)

encodeName :: Name -> Encode Builder.Builder
encodeName name = cborInt <$> nameIndex name

encodeType :: Type -> Encode Builder.Builder
encodeType ty = cborInt <$> typeIndex ty

encodeBinder :: Binder -> Encode Builder.Builder
encodeBinder binder = (<>) <$> encodeName (binderName binder) <*> encodeType (binderType binder)

encodeImports :: Imports -> Encode Builder.Builder
encodeImports imports =
  mconcat
    <$> sequence
      [ encodeMap encodeType (importHeaders imports),
        encodeMap encodeType (importSynonyms imports),
        encodeMap encodeAxiomDecl (importAxioms imports),
        encodeMap encodeType (importBinders imports),
        encodeMap (pure . encodeConRepresentation) (importConRepresentations imports),
        encodeMap (pure . encodeInts) (importConStrictFields imports),
        encodeMap (encodeList encodeName) (importDataCons imports)
      ]

-- | A map keyed by names, in the order of its keys. The decoder rebuilds
-- the map from that order.
encodeMap :: (value -> Encode Builder.Builder) -> Map Name value -> Encode Builder.Builder
encodeMap encode = encodeList (\(name, value) -> (<>) <$> encodeName name <*> encode value) . Map.toAscList

encodeInts :: [Int] -> Builder.Builder
encodeInts values = cborInt (length values) <> foldMap cborInt values

encodeDecl :: Decl -> Encode Builder.Builder
encodeDecl decl =
  case decl of
    DeclType declaration ->
      mconcat
        <$> sequence
          [ pure (cborWord 0 <> cborWord (visTag (typeVis declaration))),
            encodeName (typeName declaration),
            encodeList encodeBinder (typeBinders declaration),
            encodeType (typeResult declaration),
            pure (cborInt (length (typeRoles declaration)) <> foldMap (cborWord . roleTag) (typeRoles declaration)),
            encodeList encodeConDecl (typeCons declaration)
          ]
    DeclSynonym declaration ->
      mconcat
        <$> sequence
          [ pure (cborWord 1 <> cborWord (visTag (synVis declaration))),
            encodeName (synName declaration),
            encodeList encodeBinder (synBinders declaration),
            encodeType (synResult declaration),
            encodeType (synBody declaration)
          ]
    DeclAxiom declaration -> (cborWord 2 <>) <$> encodeAxiomDecl declaration
    DeclVal declaration ->
      mconcat
        <$> sequence
          [ pure (cborWord 3 <> cborWord (visTag (valVis declaration))),
            encodeName (valName declaration),
            encodeType (valType declaration),
            encodeExpr (valBody declaration),
            pure (encodeInlineSpec (valInline declaration))
          ]
    DeclRule declaration ->
      mconcat
        <$> sequence
          [ (\index -> cborWord 4 <> cborInt index) <$> textIndex (ruleName declaration),
            pure (encodeActivation (ruleActivation declaration)),
            encodeList encodeBinder (ruleTypeBinders declaration),
            encodeList encodeBinder (ruleBinders declaration),
            encodeType (ruleType declaration),
            encodeExpr (ruleLhs declaration),
            encodeExpr (ruleRhs declaration)
          ]

encodeConDecl :: ConDecl -> Encode Builder.Builder
encodeConDecl declaration =
  mconcat
    <$> sequence
      [ pure (cborWord (visTag (conVis declaration))),
        encodeName (conName declaration),
        encodeType (conType declaration),
        pure (encodeConRepresentation (conRepresentation declaration)),
        pure (encodeInts (conStrictFields declaration))
      ]

encodeAxiomDecl :: AxiomDecl -> Encode Builder.Builder
encodeAxiomDecl declaration =
  mconcat
    <$> sequence
      [ pure (cborWord (visTag (axiomVis declaration))),
        encodeName (axiomName declaration),
        encodeList encodeBinder (axiomBinders declaration),
        pure (cborWord (roleTag (axiomRole declaration))),
        encodeType (axiomLeft declaration),
        encodeType (axiomRight declaration)
      ]

encodeConRepresentation :: ConRepresentation -> Builder.Builder
encodeConRepresentation representation =
  case representation of
    HeapConstructor -> cborWord 0
    UnboxedTupleConstructor -> cborWord 1
    UnboxedSumConstructor alternative arity -> cborWord 2 <> cborInt alternative <> cborInt arity

encodeActivation :: RuleActivation -> Builder.Builder
encodeActivation activation =
  case activation of
    AlwaysActive -> cborWord 0
    ActiveAfter phase -> cborWord 1 <> cborInt phase
    ActiveBefore phase -> cborWord 2 <> cborInt phase
    NeverActive -> cborWord 3

encodeInlineSpec :: InlineSpec -> Builder.Builder
encodeInlineSpec spec =
  case spec of
    InlineDefault -> cborWord 0
    InlineAlways activation -> cborWord 1 <> encodeActivation activation
    InlineWhenUseful activation -> cborWord 2 <> encodeActivation activation
    InlineNever activation -> cborWord 3 <> encodeActivation activation

encodeExpr :: Expr -> Encode Builder.Builder
encodeExpr expr =
  case expr of
    ExVar name -> (cborWord 0 <>) <$> encodeName name
    ExLit literal -> (cborWord 1 <>) <$> encodeLiteral literal
    ExApp function argument -> tagged 2 [encodeExpr function, encodeExpr argument]
    ExTyApp function argument -> tagged 3 [encodeExpr function, encodeType argument]
    ExLam binder body -> tagged 4 [encodeBinder binder, encodeExpr body]
    ExTyLam binder body -> tagged 5 [encodeBinder binder, encodeExpr body]
    ExLet bind body -> tagged 6 [encodeBind bind, encodeExpr body]
    ExRec binds body -> tagged 7 [encodeList encodeBind binds, encodeExpr body]
    ExCase scrutinee (Just binder) resultType alternatives ->
      tagged 8 [encodeExpr scrutinee, encodeBinder binder, encodeType resultType, encodeList encodeAlt alternatives]
    ExCase scrutinee Nothing resultType alternatives ->
      tagged 12 [encodeExpr scrutinee, encodeType resultType, encodeList encodeAlt alternatives]
    ExCast body coercion -> tagged 9 [encodeExpr body, encodeCoercion coercion]
    ExCoercion coercion -> tagged 10 [encodeCoercion coercion]
    ExForeignCall call types arguments ->
      tagged 11 [encodeForeignCall call, encodeList encodeType types, encodeList encodeExpr arguments]

tagged :: Int -> [Encode Builder.Builder] -> Encode Builder.Builder
tagged tag fields = (cborInt tag <>) . mconcat <$> sequence fields

encodeBind :: Bind -> Encode Builder.Builder
encodeBind bind = (<>) <$> encodeBinder (bindBinder bind) <*> encodeExpr (bindRhs bind)

encodeAlt :: Alt -> Encode Builder.Builder
encodeAlt alternative =
  mconcat
    <$> sequence
      [ encodeAltCon (altCon alternative),
        encodeList encodeBinder (altTypeBinders alternative),
        encodeList encodeBinder (altBinders alternative),
        encodeExpr (altRhs alternative)
      ]

encodeAltCon :: AltCon -> Encode Builder.Builder
encodeAltCon con =
  case con of
    AltData name -> (cborWord 0 <>) <$> encodeName name
    AltLit literal -> (cborWord 1 <>) <$> encodeLiteral literal
    AltDefault -> pure (cborWord 2)

encodeLiteral :: Literal -> Encode Builder.Builder
encodeLiteral literal =
  case literal of
    LitInt representation value -> (\ty -> cborWord 0 <> ty <> cborInteger value) <$> encodeType representation
    LitChar representation value -> (\ty -> cborWord 1 <> ty <> cborWord (fromIntegral (ord value))) <$> encodeType representation
    LitAddr representation value -> (\ty -> cborWord 2 <> ty <> cborBytes value) <$> encodeType representation

encodeCoercion :: Coercion -> Encode Builder.Builder
encodeCoercion coercion =
  case coercion of
    CoVar name -> tagged 0 [encodeName name]
    CoRefl ty -> tagged 1 [encodeType ty]
    CoSym inner -> tagged 2 [encodeCoercion inner]
    CoTrans left right -> tagged 3 [encodeCoercion left, encodeCoercion right]
    CoApp left right -> tagged 4 [encodeCoercion left, encodeCoercion right]
    CoFun left right -> tagged 5 [encodeCoercion left, encodeCoercion right]
    CoForAll binder body -> tagged 6 [encodeBinder binder, encodeCoercion body]
    CoNth index inner -> tagged 7 [pure (cborInt index), encodeCoercion inner]
    CoTyConApp name arguments -> tagged 8 [encodeName name, encodeList encodeCoercion arguments]
    CoAxiom name types -> tagged 9 [encodeName name, encodeList encodeType types]

encodeForeignCall :: ForeignCall -> Encode Builder.Builder
encodeForeignCall call =
  mconcat
    <$> sequence
      [ encodeName (foreignCallName call),
        encodeConvention (foreignCallConvention call),
        encodeList encodeDependency (foreignCallDependencies call),
        encodeType (foreignCallType call)
      ]
  where
    encodeDependency dependency =
      case dependency of
        ForeignAxiom name -> (cborWord 0 <>) <$> encodeName name
        ForeignConstructor name -> (cborWord 1 <>) <$> encodeName name

encodeConvention :: CallingConvention -> Encode Builder.Builder
encodeConvention convention =
  case convention of
    Prim -> pure (cborWord 0)
    CCall spec -> do
      symbol <- textIndex (ccallSymbol spec)
      pure $
        cborWord 1
          <> cborInt symbol
          <> cborWord (callTargetTag (ccallTarget spec))
          <> cborWord (safetyTag (ccallSafety spec))
          <> cborInt (length (ccallArgumentTypes spec))
          <> foldMap (cborInt . fromEnum) (ccallArgumentTypes spec)
          <> cborInt (fromEnum (ccallResultType spec))
          <> cborWord (effectTag (ccallEffect spec))

-- * Tags of the enumerations

sortTag :: Sort -> Word64
sortTag sort =
  case sort of
    SortTypeConstructor -> 0
    SortDataConstructor -> 1
    SortValue -> 2
    SortTypeVariable -> 3
    SortAxiom -> 4
    SortSynonym -> 5

visTag :: Vis -> Word64
visTag vis =
  case vis of
    Pub -> 0
    Private -> 1

roleTag :: Role -> Word64
roleTag role =
  case role of
    Nominal -> 0
    Representational -> 1
    Phantom -> 2

callTargetTag :: CCallTarget -> Word64
callTargetTag target =
  case target of
    CCallFunction -> 0
    CCallAddress -> 1
    CCallDynamic -> 2
    CCallWrapper -> 3

safetyTag :: ForeignSafety -> Word64
safetyTag safety =
  case safety of
    ForeignUnsafe -> 0
    ForeignSafe -> 1
    ForeignInterruptible -> 2

effectTag :: ForeignEffect -> Word64
effectTag effect =
  case effect of
    ForeignPure -> 0
    ForeignRealWorld -> 1

-- * Decoding

-- | The tables that the program refers to by index.
data Decoded = Decoded
  { decodedTexts :: !(Array Int Text),
    decodedNames :: !(Array Int Name),
    decodedTypes :: !(Array Int Type)
  }

type Decode = Get.Get

-- | The program in the bytes of a @core@ file.
decodeProgram :: BS.ByteString -> Either Text Program
decodeProgram bytes =
  case Get.runGetOrFail getProgram (BL.fromStrict bytes) of
    Left (_, offset, message) -> Left (T.pack ("invalid System FC at byte " <> show offset <> ": " <> message))
    Right (rest, offset, program)
      | BL.null rest -> Right program
      | otherwise -> Left (T.pack ("invalid System FC: unexpected bytes after byte " <> show offset))

getProgram :: Decode Program
getProgram = do
  magic <- getText
  unless (magic == formatMagic) (fail "not a System FC file")
  version <- getInt
  unless (version == formatVersion) (fail ("System FC format version " <> show version <> ", expected " <> show formatVersion))
  texts <- getArray getText
  names <- getArray (getNameEntry texts)
  types <- getTypes texts names
  let tables = Decoded texts names types
  scopes <- List.foldl' (\table (scopeId, package, moduleName) -> insertScope scopeId (PackageId package) moduleName table) emptyScopeTable <$!> getList (getScope tables)
  imports <- getImports tables
  decls <- getList (getDecl tables)
  pure $! Program scopes imports decls
  where
    getScope tables = do
      scopeId <- getInt
      package <- getTextIndex tables
      moduleName <- getTextIndex tables
      pure (scopeId, package, moduleName)

-- | A list. Each element is evaluated before the next one is read.
getList :: Decode value -> Decode [value]
getList getValue = do
  count <- getCount
  let go remaining accumulated
        | remaining <= 0 = pure $! reverse accumulated
        | otherwise = do
            !value <- getValue
            go (remaining - 1 :: Int) (value : accumulated)
  go count []

getCount :: Decode Int
getCount = do
  count <- getInt
  unless (count >= 0) (fail "negative count")
  pure count

getArray :: Decode value -> Decode (Array Int value)
getArray getValue = do
  values <- getList getValue
  pure $! listArray (0, length values - 1) values

-- | An index into a table, and the entry that it names.
getEntry :: String -> Array Int value -> Decode value
getEntry what table = do
  index <- getInt
  unless (inRange (bounds table) index) (fail ("unknown " <> what <> " " <> show index))
  pure $! table ! index

getTextIndex :: Decoded -> Decode Text
getTextIndex tables = getEntry "text" (decodedTexts tables)

getNameEntry :: Array Int Text -> Decode Name
getNameEntry texts = do
  text <- getEntry "text" texts
  sort <- getSort
  originTag <- getWord
  origin <-
    case originTag of
      0 -> OriginLocal . Unique <$!> getInt
      1 -> do
        package <- getEntry "text" texts
        moduleName <- getEntry "text" texts
        pure $! OriginTop (PackageId package) moduleName
      _ -> fail "unknown name origin"
  pure $! Name text sort origin

-- | The type table. A type refers only to the types before it, so each
-- entry is built from entries that are already built.
getTypes :: Array Int Text -> Array Int Name -> Decode (Array Int Type)
getTypes texts names = do
  count <- getCount
  let go index built
        | index >= count = pure built
        | otherwise = do
            !ty <- getTypeEntry built
            go (index + 1) (IntMap.insert index ty built)
  built <- go 0 IntMap.empty
  pure $! listArray (0, count - 1) (IntMap.elems built)
  where
    getTypeEntry built = do
      let child = do
            index <- getInt
            case IntMap.lookup index built of
              Just ty -> pure ty
              Nothing -> fail ("type " <> show index <> " before its definition")
          name = getEntry "name" names
      tag <- getWord
      case tag of
        0 -> TyVar <$!> name
        1 -> TyCon <$!> name
        2 -> TyApp <$!> child <*!> child
        3 -> TyFun <$!> child <*!> child <*!> child <*!> child
        4 -> do
          binder <- Binder <$!> name <*!> child
          TyForAll binder <$!> child
        5 -> TyEq <$!> child <*!> child
        6 -> do
          kindName <- name
          literalTag <- getWord
          literal <-
            case literalTag of
              0 -> TyLitNat <$!> getInteger
              1 -> TyLitSymbol <$!> getEntry "text" texts
              2 -> TyLitChar <$!> getCharacter
              _ -> fail "unknown type literal"
          pure $! TyLit kindName literal
        _ -> fail "unknown type"

-- | Apply a decoded function to a decoded value and evaluate the result.
(<*!>) :: Decode (a -> b) -> Decode a -> Decode b
getFunction <*!> getArgument = do
  function <- getFunction
  argument <- getArgument
  pure $! function argument

infixl 4 <*!>

getCharacter :: Decode Char
getCharacter = do
  code <- getWord
  unless (code <= 0x10FFFF) (fail "invalid character")
  pure $! chr (fromIntegral code)

getName :: Decoded -> Decode Name
getName tables = getEntry "name" (decodedNames tables)

getType :: Decoded -> Decode Type
getType tables = getEntry "type" (decodedTypes tables)

getBinder :: Decoded -> Decode Binder
getBinder tables = Binder <$!> getName tables <*!> getType tables

getImports :: Decoded -> Decode Imports
getImports tables =
  Imports
    <$!> getMap (getType tables)
      <*!> getMap (getType tables)
      <*!> getMap (getAxiomDecl tables)
      <*!> getMap (getType tables)
      <*!> getMap getConRepresentation
      <*!> getMap getInts
      <*!> getMap (getList (getName tables))
  where
    getMap getValue = Map.fromDistinctAscList <$!> getList ((,) <$!> getName tables <*!> getValue)

getInts :: Decode [Int]
getInts = getList getInt

getDecl :: Decoded -> Decode Decl
getDecl tables = do
  tag <- getWord
  case tag of
    0 ->
      DeclType
        <$!> ( TypeDecl
                 <$!> getVis
                   <*!> getName tables
                   <*!> getList (getBinder tables)
                   <*!> getType tables
                   <*!> getList getRole
                   <*!> getList (getConDecl tables)
             )
    1 ->
      DeclSynonym
        <$!> ( SynonymDecl
                 <$!> getVis
                   <*!> getName tables
                   <*!> getList (getBinder tables)
                   <*!> getType tables
                   <*!> getType tables
             )
    2 -> DeclAxiom <$!> getAxiomDecl tables
    3 ->
      DeclVal
        <$!> ( ValDecl
                 <$!> getVis
                   <*!> getName tables
                   <*!> getType tables
                   <*!> getExpr tables
                   <*!> getInlineSpec
             )
    4 ->
      DeclRule
        <$!> ( RuleDecl
                 <$!> getTextIndex tables
                   <*!> getActivation
                   <*!> getList (getBinder tables)
                   <*!> getList (getBinder tables)
                   <*!> getType tables
                   <*!> getExpr tables
                   <*!> getExpr tables
             )
    _ -> fail "unknown declaration"

getConDecl :: Decoded -> Decode ConDecl
getConDecl tables =
  ConDecl
    <$!> getVis
      <*!> getName tables
      <*!> getType tables
      <*!> getConRepresentation
      <*!> getInts

getAxiomDecl :: Decoded -> Decode AxiomDecl
getAxiomDecl tables =
  AxiomDecl
    <$!> getVis
      <*!> getName tables
      <*!> getList (getBinder tables)
      <*!> getRole
      <*!> getType tables
      <*!> getType tables

getConRepresentation :: Decode ConRepresentation
getConRepresentation = do
  tag <- getWord
  case tag of
    0 -> pure HeapConstructor
    1 -> pure UnboxedTupleConstructor
    2 -> UnboxedSumConstructor <$!> getInt <*!> getInt
    _ -> fail "unknown constructor representation"

getActivation :: Decode RuleActivation
getActivation = do
  tag <- getWord
  case tag of
    0 -> pure AlwaysActive
    1 -> ActiveAfter <$!> getInt
    2 -> ActiveBefore <$!> getInt
    3 -> pure NeverActive
    _ -> fail "unknown activation"

getInlineSpec :: Decode InlineSpec
getInlineSpec = do
  tag <- getWord
  case tag of
    0 -> pure InlineDefault
    1 -> InlineAlways <$!> getActivation
    2 -> InlineWhenUseful <$!> getActivation
    3 -> InlineNever <$!> getActivation
    _ -> fail "unknown inline specification"

getExpr :: Decoded -> Decode Expr
getExpr tables = do
  tag <- getWord
  case tag of
    0 -> ExVar <$!> getName tables
    1 -> ExLit <$!> getLiteral tables
    2 -> ExApp <$!> getExpr tables <*!> getExpr tables
    3 -> ExTyApp <$!> getExpr tables <*!> getType tables
    4 -> ExLam <$!> getBinder tables <*!> getExpr tables
    5 -> ExTyLam <$!> getBinder tables <*!> getExpr tables
    6 -> ExLet <$!> getBind tables <*!> getExpr tables
    7 -> ExRec <$!> getList (getBind tables) <*!> getExpr tables
    12 -> ExCase <$!> getExpr tables <*!> pure Nothing <*!> getType tables <*!> getList (getAlt tables)
    8 -> ExCase <$!> getExpr tables <*!> (Just <$!> getBinder tables) <*!> getType tables <*!> getList (getAlt tables)
    9 -> ExCast <$!> getExpr tables <*!> getCoercion tables
    10 -> ExCoercion <$!> getCoercion tables
    11 -> ExForeignCall <$!> getForeignCall tables <*!> getList (getType tables) <*!> getList (getExpr tables)
    _ -> fail "unknown expression"

getBind :: Decoded -> Decode Bind
getBind tables = Bind <$!> getBinder tables <*!> getExpr tables

getAlt :: Decoded -> Decode Alt
getAlt tables =
  Alt
    <$!> getAltCon tables
      <*!> getList (getBinder tables)
      <*!> getList (getBinder tables)
      <*!> getExpr tables

getAltCon :: Decoded -> Decode AltCon
getAltCon tables = do
  tag <- getWord
  case tag of
    0 -> AltData <$!> getName tables
    1 -> AltLit <$!> getLiteral tables
    2 -> pure AltDefault
    _ -> fail "unknown alternative"

getLiteral :: Decoded -> Decode Literal
getLiteral tables = do
  tag <- getWord
  case tag of
    0 -> LitInt <$!> getType tables <*!> getInteger
    1 -> LitChar <$!> getType tables <*!> getCharacter
    2 -> LitAddr <$!> getType tables <*!> getBytes
    _ -> fail "unknown literal"

getCoercion :: Decoded -> Decode Coercion
getCoercion tables = do
  tag <- getWord
  case tag of
    0 -> CoVar <$!> getName tables
    1 -> CoRefl <$!> getType tables
    2 -> CoSym <$!> getCoercion tables
    3 -> CoTrans <$!> getCoercion tables <*!> getCoercion tables
    4 -> CoApp <$!> getCoercion tables <*!> getCoercion tables
    5 -> CoFun <$!> getCoercion tables <*!> getCoercion tables
    6 -> CoForAll <$!> getBinder tables <*!> getCoercion tables
    7 -> CoNth <$!> getInt <*!> getCoercion tables
    8 -> CoTyConApp <$!> getName tables <*!> getList (getCoercion tables)
    9 -> CoAxiom <$!> getName tables <*!> getList (getType tables)
    _ -> fail "unknown coercion"

getForeignCall :: Decoded -> Decode ForeignCall
getForeignCall tables =
  ForeignCall
    <$!> getName tables
      <*!> getConvention tables
      <*!> getList getDependency
      <*!> getType tables
  where
    getDependency = do
      tag <- getWord
      case tag of
        0 -> ForeignAxiom <$!> getName tables
        1 -> ForeignConstructor <$!> getName tables
        _ -> fail "unknown foreign dependency"

getConvention :: Decoded -> Decode CallingConvention
getConvention tables = do
  tag <- getWord
  case tag of
    0 -> pure Prim
    1 ->
      CCall
        <$!> ( CCallSpec
                 <$!> getTextIndex tables
                   <*!> getEnumeration "call target" [CCallFunction, CCallAddress, CCallDynamic, CCallWrapper]
                   <*!> getEnumeration "safety" [ForeignUnsafe, ForeignSafe, ForeignInterruptible]
                   <*!> getList getAbiType
                   <*!> getAbiType
                   <*!> getEnumeration "effect" [ForeignPure, ForeignRealWorld]
             )
    _ -> fail "unknown calling convention"
  where
    getAbiType = do
      index <- getInt
      unless (index >= fromEnum (minBound :: CAbiType) && index <= fromEnum (maxBound :: CAbiType)) (fail "unknown C type")
      pure $! toEnum index

getSort :: Decode Sort
getSort = getEnumeration "sort" [SortTypeConstructor, SortDataConstructor, SortValue, SortTypeVariable, SortAxiom, SortSynonym]

getVis :: Decode Vis
getVis = getEnumeration "visibility" [Pub, Private]

getRole :: Decode Role
getRole = getEnumeration "role" [Nominal, Representational, Phantom]

-- | A value of an enumeration, by its tag. The list gives the values in
-- the order of their tags, as the @...Tag@ functions above give them.
getEnumeration :: String -> [value] -> Decode value
getEnumeration what values = do
  tag <- getWord
  case drop (fromIntegral tag) values of
    value : _ | tag < fromIntegral (length values) -> pure $! value
    _ -> fail ("unknown " <> what)
