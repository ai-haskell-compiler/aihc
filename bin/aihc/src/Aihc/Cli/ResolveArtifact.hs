module Aihc.Cli.ResolveArtifact
  ( ResolveArtifact (..),
    decodeResolveArtifact,
    encodeResolveArtifact,
    encodeResolveArtifactParts,
    encodeResolveExports,
  )
where

import Aihc.Cbor (cborArray, cborInt, cborText, cborWord, getArrayLength, getInt, getText, getWord)
import Aihc.Parser.Syntax (FixityAssoc (..))
import Aihc.Resolve (Entity (..), ExportEntry (..), Exports, GlobalName (..), LocalId (..), OperatorFixity (..), PackageId (..), ResolutionNamespace (..), exportsEntries, exportsFromEntries)
import Control.Monad (replicateM, (<$!>))
import Data.Binary.Get qualified as Get
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Word (Word64)

-- | What the store keeps of one module after name resolution: what the
-- module exports, as the modules that import it see it.
data ResolveArtifact = ResolveArtifact
  { resolveArtifactModuleName :: !Text,
    resolveArtifactExports :: !Exports
  }
  deriving (Eq)

encodeResolveArtifact :: ResolveArtifact -> BL.ByteString
encodeResolveArtifact = fst . encodeResolveArtifactParts

-- | The artifact bytes together with the bytes of the exports inside them,
-- so that the writer can take the exports digest from bytes it encodes
-- anyway.
encodeResolveArtifactParts :: ResolveArtifact -> (BL.ByteString, BL.ByteString)
encodeResolveArtifactParts artifact =
  ( Builder.toLazyByteString $
      cborArray 3
        <> cborText "aihc-resolve"
        <> cborText (resolveArtifactModuleName artifact)
        <> Builder.lazyByteString exportsBytes,
    exportsBytes
  )
  where
    exportsBytes = encodeResolveExports (resolveArtifactExports artifact)

encodeResolveExports :: Exports -> BL.ByteString
encodeResolveExports = Builder.toLazyByteString . encodeExports

decodeResolveArtifact :: BS.ByteString -> Either String ResolveArtifact
decodeResolveArtifact bytes =
  case Get.runGetOrFail getArtifact (BL.fromStrict bytes) of
    Left (_, _, message) -> Left message
    Right (remaining, _, artifact)
      | BL.null remaining -> Right artifact
      | otherwise -> Left "invalid trailing data"

getArtifact :: Get.Get ResolveArtifact
getArtifact = do
  3 <- getArrayLength
  "aihc-resolve" <- getText
  resolveArtifactModuleName <- getText
  resolveArtifactExports <- getExports
  pure ResolveArtifact {resolveArtifactModuleName, resolveArtifactExports}

-- | The exports as the list of facts that 'exportsEntries' gives, each
-- tagged by its kind.
encodeExports :: Exports -> Builder.Builder
encodeExports exports = cborArray (length entries) <> foldMap encodeEntry entries
  where
    entries = exportsEntries exports

getExports :: Get.Get Exports
getExports = do
  count <- getArrayLength
  exportsFromEntries <$!> replicateM count getEntry

encodeEntry :: ExportEntry -> Builder.Builder
encodeEntry entry =
  case entry of
    ExportTerm name entity -> cborArray 3 <> cborWord 0 <> cborText name <> encodeEntity entity
    ExportType name entity -> cborArray 3 <> cborWord 1 <> cborText name <> encodeEntity entity
    ExportConstructors name members -> cborArray 3 <> cborWord 2 <> cborText name <> encodeTextList members
    ExportRecordFields name members -> cborArray 3 <> cborWord 3 <> cborText name <> encodeTextList members
    ExportMethods name members -> cborArray 3 <> cborWord 4 <> cborText name <> encodeTextList members
    ExportAssociatedTypes name members -> cborArray 3 <> cborWord 5 <> cborText name <> encodeTextList members
    ExportFixity name fixity -> cborArray 3 <> cborWord 6 <> cborText name <> encodeFixity fixity

getEntry :: Get.Get ExportEntry
getEntry = do
  3 <- getArrayLength
  tag <- getWord
  name <- getText
  case tag of
    0 -> ExportTerm name <$!> getEntity
    1 -> ExportType name <$!> getEntity
    2 -> ExportConstructors name <$!> getTextList
    3 -> ExportRecordFields name <$!> getTextList
    4 -> ExportMethods name <$!> getTextList
    5 -> ExportAssociatedTypes name <$!> getTextList
    6 -> ExportFixity name <$!> getFixity
    _ -> fail "unsupported export entry"

encodeEntity :: Entity -> Builder.Builder
encodeEntity entity =
  case entity of
    EntityGlobal (GlobalName name (PackageId packageId) moduleName' namespace) ->
      cborArray 5 <> cborWord 0 <> cborText name <> cborText packageId <> cborText moduleName' <> cborWord (namespaceTag namespace)
    EntityLocal (LocalId unique) -> cborArray 2 <> cborWord 1 <> cborInt unique
    EntitySyntax -> cborArray 1 <> cborWord 2

getEntity :: Get.Get Entity
getEntity = do
  length' <- getArrayLength
  tag <- getWord
  case (length', tag) of
    (5, 0) -> do
      name <- getText
      packageId <- PackageId <$!> getText
      moduleName' <- getText
      EntityGlobal . GlobalName name packageId moduleName' <$!> getNamespace
    (2, 1) -> EntityLocal . LocalId <$!> getInt
    (1, 2) -> pure EntitySyntax
    _ -> fail "unsupported entity"

namespaceTag :: ResolutionNamespace -> Word64
namespaceTag namespace = case namespace of
  ResolutionNamespaceTerm -> 0
  ResolutionNamespaceType -> 1
  ResolutionNamespaceModule -> 2

getNamespace :: Get.Get ResolutionNamespace
getNamespace = do
  tag <- getWord
  case tag of
    0 -> pure ResolutionNamespaceTerm
    1 -> pure ResolutionNamespaceType
    2 -> pure ResolutionNamespaceModule
    _ -> fail "unsupported namespace"

encodeTextList :: [Text] -> Builder.Builder
encodeTextList values = cborArray (length values) <> foldMap cborText values

getTextList :: Get.Get [Text]
getTextList = do
  count <- getArrayLength
  replicateM count getText

encodeFixity :: OperatorFixity -> Builder.Builder
encodeFixity (OperatorFixity association precedence) =
  cborArray 2 <> cborWord (fixityTag association) <> cborWord (fromIntegral precedence)
  where
    fixityTag Infix = 0
    fixityTag InfixL = 1
    fixityTag InfixR = 2

getFixity :: Get.Get OperatorFixity
getFixity = do
  2 <- getArrayLength
  association <- getFixityAssoc
  precedence <- fromIntegral <$!> getWord
  pure (OperatorFixity association precedence)

getFixityAssoc :: Get.Get FixityAssoc
getFixityAssoc = do
  tag <- getWord
  case tag of
    0 -> pure Infix
    1 -> pure InfixL
    2 -> pure InfixR
    _ -> fail "unsupported fixity association"
