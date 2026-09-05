{-# LANGUAGE OverloadedStrings #-}

-- | Shared data for direct native object generation.
module Aihc.Native.Object
  ( Draft (..),
    Fixup (..),
    FixupKind (..),
    Image (..),
    ImageSection (..),
    Item (..),
    ObjectError (..),
    Relocation (..),
    SectionRole (..),
    Symbol (..),
    addGlobal,
    addItem,
    emptyDraft,
    layoutDraft,
    selectSection,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word32, Word64)

data SectionRole
  = TextSection
  | TextConstantsSection
  | ReadOnlySection
  | DataSection
  | NoExecuteStackSection
  deriving (Eq, Ord, Show)

data FixupKind
  = Arm64Branch26
  | Arm64Branch19
  | Arm64Adr21
  | Arm64Page21
  | Arm64PageOffset12
  | Absolute64
  | X86Pc32
  | X86Plt32
  deriving (Eq, Show)

data Fixup = Fixup
  { fixupKind :: !FixupKind,
    fixupTarget :: !Text,
    fixupAddend :: !Int64,
    fixupBytes :: !ByteString
  }
  deriving (Eq, Show)

data Item
  = Bytes !ByteString
  | Align !Int !ByteString
  | Label !Text
  | Apply !Fixup
  deriving (Eq, Show)

data Draft = Draft
  { draftCurrentSection :: !(Maybe SectionRole),
    draftSectionOrder :: ![SectionRole],
    draftSectionItems :: !(Map SectionRole [Item]),
    draftGlobals :: !(Set Text)
  }
  deriving (Eq, Show)

data Symbol = Symbol
  { symbolName :: !Text,
    symbolGlobal :: !Bool,
    symbolSection :: !(Maybe SectionRole),
    symbolOffset :: !Word64
  }
  deriving (Eq, Show)

data Relocation = Relocation
  { relocationOffset :: !Word64,
    relocationKind :: !FixupKind,
    relocationTarget :: !Text,
    relocationAddend :: !Int64
  }
  deriving (Eq, Show)

data ImageSection = ImageSection
  { imageSectionRole :: !SectionRole,
    imageSectionAlignment :: !Int,
    imageSectionBytes :: !BL.ByteString,
    imageSectionRelocations :: ![Relocation]
  }
  deriving (Eq, Show)

data Image = Image
  { imageSections :: ![ImageSection],
    imageSymbols :: ![Symbol]
  }
  deriving (Eq, Show)

data ObjectError
  = ObjectNoSection
  | ObjectDuplicateSymbol !Text
  | ObjectMissingSymbol !Text
  | ObjectInvalidAlignment !Int
  | ObjectDisplacementOutOfRange !Text
  | ObjectInvalidFixup !FixupKind
  | ObjectInvalidInput !Text
  | ObjectSizeOverflow !Text
  deriving (Eq, Show)

emptyDraft :: Draft
emptyDraft = Draft Nothing [] Map.empty Set.empty

selectSection :: SectionRole -> Draft -> Draft
selectSection role draft =
  draft
    { draftCurrentSection = Just role,
      draftSectionOrder =
        if role `elem` draftSectionOrder draft
          then draftSectionOrder draft
          else draftSectionOrder draft <> [role],
      -- Keep the existing list. An append would copy all previous items.
      draftSectionItems = Map.insertWith (\_ items -> items) role [] (draftSectionItems draft)
    }

addGlobal :: Text -> Draft -> Draft
addGlobal name draft = draft {draftGlobals = Set.insert name (draftGlobals draft)}

addItem :: Item -> Draft -> Either ObjectError Draft
addItem item draft =
  case draftCurrentSection draft of
    Nothing -> Left ObjectNoSection
    Just role ->
      Right
        draft
          { draftSectionItems = Map.adjust (item :) role (draftSectionItems draft)
          }

layoutDraft :: Draft -> Either ObjectError Image
layoutDraft draft = do
  firstPass <- mapM layoutSection (draftSectionOrder draft)
  definitions <- collectDefinitions firstPass
  let referenced = Set.fromList [fixupTarget fixup | section <- firstPass, (_, fixup) <- laidFixups section]
      names = Set.toAscList (Map.keysSet definitions <> referenced <> draftGlobals draft)
      symbols = map (makeSymbol definitions) names
      globals = draftGlobals draft
  sections <- mapM (resolveSection globals definitions) firstPass
  pure Image {imageSections = sections, imageSymbols = symbols}
  where
    layoutSection role = layoutItems role (reverse (Map.findWithDefault [] role (draftSectionItems draft)))
    makeSymbol definitions name =
      case Map.lookup name definitions of
        Just (role, offset) -> Symbol name (name `Set.member` draftGlobals draft) (Just role) offset
        Nothing -> Symbol name True Nothing 0

data LaidSection = LaidSection
  { laidRole :: !SectionRole,
    laidAlignment :: !Int,
    laidBytes :: !ByteString,
    laidLabels :: ![(Text, Word64)],
    laidFixups :: ![(Word64, Fixup)]
  }

layoutItems :: SectionRole -> [Item] -> Either ObjectError LaidSection
layoutItems role = go 0 0 [] [] []
  where
    go offset alignment bytes labels fixups remaining =
      case remaining of
        [] ->
          pure
            LaidSection
              { laidRole = role,
                laidAlignment = alignment,
                laidBytes = BS.concat (reverse bytes),
                laidLabels = reverse labels,
                laidFixups = reverse fixups
              }
        item : rest ->
          case item of
            Bytes value -> go (offset + byteLength value) alignment (value : bytes) labels fixups rest
            Apply fixup ->
              let value = fixupBytes fixup
               in go (offset + byteLength value) alignment (value : bytes) labels ((offset, fixup) : fixups) rest
            Label name -> go offset alignment bytes ((name, offset) : labels) fixups rest
            Align alignmentPower fill
              | alignmentPower < 0 || alignmentPower > 30 -> Left (ObjectInvalidAlignment alignmentPower)
              | BS.null fill -> Left (ObjectInvalidInput "empty alignment fill")
              | otherwise ->
                  let boundary = (1 `shiftL` alignmentPower) :: Word64
                      padding = fromIntegral ((boundary - offset `mod` boundary) `mod` boundary)
                      (fillCount, fillRemainder) = padding `divMod` BS.length fill
                      paddingBytes = BS.concat (replicate fillCount fill) <> BS.take fillRemainder fill
                   in go (offset + fromIntegral padding) (max alignment alignmentPower) (paddingBytes : bytes) labels fixups rest
    byteLength = fromIntegral . BS.length

collectDefinitions :: [LaidSection] -> Either ObjectError (Map Text (SectionRole, Word64))
collectDefinitions = foldl' addSection (Right Map.empty)
  where
    addSection result section = do
      definitions <- result
      foldl' (addLabel (laidRole section)) (Right definitions) (laidLabels section)
    addLabel role result (name, offset) = do
      definitions <- result
      if Map.member name definitions
        then Left (ObjectDuplicateSymbol name)
        else pure (Map.insert name (role, offset) definitions)

resolveSection :: Set Text -> Map Text (SectionRole, Word64) -> LaidSection -> Either ObjectError ImageSection
resolveSection globals definitions section = do
  (patches, relocations) <- foldl' resolve (Right ([], [])) (laidFixups section)
  bytes <- applyPatches (laidBytes section) (reverse patches)
  pure
    ImageSection
      { imageSectionRole = laidRole section,
        imageSectionAlignment = laidAlignment section,
        imageSectionBytes = bytes,
        imageSectionRelocations = reverse relocations
      }
  where
    resolve result (offset, fixup) = do
      (patches, relocations) <- result
      case Map.lookup (fixupTarget fixup) definitions of
        Just (targetRole, targetOffset)
          | canResolve (fixupKind fixup)
              && targetRole == laidRole section
              && fixupTarget fixup `Set.notMember` globals -> do
              patched <- patchLocal offset targetOffset fixup (laidBytes section)
              pure ((offset, patched) : patches, relocations)
        _ ->
          pure
            ( patches,
              Relocation offset (fixupKind fixup) (fixupTarget fixup) (fixupAddend fixup) : relocations
            )

canResolve :: FixupKind -> Bool
canResolve kind =
  case kind of
    Arm64Branch26 -> True
    Arm64Branch19 -> True
    Arm64Adr21 -> True
    X86Pc32 -> True
    X86Plt32 -> True
    _ -> False

patchLocal :: Word64 -> Word64 -> Fixup -> ByteString -> Either ObjectError Word32
patchLocal offset target fixup bytes =
  case fixupKind fixup of
    Arm64Branch26 -> do
      instruction <- readWord32 offset bytes
      let displacement = signedDifference target offset + fixupAddend fixup
      if displacement `mod` 4 /= 0 || not (fitsSigned 28 displacement)
        then Left (ObjectDisplacementOutOfRange (fixupTarget fixup))
        else pure (instruction .|. fromIntegral ((displacement `shiftR` 2) .&. 0x03ffffff))
    Arm64Branch19 -> do
      instruction <- readWord32 offset bytes
      let displacement = signedDifference target offset + fixupAddend fixup
      if displacement `mod` 4 /= 0 || not (fitsSigned 21 displacement)
        then Left (ObjectDisplacementOutOfRange (fixupTarget fixup))
        else pure (instruction .|. fromIntegral (((displacement `shiftR` 2) .&. 0x7ffff) `shiftL` 5))
    Arm64Adr21 -> do
      instruction <- readWord32 offset bytes
      let displacement = signedDifference target offset + fixupAddend fixup
      if not (fitsSigned 21 displacement)
        then Left (ObjectDisplacementOutOfRange (fixupTarget fixup))
        else
          let immediate = displacement .&. 0x1fffff
              low = fromIntegral ((immediate .&. 3) `shiftL` 29)
              high = fromIntegral (((immediate `shiftR` 2) .&. 0x7ffff) `shiftL` 5)
           in pure (instruction .|. low .|. high)
    X86Pc32 -> patchX86
    X86Plt32 -> patchX86
    kind -> Left (ObjectInvalidFixup kind)
  where
    patchX86 =
      let displacement = signedDifference target offset + fixupAddend fixup
       in if fitsSigned 32 displacement
            then pure (fromIntegral displacement)
            else Left (ObjectDisplacementOutOfRange (fixupTarget fixup))

applyPatches :: ByteString -> [(Word64, Word32)] -> Either ObjectError BL.ByteString
applyPatches bytes = fmap Builder.toLazyByteString . go 0
  where
    size = BS.length bytes
    go start patches =
      case patches of
        [] -> pure (Builder.byteString (BS.drop start bytes))
        (offset, value) : rest -> do
          let index = fromIntegral offset
          if index < start || index + 4 > size
            then Left (ObjectSizeOverflow "fixup offset")
            else do
              suffix <- go (index + 4) rest
              pure (Builder.byteString (BS.take (index - start) (BS.drop start bytes)) <> Builder.word32LE value <> suffix)

signedDifference :: Word64 -> Word64 -> Int64
signedDifference left right = fromIntegral left - fromIntegral right

fitsSigned :: Int -> Int64 -> Bool
fitsSigned bits value = value >= negate (1 `shiftL` (bits - 1)) && value < (1 `shiftL` (bits - 1))

readWord32 :: Word64 -> ByteString -> Either ObjectError Word32
readWord32 offset bytes =
  if offset + 4 > fromIntegral (BS.length bytes)
    then Left (ObjectSizeOverflow "fixup offset")
    else
      let index = fromIntegral offset
       in pure
            ( fromIntegral (BS.index bytes index)
                .|. fromIntegral (BS.index bytes (index + 1)) `shiftL` 8
                .|. fromIntegral (BS.index bytes (index + 2)) `shiftL` 16
                .|. fromIntegral (BS.index bytes (index + 3)) `shiftL` 24
            )
