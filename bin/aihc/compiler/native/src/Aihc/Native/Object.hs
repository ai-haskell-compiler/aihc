{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared data for direct native object generation.
--
-- A 'Draft' accumulates the bytes of every section in a
-- 'Data.ByteString.Builder.Builder' as the assembler walks the instruction
-- stream, together with the offset of each label and each fixup. Nothing is
-- kept per instruction, so the assembler allocates a few bytes rather than a
-- boxed item for every machine word it emits.
module Aihc.Native.Object
  ( Draft (..),
    draftSections,
    Fixup (..),
    FixupKind (..),
    Image (..),
    ImageSection (..),
    Item (..),
    Name (..),
    nameText,
    ObjectError (..),
    Relocation (..),
    SectionDraft (..),
    SectionRole (..),
    Symbol (..),
    addGlobal,
    addItem,
    addItems,
    emptyDraft,
    layoutDraft,
    layoutDraftWith,
    sectionBytes,
    selectSection,
    sealFunction,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Builder.Extra qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64, Word8)

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

-- | What a label or a fixup names: a symbol, by its text, or a label private
-- to the object, by a number. A private label carries its text only for
-- rendering; the assembler never compares or stores it.
data Name
  = SymbolName !Text
  | LocalName !Int Text

instance Eq Name where
  SymbolName left == SymbolName right = left == right
  LocalName left _ == LocalName right _ = left == right
  _ == _ = False

instance Ord Name where
  compare (LocalName left _) (LocalName right _) = compare left right
  compare (LocalName _ _) (SymbolName _) = LT
  compare (SymbolName _) (LocalName _ _) = GT
  compare (SymbolName left) (SymbolName right) = compare left right

instance Show Name where
  showsPrec precedence name = showsPrec precedence (nameText name)

nameText :: Name -> Text
nameText name =
  case name of
    SymbolName text -> text
    LocalName _ text -> text

-- | A place in a section whose bytes depend on the address of a symbol. The
-- width is the number of bytes the fixup occupies and the word is the value
-- written there before the address is known: the encoded instruction for a
-- branch, and zero for an absolute slot.
data Fixup = Fixup
  { fixupKind :: !FixupKind,
    fixupTarget :: !Name,
    fixupAddend :: !Int64,
    fixupWidth :: !Int,
    fixupWord :: !Word64
  }
  deriving (Eq, Show)

data Item
  = Bytes !ByteString
  | -- | A little-endian word of the given byte width.
    Word !Int !Word64
  | Align !Int !ByteString
  | Label !Name
  | Apply !Fixup
  deriving (Eq, Show)

-- | The bytes of one section and the offsets recorded inside them.
-- | The bytes gather in a builder that is flushed into a strict chunk every
-- 'chunkBytes', so that no long chain of pending appends stays alive.
data SectionDraft = SectionDraft
  { sectionSize :: !Word64,
    sectionAlignment :: !Int,
    -- | The flushed chunks, latest first.
    sectionChunksRev :: ![ByteString],
    -- | The bytes appended since the last flush, and how many.
    sectionPending :: !Builder.Builder,
    sectionPendingSize :: !Int,
    sectionLabelsRev :: ![(Text, Word64)],
    sectionLocalsRev :: ![(Int, Word64)],
    sectionFixupsRev :: ![(Word64, Fixup)],
    sectionPatchesRev :: ![(Word64, Word32)],
    sectionFunctionStart :: !Word64
  }

-- | The section being written is kept apart from the others, so that an
-- item touches no map.
data Draft = Draft
  { draftCurrentSection :: !(Maybe SectionRole),
    draftCurrent :: !SectionDraft,
    draftSectionOrder :: ![SectionRole],
    -- | Every section other than the current one.
    draftOtherSections :: !(Map SectionRole SectionDraft),
    draftGlobals :: !(Set Text)
  }

-- | Every section of the draft.
draftSections :: Draft -> Map SectionRole SectionDraft
draftSections draft =
  case draftCurrentSection draft of
    Nothing -> draftOtherSections draft
    Just role -> Map.insert role (draftCurrent draft) (draftOtherSections draft)

data Symbol = Symbol
  { symbolName :: !Text,
    symbolGlobal :: !Bool,
    symbolSection :: !(Maybe SectionRole),
    symbolOffset :: !Word64
  }
  deriving (Eq, Show)

-- | A place whose bytes the linker fills in. The symbol is a position in
-- 'imageSymbols', so an object writer never looks a name up again.
data Relocation = Relocation
  { relocationOffset :: !Word64,
    relocationKind :: !FixupKind,
    relocationSymbol :: !Int,
    relocationAddend :: !Int64
  }
  deriving (Eq, Show)

data ImageSection = ImageSection
  { imageSectionRole :: !SectionRole,
    imageSectionAlignment :: !Int,
    imageSectionSize :: !Word64,
    imageSectionBytes :: !BL.ByteString,
    imageSectionRelocations :: ![Relocation]
  }
  deriving (Eq, Show)

-- | The symbols are in ascending name order, and every 'relocationSymbol' is
-- a position in that list.
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
emptyDraft = Draft Nothing emptySection [] Map.empty Set.empty

emptySection :: SectionDraft
emptySection = SectionDraft 0 0 [] mempty 0 [] [] [] [] 0

-- | Resolve function labels and release their names before the next function.
sealFunction :: Draft -> Either ObjectError Draft
sealFunction draft = do
  locals <- foldl' addLocal (Right IntMap.empty) (sectionLocalsRev section)
  (fixups, patches) <- foldl' (resolve locals) (Right ([], sectionPatchesRev section)) current
  pure draft {draftCurrent = section {sectionLocalsRev = [], sectionFixupsRev = reverse fixups <> earlier, sectionPatchesRev = patches, sectionFunctionStart = sectionSize section}}
  where
    section = draftCurrent draft
    (current, earlier) = span ((>= sectionFunctionStart section) . fst) (sectionFixupsRev section)
    addLocal result (identifier, offset) = do
      locals <- result
      if IntMap.member identifier locals
        then Left (ObjectDuplicateSymbol (T.pack (".L" <> show identifier)))
        else pure (IntMap.insert identifier offset locals)
    resolve locals result entry@(offset, fixup) = do
      (fixups, patches) <- result
      case fixupTarget fixup of
        LocalName identifier _ -> case IntMap.lookup identifier locals of
          Nothing -> Left (ObjectMissingSymbol (nameText (fixupTarget fixup)))
          Just target -> do
            value <- patchLocal offset target fixup
            pure (fixups, (offset, value) : patches)
        SymbolName _ -> pure (entry : fixups, patches)

selectSection :: SectionRole -> Draft -> Draft
selectSection role draft
  | draftCurrentSection draft == Just role = draft
  | otherwise =
      let others = draftSections draft
       in draft
            { draftCurrentSection = Just role,
              draftCurrent = Map.findWithDefault emptySection role others,
              draftSectionOrder =
                if role `elem` draftSectionOrder draft
                  then draftSectionOrder draft
                  else draftSectionOrder draft <> [role],
              draftOtherSections = Map.delete role others
            }

addGlobal :: Text -> Draft -> Draft
addGlobal name draft = draft {draftGlobals = Set.insert name (draftGlobals draft)}

-- | Append one item to the current section.
addItem :: Item -> Draft -> Either ObjectError Draft
addItem item draft =
  case draftCurrentSection draft of
    Nothing -> Left ObjectNoSection
    Just _ -> do
      next <- appendItem item (draftCurrent draft)
      pure draft {draftCurrent = next}

-- | Append a run of items to the current section.
addItems :: [Item] -> Draft -> Either ObjectError Draft
addItems items draft =
  case draftCurrentSection draft of
    Nothing -> Left ObjectNoSection
    Just _ -> do
      next <- appendItems items (draftCurrent draft)
      pure draft {draftCurrent = next}

-- | Append a run of items, carrying the section in loop variables so that
-- an item costs no record. An alignment goes through 'appendItem'.
appendItems :: [Item] -> SectionDraft -> Either ObjectError SectionDraft
appendItems items0 section0 =
  go
    (sectionSize section0)
    (sectionChunksRev section0)
    (sectionPending section0)
    (sectionPendingSize section0)
    (sectionLabelsRev section0)
    (sectionLocalsRev section0)
    (sectionFixupsRev section0)
    items0
  where
    go !size chunks pending !pendingSize labels locals fixups items =
      case items of
        [] -> pure (rebuild size chunks pending pendingSize labels locals fixups)
        item : rest ->
          case item of
            Word width value ->
              append size chunks (pending <> littleEndian width value) (pendingSize + width) (fromIntegral width) labels locals fixups rest
            Bytes value ->
              append size chunks (pending <> Builder.byteString value) (pendingSize + BS.length value) (fromIntegral (BS.length value)) labels locals fixups rest
            Label (SymbolName name) -> go size chunks pending pendingSize ((name, size) : labels) locals fixups rest
            Label (LocalName identifier _) -> go size chunks pending pendingSize labels ((identifier, size) : locals) fixups rest
            Apply fixup ->
              let width = fixupWidth fixup
               in append size chunks (pending <> littleEndian width (fixupWord fixup)) (pendingSize + width) (fromIntegral width) labels locals ((size, fixup) : fixups) rest
            Align _ _ -> do
              next <- appendItem item (rebuild size chunks pending pendingSize labels locals fixups)
              appendItems rest next
    append size chunks pending pendingSize width labels locals fixups rest
      | pendingSize >= chunkBytes =
          go (size + width) (flushPending pending pendingSize : chunks) mempty 0 labels locals fixups rest
      | otherwise = go (size + width) chunks pending pendingSize labels locals fixups rest
    rebuild size chunks pending pendingSize labels locals fixups =
      section0
        { sectionSize = size,
          sectionChunksRev = chunks,
          sectionPending = pending,
          sectionPendingSize = pendingSize,
          sectionLabelsRev = labels,
          sectionLocalsRev = locals,
          sectionFixupsRev = fixups
        }

appendItem :: Item -> SectionDraft -> Either ObjectError SectionDraft
appendItem item section =
  case item of
    Bytes value -> pure (appendBytes (fromIntegral (BS.length value)) (Builder.byteString value) section)
    Word width value -> pure (appendBytes (fromIntegral width) (littleEndian width value) section)
    Label (SymbolName name) -> pure section {sectionLabelsRev = (name, sectionSize section) : sectionLabelsRev section}
    Label (LocalName identifier _) -> pure section {sectionLocalsRev = (identifier, sectionSize section) : sectionLocalsRev section}
    Apply fixup ->
      pure
        ( appendBytes
            (fromIntegral (fixupWidth fixup))
            (littleEndian (fixupWidth fixup) (fixupWord fixup))
            section {sectionFixupsRev = (sectionSize section, fixup) : sectionFixupsRev section}
        )
    Align alignmentPower fill
      | alignmentPower < 0 || alignmentPower > 30 -> Left (ObjectInvalidAlignment alignmentPower)
      | BS.null fill -> Left (ObjectInvalidInput "empty alignment fill")
      | otherwise ->
          let boundary = (1 `shiftL` alignmentPower) :: Word64
              padding = fromIntegral ((boundary - sectionSize section `mod` boundary) `mod` boundary)
              (fillCount, fillRemainder) = padding `divMod` BS.length fill
              paddingBytes =
                mconcat (replicate fillCount (Builder.byteString fill))
                  <> Builder.byteString (BS.take fillRemainder fill)
           in pure
                ( appendBytes
                    (fromIntegral padding)
                    paddingBytes
                    section {sectionAlignment = max (sectionAlignment section) alignmentPower}
                )

appendBytes :: Word64 -> Builder.Builder -> SectionDraft -> SectionDraft
appendBytes width bytes section
  | pendingSize >= chunkBytes = flushSection appended
  | otherwise = appended
  where
    pendingSize = sectionPendingSize section + fromIntegral width
    appended =
      section
        { sectionSize = sectionSize section + width,
          sectionPending = sectionPending section <> bytes,
          sectionPendingSize = pendingSize
        }

chunkBytes :: Int
chunkBytes = 65536

-- | Turn the pending bytes into a chunk.
flushSection :: SectionDraft -> SectionDraft
flushSection section
  | sectionPendingSize section == 0 = section
  | otherwise =
      section
        { sectionChunksRev = flushPending (sectionPending section) (sectionPendingSize section) : sectionChunksRev section,
          sectionPending = mempty,
          sectionPendingSize = 0
        }

flushPending :: Builder.Builder -> Int -> ByteString
flushPending pending pendingSize =
  BL.toStrict (Builder.toLazyByteStringWith (Builder.untrimmedStrategy pendingSize pendingSize) mempty pending)

-- | Every byte of the section.
sectionBytes :: SectionDraft -> BL.ByteString
sectionBytes section = BL.fromChunks (reverse (sectionChunksRev (flushSection section)))

littleEndian :: Int -> Word64 -> Builder.Builder
littleEndian width value =
  case width of
    1 -> Builder.word8 (fromIntegral value)
    2 -> Builder.word16LE (fromIntegral value)
    4 -> Builder.word32LE (fromIntegral value)
    8 -> Builder.word64LE value
    _ -> mconcat [Builder.word8 (byteAt index) | index <- [0 .. width - 1]]
  where
    byteAt index = fromIntegral (value `shiftR` (8 * index)) :: Word8

layoutDraft :: Draft -> Either ObjectError Image
layoutDraft = layoutDraftWith (const sectionBytes)

-- | Layout uses explicit sizes. It does not traverse section payloads.
layoutDraftWith :: (SectionRole -> SectionDraft -> BL.ByteString) -> Draft -> Either ObjectError Image
layoutDraftWith payload draft = do
  let firstPass = map layoutSection (draftSectionOrder draft)
  definitions <- collectDefinitions firstPass
  locals <- collectLocals firstPass
  let globals = draftGlobals draft
      -- Only a name that the linker needs becomes a symbol: a global one, or
      -- one that a relocation names. A label that this object resolves on its
      -- own, such as a branch target inside one function, needs no symbol.
      -- Generated code has many of these, and each one would otherwise cost a
      -- symbol table entry and its name.
      relocated =
        Set.fromList
          [ name
          | section <- firstPass,
            (_, fixup) <- laidFixups section,
            not (isLocalPatch globals definitions (laidRole section) fixup),
            SymbolName name <- [fixupTarget fixup]
          ]
      kept = Map.keysSet (Map.filterWithKey (\name _ -> name `Set.member` globals || name `Set.member` relocated) definitions)
      names = Set.toAscList (kept <> relocated <> globals)
      -- A name that this object defines and does not export is read by
      -- nothing: only the relocations beside it name it, and they name it by
      -- position. The text it was given upstream is dead weight, and
      -- generated code has one such name per entry function, per info table,
      -- and per enter stub, which is most of the string table of a library
      -- object. Number them instead. An exported or undefined name keeps its
      -- text, because that is what another object matches against.
      privateLabels =
        Map.fromList
          ( zip
              [name | name <- names, name `Set.notMember` globals, name `Map.member` definitions]
              [".L" <> T.pack (show index) | index <- [0 :: Int ..]]
          )
      emitted name = Map.findWithDefault name name privateLabels
      -- 'imageSymbols' ascends by the name each symbol is written under, so
      -- the order follows the label rather than the name it replaced.
      ordered = sortOn fst [(emitted name, name) | name <- names]
      symbols = [makeSymbol definitions label name | (label, name) <- ordered]
      table = Map.fromList (zip (map snd ordered) [0 ..])
  resolved <- mapM (resolveSection globals definitions locals table) firstPass
  pure Image {imageSections = resolved, imageSymbols = symbols}
  where
    sections = draftSections draft
    layoutSection role =
      let section = Map.findWithDefault emptySection role sections
       in LaidSection
            { laidRole = role,
              laidAlignment = sectionAlignment section,
              laidBytes = payload role section,
              laidSize = sectionSize section,
              laidLabels = reverse (sectionLabelsRev section),
              laidLocals = reverse (sectionLocalsRev section),
              laidFixups = reverse (sectionFixupsRev section),
              laidPatches = sectionPatchesRev section
            }
    makeSymbol definitions label name =
      case Map.lookup name definitions of
        Just (role, offset) -> Symbol label (name `Set.member` draftGlobals draft) (Just role) offset
        Nothing -> Symbol label True Nothing 0

data LaidSection = LaidSection
  { laidRole :: !SectionRole,
    laidAlignment :: !Int,
    laidBytes :: BL.ByteString,
    laidSize :: !Word64,
    laidLabels :: ![(Text, Word64)],
    laidLocals :: ![(Int, Word64)],
    laidFixups :: ![(Word64, Fixup)],
    laidPatches :: ![(Word64, Word32)]
  }

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

-- | Whether this object can fill a fixup in without the linker. The target
-- must sit in the same section, be private to this object, and have a kind
-- that 'patchLocal' handles.
isLocalPatch :: Set Text -> Map Text (SectionRole, Word64) -> SectionRole -> Fixup -> Bool
isLocalPatch globals definitions role fixup =
  canResolve (fixupKind fixup)
    && case fixupTarget fixup of
      LocalName _ _ -> True
      SymbolName name ->
        name `Set.notMember` globals
          && case Map.lookup name definitions of
            Just (targetRole, _) -> targetRole == role
            Nothing -> False

-- | The private labels of every section, by number.
collectLocals :: [LaidSection] -> Either ObjectError (IntMap (SectionRole, Word64))
collectLocals = foldl' addSection (Right IntMap.empty)
  where
    addSection result section = do
      locals <- result
      foldl' (addLabel (laidRole section)) (Right locals) (laidLocals section)
    addLabel role result (identifier, offset) = do
      locals <- result
      if IntMap.member identifier locals
        then Left (ObjectDuplicateSymbol (T.pack (".L" <> show identifier)))
        else pure (IntMap.insert identifier (role, offset) locals)

resolveSection :: Set Text -> Map Text (SectionRole, Word64) -> IntMap (SectionRole, Word64) -> Map Text Int -> LaidSection -> Either ObjectError ImageSection
resolveSection globals definitions locals table section = do
  (patches, relocations) <- foldl' resolve (Right ([], [])) (laidFixups section)
  bytes <- applyPatches (laidSize section) (laidBytes section) (sortOn fst (laidPatches section <> patches))
  pure
    ImageSection
      { imageSectionRole = laidRole section,
        imageSectionAlignment = laidAlignment section,
        imageSectionSize = laidSize section,
        imageSectionBytes = bytes,
        imageSectionRelocations = reverse relocations
      }
  where
    resolve result (offset, fixup) = do
      (patches, relocations) <- result
      case fixupTarget fixup of
        -- A private label is only ever reached by a branch within its own
        -- section, so the object always fills the fixup in itself.
        LocalName identifier _ ->
          case IntMap.lookup identifier locals of
            Just (targetRole, targetOffset)
              | targetRole == laidRole section && canResolve (fixupKind fixup) -> do
                  patched <- patchLocal offset targetOffset fixup
                  pure ((offset, patched) : patches, relocations)
              | otherwise -> Left (ObjectInvalidFixup (fixupKind fixup))
            Nothing -> Left (ObjectMissingSymbol (nameText (fixupTarget fixup)))
        SymbolName name
          | isLocalPatch globals definitions (laidRole section) fixup ->
              case Map.lookup name definitions of
                Nothing -> Left (ObjectMissingSymbol name)
                Just (_, targetOffset) -> do
                  patched <- patchLocal offset targetOffset fixup
                  pure ((offset, patched) : patches, relocations)
          | otherwise ->
              case Map.lookup name table of
                Nothing -> Left (ObjectMissingSymbol name)
                Just index ->
                  pure
                    ( patches,
                      Relocation offset (fixupKind fixup) index (fixupAddend fixup) : relocations
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

patchLocal :: Word64 -> Word64 -> Fixup -> Either ObjectError Word32
patchLocal offset target fixup =
  case fixupKind fixup of
    Arm64Branch26 ->
      if displacement `mod` 4 /= 0 || not (fitsSigned 28 displacement)
        then Left (ObjectDisplacementOutOfRange (nameText (fixupTarget fixup)))
        else pure (instruction .|. fromIntegral ((displacement `shiftR` 2) .&. 0x03ffffff))
    Arm64Branch19 ->
      if displacement `mod` 4 /= 0 || not (fitsSigned 21 displacement)
        then Left (ObjectDisplacementOutOfRange (nameText (fixupTarget fixup)))
        else pure (instruction .|. fromIntegral (((displacement `shiftR` 2) .&. 0x7ffff) `shiftL` 5))
    Arm64Adr21 ->
      if not (fitsSigned 21 displacement)
        then Left (ObjectDisplacementOutOfRange (nameText (fixupTarget fixup)))
        else
          let immediate = displacement .&. 0x1fffff
              low = fromIntegral ((immediate .&. 3) `shiftL` 29)
              high = fromIntegral (((immediate `shiftR` 2) .&. 0x7ffff) `shiftL` 5)
           in pure (instruction .|. low .|. high)
    X86Pc32 -> patchX86
    X86Plt32 -> patchX86
    kind -> Left (ObjectInvalidFixup kind)
  where
    instruction = fromIntegral (fixupWord fixup) :: Word32
    displacement = signedDifference target offset + fixupAddend fixup
    patchX86 =
      if fitsSigned 32 displacement
        then pure (fromIntegral displacement)
        else Left (ObjectDisplacementOutOfRange (nameText (fixupTarget fixup)))

-- | Apply ordered patches as the consumer reads the section chunks.
applyPatches :: Word64 -> BL.ByteString -> [(Word64, Word32)] -> Either ObjectError BL.ByteString
applyPatches size bytes patches = do
  check 0 patches
  pure (Builder.toLazyByteString (patch 0 bytes patches))
  where
    check start remaining =
      case remaining of
        [] -> pure ()
        (offset, _) : rest ->
          let index = offset
           in if index < start || index > size || size - index < 4
                then Left (ObjectSizeOverflow "fixup offset")
                else check (index + 4) rest
    patch _ remaining [] = Builder.lazyByteString remaining
    patch start remaining ((offset, value) : rest) =
      let (prefix, suffix) = BL.splitAt (fromIntegral (offset - start)) remaining
       in Builder.lazyByteString prefix <> Builder.word32LE value <> patch (offset + 4) (BL.drop 4 suffix) rest

signedDifference :: Word64 -> Word64 -> Int64
signedDifference left right = fromIntegral left - fromIntegral right

fitsSigned :: Int -> Int64 -> Bool
fitsSigned bits value = value >= negate (1 `shiftL` (bits - 1)) && value < (1 `shiftL` (bits - 1))
