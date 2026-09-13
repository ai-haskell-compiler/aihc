{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Direct native object generation into mutable section buffers.
--
-- An 'Object' owns one buffer per section: a run of 64 KB pinned chunks
-- that the assembler writes machine words into as it walks the instruction
-- stream, together with unboxed columns of the labels it defined and the
-- fixups it still owes. Nothing is allocated per instruction beyond the
-- bytes themselves: a word is a store, a label is a column write, and a
-- fixup is a row. Symbols are interned to numbers as they arrive, so the
-- layout that turns the buffers into an 'Image' indexes arrays rather than
-- comparing names, and a fixup this object resolves on its own is patched
-- into the buffer in place rather than copied through a patch list.
--
-- Everything lives in 'ST', so an object assembled from a list of
-- statements is a pure function of that list, and a chunk becomes the
-- payload of its section without a copy once the buffer is frozen.
module Aihc.Native.Object
  ( Object,
    newObject,
    assembleObject,
    applyAll,
    currentSectionRole,
    selectSection,
    addGlobal,
    Item (..),
    Fixup (..),
    emitItem,
    emitItems,
    emitAlign,
    sealFunction,
    layoutObject,
    FixupKind (..),
    Image (..),
    ImageSection (..),
    imageMetadata,
    Name (..),
    nameText,
    ObjectError (..),
    Relocation (..),
    SectionRole (..),
    Symbol (..),
  )
where

import Control.Monad (filterM, when)
import Control.Monad.ST (ST, runST)
import Data.Array (listArray, (!))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short (toShort)
import Data.ByteString.Short.Internal (ShortByteString (SBS), fromShort)
import Data.Int (Int64, Int8)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, copyByteArray, newPinnedByteArray, readByteArray, unsafeFreezeByteArray, writeByteArray)
import Data.Primitive.MutVar (MutVar, modifyMutVar', newMutVar, readMutVar, writeMutVar)
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, setPrimArray, writePrimArray)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Generic.Mutable qualified as MG
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word32, Word64, Word8)

data SectionRole
  = TextSection
  | TextConstantsSection
  | ReadOnlySection
  | DataSection
  | NoExecuteStackSection
  deriving (Enum, Eq, Ord, Show)

data FixupKind
  = Arm64Branch26
  | Arm64Branch19
  | Arm64Adr21
  | Arm64Page21
  | Arm64PageOffset12
  | Absolute64
  | X86Pc32
  | X86Plt32
  deriving (Enum, Eq, Show)

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

-- | One thing an encoder adds to the current section.
data Item
  = Bytes !ByteString
  | -- | A little-endian word of the given byte width.
    Word !Int !Word64
  | Label !Name
  | Apply !Fixup
  deriving (Eq, Show)

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

-- | The relocations ascend by offset.
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

-- | Remove payload references from metadata used after section output.
imageMetadata :: Image -> Image
imageMetadata image = image {imageSections = map (\section -> section {imageSectionBytes = BL.empty}) (imageSections image)}

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

ok :: Either ObjectError ()
ok = Right ()

-- | Run actions until the first that fails.
applyAll :: (value -> ST s (Either err ())) -> [value] -> ST s (Either err ())
applyAll action = go
  where
    go values =
      case values of
        [] -> pure (Right ())
        value : rest -> do
          result <- action value
          case result of
            Left err -> pure (Left err)
            Right () -> go rest
{-# INLINE applyAll #-}

-- Columns

-- | A growable vector whose cells past the end read as a default.
data Column v s value = Column !value !(MutVar s (v s value))

newColumn :: (MG.MVector v value) => value -> Int -> ST s (Column v s value)
newColumn fallback capacity = Column fallback <$> (MG.replicate capacity fallback >>= newMutVar)
{-# INLINE newColumn #-}

readColumn :: (MG.MVector v value) => Column v s value -> Int -> ST s value
readColumn (Column fallback ref) index = do
  vector <- readMutVar ref
  if index < MG.length vector
    then MG.unsafeRead vector index
    else pure fallback
{-# INLINE readColumn #-}

writeColumn :: (MG.MVector v value) => Column v s value -> Int -> value -> ST s ()
writeColumn (Column fallback ref) index value = do
  vector <- readMutVar ref
  let size = MG.length vector
  grown <-
    if index < size
      then pure vector
      else do
        let size' = max (index + 1) (2 * size)
        larger <- MG.unsafeGrow vector (size' - size)
        MG.set (MG.unsafeSlice size (size' - size) larger) fallback
        writeMutVar ref larger
        pure larger
  MG.unsafeWrite grown index value
{-# INLINE writeColumn #-}

type Unboxed = Column MU.MVector

type Boxed = Column MV.MVector

undefinedSection :: Int8
undefinedSection = -1

-- Sections

chunkBytes :: Int
chunkBytes = 65536

chunkShift :: Int
chunkShift = 16

chunkMask :: Int
chunkMask = chunkBytes - 1

-- | The bytes of one section and the fixups recorded over them. A fixup
-- row names its target by symbol number, or by @-1 - local@ for a private
-- label.
data Section s = Section
  { sectionRole :: !SectionRole,
    -- | The size, the alignment power, the chunk count, the fixup count,
    -- and the number of leading fixups that 'sealFunction' has already
    -- examined.
    sectionCounters :: !(MutablePrimArray s Int),
    sectionChunks :: !(Boxed s (MutableByteArray s)),
    fixupOffsets :: !(Unboxed s Int),
    fixupAddends :: !(Unboxed s Int64),
    fixupTargets :: !(Unboxed s Int),
    fixupKinds :: !(Unboxed s Word8)
  }

counterSize, counterAlignment, counterChunks, counterFixups, counterSealed :: Int
counterSize = 0
counterAlignment = 1
counterChunks = 2
counterFixups = 3
counterSealed = 4

newSection :: SectionRole -> ST s (Section s)
newSection role = do
  counters <- newPrimArray 5
  setPrimArray counters 0 5 0
  -- The chunk column is never read past its count, so its default is
  -- never observed; the first chunk stands in for it.
  first <- newPinnedByteArray chunkBytes
  Section role counters
    <$> newColumn first 4
    <*> newColumn 0 64
    <*> newColumn 0 64
    <*> newColumn 0 64
    <*> newColumn 0 64

sectionSize :: Section s -> ST s Int
sectionSize section = readPrimArray (sectionCounters section) counterSize
{-# INLINE sectionSize #-}

-- | The chunk with the given index, allocated when it is the next one.
chunkAt :: Section s -> Int -> ST s (MutableByteArray s)
chunkAt section index = do
  count <- readPrimArray (sectionCounters section) counterChunks
  if index < count
    then readColumn (sectionChunks section) index
    else do
      chunk <- newPinnedByteArray chunkBytes
      writeColumn (sectionChunks section) count chunk
      writePrimArray (sectionCounters section) counterChunks (count + 1)
      pure chunk

-- | Store a little-endian word inside one chunk.
pokeWord :: MutableByteArray s -> Int -> Int -> Word64 -> ST s ()
pokeWord chunk offset width value = go 0
  where
    go index =
      when (index < width) $ do
        writeByteArray chunk (offset + index) (fromIntegral (value `shiftR` (8 * index)) :: Word8)
        go (index + 1)

readByte :: MutableByteArray s -> Int -> ST s Word8
readByte = readByteArray
{-# INLINE readByte #-}

-- | Load a little-endian word from inside one chunk.
peekWord :: MutableByteArray s -> Int -> Int -> ST s Word64
peekWord chunk offset width = go 0 0
  where
    go index !value
      | index < width = do
          byte <- readByte chunk (offset + index)
          go (index + 1) (value .|. (fromIntegral byte `shiftL` (8 * index)))
      | otherwise = pure value

-- | Append one byte.
emitByte :: Section s -> Word8 -> ST s ()
emitByte section value = do
  size <- sectionSize section
  chunk <- chunkAt section (size `shiftR` chunkShift)
  writeByteArray chunk (size .&. chunkMask) value
  writePrimArray (sectionCounters section) counterSize (size + 1)

-- | Append a little-endian word.
emitWord :: Section s -> Int -> Word64 -> ST s ()
emitWord section width value = do
  size <- sectionSize section
  let within = size .&. chunkMask
  if within + width <= chunkBytes
    then do
      chunk <- chunkAt section (size `shiftR` chunkShift)
      pokeWord chunk within width value
      writePrimArray (sectionCounters section) counterSize (size + width)
    else mapM_ (\index -> emitByte section (fromIntegral (value `shiftR` (8 * index)))) [0 .. width - 1]

-- | Append bytes.
emitBytes :: Section s -> ByteString -> ST s ()
emitBytes section value = go 0
  where
    source = case toShort value of SBS array -> ByteArray array
    total = BS.length value
    go done =
      when (done < total) $ do
        size <- sectionSize section
        let within = size .&. chunkMask
            count = min (total - done) (chunkBytes - within)
        chunk <- chunkAt section (size `shiftR` chunkShift)
        copyByteArray chunk within source done count
        writePrimArray (sectionCounters section) counterSize (size + count)
        go (done + count)

-- | Read a little-endian word at an offset.
readWordAt :: Section s -> Int -> Int -> ST s Word64
readWordAt section offset width = do
  let within = offset .&. chunkMask
  if within + width <= chunkBytes
    then do
      chunk <- chunkAt section (offset `shiftR` chunkShift)
      peekWord chunk within width
    else do
      bytes <-
        mapM
          ( \index -> do
              chunk <- chunkAt section ((offset + index) `shiftR` chunkShift)
              readByte chunk ((offset + index) .&. chunkMask)
          )
          [0 .. width - 1]
      pure (foldr (\byte value -> (value `shiftL` 8) .|. fromIntegral byte) 0 bytes)

-- | Write a little-endian word at an offset.
writeWordAt :: Section s -> Int -> Int -> Word64 -> ST s ()
writeWordAt section offset width value = do
  let within = offset .&. chunkMask
  if within + width <= chunkBytes
    then do
      chunk <- chunkAt section (offset `shiftR` chunkShift)
      pokeWord chunk within width value
    else
      mapM_
        ( \index -> do
            chunk <- chunkAt section ((offset + index) `shiftR` chunkShift)
            writeByteArray chunk ((offset + index) .&. chunkMask) (fromIntegral (value `shiftR` (8 * index)) :: Word8)
        )
        [0 .. width - 1]

-- | Every byte of the section, sharing the chunk memory. Nothing writes to
-- the section afterwards.
freezeSection :: Section s -> ST s BL.ByteString
freezeSection section = do
  size <- sectionSize section
  count <- readPrimArray (sectionCounters section) counterChunks
  pieces <-
    mapM
      ( \index -> do
          ByteArray frozen <- readColumn (sectionChunks section) index >>= unsafeFreezeByteArray
          let width = if index == count - 1 then size - index * chunkBytes else chunkBytes
          -- The chunk is pinned, so this shares it rather than copying.
          pure (BS.take width (fromShort (SBS frozen)))
      )
      [0 .. count - 1]
  pure (BL.fromChunks pieces)

-- | A fixup row, read back for resolution.
data FixupRow = FixupRow
  { fixupRowOffset :: !Int,
    fixupRowAddend :: !Int64,
    fixupRowTarget :: !Int,
    fixupRowKind :: !FixupKind
  }

readFixup :: Section s -> Int -> ST s FixupRow
readFixup section row =
  FixupRow
    <$> readColumn (fixupOffsets section) row
    <*> readColumn (fixupAddends section) row
    <*> readColumn (fixupTargets section) row
    <*> (toEnum . fromIntegral <$> readColumn (fixupKinds section) row)

writeFixup :: Section s -> Int -> FixupRow -> ST s ()
writeFixup section row fixup = do
  writeColumn (fixupOffsets section) row (fixupRowOffset fixup)
  writeColumn (fixupAddends section) row (fixupRowAddend fixup)
  writeColumn (fixupTargets section) row (fixupRowTarget fixup)
  writeColumn (fixupKinds section) row (fromIntegral (fromEnum (fixupRowKind fixup)))

-- Objects

-- | An object under construction.
data Object s = Object
  { objectCurrent :: !(MutVar s (Maybe (Section s))),
    objectSections :: !(MutVar s (Map SectionRole (Section s))),
    -- | The sections in the order they were first selected, latest first.
    objectOrder :: !(MutVar s [SectionRole]),
    objectSymbolIds :: !(MutVar s (Map Text Int)),
    -- | The text of every symbol, latest first.
    objectSymbolNames :: !(MutVar s [Text]),
    objectSymbolCount :: !(MutablePrimArray s Int),
    objectSymbolOffsets :: !(Unboxed s Int),
    -- | The section a symbol is defined in, or -1.
    objectSymbolSections :: !(Unboxed s Int8),
    objectSymbolGlobals :: !(Unboxed s Bool),
    -- | Private labels, by number.
    objectLocalOffsets :: !(Unboxed s Int),
    objectLocalSections :: !(Unboxed s Int8)
  }

newObject :: ST s (Object s)
newObject = do
  count <- newPrimArray 1
  writePrimArray count 0 0
  Object
    <$> newMutVar Nothing
    <*> newMutVar Map.empty
    <*> newMutVar []
    <*> newMutVar Map.empty
    <*> newMutVar []
    <*> pure count
    <*> newColumn 0 256
    <*> newColumn undefinedSection 256
    <*> newColumn False 256
    <*> newColumn 0 256
    <*> newColumn undefinedSection 256

-- | Build an object, lay it out, and encode it. The object exists only
-- inside the call, so the result is a pure function of the builder.
assembleObject :: (ObjectError -> err) -> (Image -> Either ObjectError BL.ByteString) -> (forall s. Object s -> ST s (Either err ())) -> Either err BL.ByteString
assembleObject wrap encode build = runST $ do
  object <- newObject
  result <- build object
  case result of
    Left err -> pure (Left err)
    Right () -> either (Left . wrap) (either (Left . wrap) Right . encode) <$> layoutObject object

currentSectionRole :: Object s -> ST s (Maybe SectionRole)
currentSectionRole object = fmap sectionRole <$> readMutVar (objectCurrent object)

selectSection :: SectionRole -> Object s -> ST s ()
selectSection role object = do
  current <- readMutVar (objectCurrent object)
  case current of
    Just section | sectionRole section == role -> pure ()
    _ -> do
      sections <- readMutVar (objectSections object)
      section <- case Map.lookup role sections of
        Just section -> pure section
        Nothing -> do
          section <- newSection role
          writeMutVar (objectSections object) (Map.insert role section sections)
          modifyMutVar' (objectOrder object) (role :)
          pure section
      writeMutVar (objectCurrent object) (Just section)

-- | The number of a symbol, assigned on first sight.
internSymbol :: Object s -> Text -> ST s Int
internSymbol object name = do
  ids <- readMutVar (objectSymbolIds object)
  case Map.lookup name ids of
    Just identifier -> pure identifier
    Nothing -> do
      identifier <- readPrimArray (objectSymbolCount object) 0
      writePrimArray (objectSymbolCount object) 0 (identifier + 1)
      writeColumn (objectSymbolSections object) identifier undefinedSection
      writeMutVar (objectSymbolIds object) (Map.insert name identifier ids)
      modifyMutVar' (objectSymbolNames object) (name :)
      pure identifier

addGlobal :: Text -> Object s -> ST s ()
addGlobal name object = do
  identifier <- internSymbol object name
  writeColumn (objectSymbolGlobals object) identifier True

localText :: Int -> Text
localText identifier = ".L" <> T.pack (show identifier)

defineLabel :: Object s -> Section s -> Name -> ST s (Either ObjectError ())
defineLabel object section name = do
  size <- sectionSize section
  let role = fromIntegral (fromEnum (sectionRole section)) :: Int8
  case name of
    SymbolName text -> do
      identifier <- internSymbol object text
      defined <- readColumn (objectSymbolSections object) identifier
      if defined /= undefinedSection
        then pure (Left (ObjectDuplicateSymbol text))
        else do
          writeColumn (objectSymbolOffsets object) identifier size
          writeColumn (objectSymbolSections object) identifier role
          pure ok
    LocalName identifier _ -> do
      defined <- readColumn (objectLocalSections object) identifier
      if defined /= undefinedSection
        then pure (Left (ObjectDuplicateSymbol (localText identifier)))
        else do
          writeColumn (objectLocalOffsets object) identifier size
          writeColumn (objectLocalSections object) identifier role
          pure ok

-- | Append one item to the current section.
emitItem :: Object s -> Item -> ST s (Either ObjectError ())
emitItem object item = do
  current <- readMutVar (objectCurrent object)
  case current of
    Nothing -> pure (Left ObjectNoSection)
    Just section ->
      case item of
        Word width value -> emitWord section width value >> pure ok
        Bytes value -> emitBytes section value >> pure ok
        Label name -> defineLabel object section name
        Apply fixup -> do
          size <- sectionSize section
          target <- case fixupTarget fixup of
            SymbolName text -> internSymbol object text
            LocalName identifier _ -> pure (-1 - identifier)
          emitWord section (fixupWidth fixup) (fixupWord fixup)
          row <- readPrimArray (sectionCounters section) counterFixups
          writePrimArray (sectionCounters section) counterFixups (row + 1)
          writeFixup section row (FixupRow size (fixupAddend fixup) target (fixupKind fixup))
          pure ok

-- | Append a run of items to the current section.
emitItems :: Object s -> [Item] -> ST s (Either ObjectError ())
emitItems object = applyAll (emitItem object)

-- | Pad the current section to a power-of-two boundary with copies of the
-- fill, and record the alignment.
emitAlign :: Object s -> Int -> ByteString -> ST s (Either ObjectError ())
emitAlign object alignmentPower fill = do
  current <- readMutVar (objectCurrent object)
  case current of
    Nothing -> pure (Left ObjectNoSection)
    Just section
      | alignmentPower < 0 || alignmentPower > 30 -> pure (Left (ObjectInvalidAlignment alignmentPower))
      | BS.null fill -> pure (Left (ObjectInvalidInput "empty alignment fill"))
      | otherwise -> do
          size <- sectionSize section
          alignment <- readPrimArray (sectionCounters section) counterAlignment
          writePrimArray (sectionCounters section) counterAlignment (max alignment alignmentPower)
          let boundary = 1 `shiftL` alignmentPower
              padding = (boundary - size `mod` boundary) `mod` boundary
              -- A block of whole fills, bounded so that a large alignment
              -- pads in pieces.
              block = BS.concat (replicate (max 1 (min padding 4096 `div` BS.length fill)) fill)
              go remaining =
                when (remaining > 0) $ do
                  let count = min remaining (BS.length block)
                  emitBytes section (BS.take count block)
                  go (remaining - count)
          go padding
          pure ok

-- | Resolve the fixups of the function just emitted that name its private
-- labels, patching the section in place, and keep the rest for layout.
sealFunction :: Object s -> ST s (Either ObjectError ())
sealFunction object = do
  current <- readMutVar (objectCurrent object)
  case current of
    Nothing -> pure ok
    Just section -> do
      sealed <- readPrimArray (sectionCounters section) counterSealed
      count <- readPrimArray (sectionCounters section) counterFixups
      let go source target
            | source == count = do
                writePrimArray (sectionCounters section) counterFixups target
                writePrimArray (sectionCounters section) counterSealed target
                pure ok
            | otherwise = do
                fixup <- readFixup section source
                if fixupRowTarget fixup < 0
                  then do
                    result <- resolveLocal object section fixup
                    case result of
                      Left err -> pure (Left err)
                      Right () -> go (source + 1) target
                  else do
                    when (source /= target) (writeFixup section target fixup)
                    go (source + 1) (target + 1)
      go sealed sealed

-- | Patch a fixup that names a private label of this section.
resolveLocal :: Object s -> Section s -> FixupRow -> ST s (Either ObjectError ())
resolveLocal object section fixup = do
  let identifier = -1 - fixupRowTarget fixup
  defined <- readColumn (objectLocalSections object) identifier
  if defined == undefinedSection
    then pure (Left (ObjectMissingSymbol (localText identifier)))
    else
      if fromIntegral defined /= fromEnum (sectionRole section) || not (canResolve (fixupRowKind fixup))
        then pure (Left (ObjectInvalidFixup (fixupRowKind fixup)))
        else do
          target <- readColumn (objectLocalOffsets object) identifier
          patchLocal section (localText identifier) target fixup

-- | Whether this object can fill a fixup in without the linker.
canResolve :: FixupKind -> Bool
canResolve kind =
  case kind of
    Arm64Branch26 -> True
    Arm64Branch19 -> True
    Arm64Adr21 -> True
    X86Pc32 -> True
    X86Plt32 -> True
    _ -> False

-- | Fill a fixup in with the displacement to a target in the same section.
patchLocal :: Section s -> Text -> Int -> FixupRow -> ST s (Either ObjectError ())
patchLocal section name target fixup = do
  word <- readWordAt section offset 4
  let instruction = fromIntegral word :: Word32
  case fixupRowKind fixup of
    Arm64Branch26
      | displacement `mod` 4 /= 0 || not (fitsSigned 28 displacement) -> outOfRange
      | otherwise -> write (instruction .|. fromIntegral ((displacement `shiftR` 2) .&. 0x03ffffff))
    Arm64Branch19
      | displacement `mod` 4 /= 0 || not (fitsSigned 21 displacement) -> outOfRange
      | otherwise -> write (instruction .|. fromIntegral (((displacement `shiftR` 2) .&. 0x7ffff) `shiftL` 5))
    Arm64Adr21
      | not (fitsSigned 21 displacement) -> outOfRange
      | otherwise ->
          let immediate = displacement .&. 0x1fffff
              low = fromIntegral ((immediate .&. 3) `shiftL` 29)
              high = fromIntegral (((immediate `shiftR` 2) .&. 0x7ffff) `shiftL` 5)
           in write (instruction .|. low .|. high)
    X86Pc32 -> patchX86
    X86Plt32 -> patchX86
    kind -> pure (Left (ObjectInvalidFixup kind))
  where
    offset = fixupRowOffset fixup
    displacement = fromIntegral target - fromIntegral offset + fixupRowAddend fixup :: Int64
    outOfRange = pure (Left (ObjectDisplacementOutOfRange name))
    write value = writeWordAt section offset 4 (fromIntegral (value :: Word32)) >> pure ok
    patchX86
      | fitsSigned 32 displacement = write (fromIntegral displacement)
      | otherwise = outOfRange

fitsSigned :: Int -> Int64 -> Bool
fitsSigned bits value = value >= negate (1 `shiftL` (bits - 1)) && value < (1 `shiftL` (bits - 1))

-- Layout

-- | Resolve every fixup left, patching the ones this object can fill in and
-- turning the rest into relocations, and choose the symbols. Only a name
-- that the linker needs becomes a symbol: a global one, or one that a
-- relocation names. A label that this object resolves on its own, such as a
-- branch target inside one function, needs no symbol. A name that this
-- object defines and does not export is read by nothing: only the
-- relocations beside it name it, and they name it by position. The text it
-- was given upstream is dead weight, and generated code has one such name
-- per entry function, per info table, and per enter stub, which is most of
-- the string table of a library object. Those are numbered instead. An
-- exported or undefined name keeps its text, because that is what another
-- object matches against.
layoutObject :: Object s -> ST s (Either ObjectError Image)
layoutObject object = do
  order <- reverse <$> readMutVar (objectOrder object)
  sections <- readMutVar (objectSections object)
  symbolCount <- readPrimArray (objectSymbolCount object) 0
  names <- listArray (0, symbolCount - 1) . reverse <$> readMutVar (objectSymbolNames object)
  relocated <- MU.replicate symbolCount False
  resolved <- resolveSections object (names !) relocated [sections Map.! role | role <- order]
  case resolved of
    Left err -> pure (Left err)
    Right sectionRelocations -> do
      ids <- Map.toAscList <$> readMutVar (objectSymbolIds object)
      candidates <- mapM (\named@(_, identifier) -> (,) named <$> readSymbol identifier) ids
      needed <- filterM (\((_, identifier), row) -> if symbolRowGlobal row then pure True else MU.unsafeRead relocated identifier) candidates
      let defined row = symbolRowSection row /= undefinedSection
          privateLabels =
            IntMap.fromList
              ( zip
                  [identifier | ((_, identifier), row) <- needed, not (symbolRowGlobal row), defined row]
                  [".L" <> T.pack (show index) | index <- [0 :: Int ..]]
              )
          emitted (name, identifier) = IntMap.findWithDefault name identifier privateLabels
          ordered = sortOn fst [(emitted named, (named, row)) | (named, row) <- needed]
          symbols =
            [ if defined row
                then Symbol label (symbolRowGlobal row) (Just (toEnum (fromIntegral (symbolRowSection row)))) (fromIntegral (symbolRowOffset row))
                else Symbol label True Nothing 0
            | (label, (_, row)) <- ordered
            ]
      positions <- MU.replicate symbolCount (-1 :: Int)
      mapM_ (\(position, (_, ((_, identifier), _))) -> MU.unsafeWrite positions identifier position) (zip [0 ..] ordered)
      imageSections <-
        mapM
          ( \(section, relocations) -> do
              size <- sectionSize section
              alignment <- readPrimArray (sectionCounters section) counterAlignment
              bytes <- freezeSection section
              placed <- mapM (\(offset, kind, identifier, addend) -> (\position -> Relocation offset kind position addend) <$> MU.unsafeRead positions identifier) relocations
              pure
                ImageSection
                  { imageSectionRole = sectionRole section,
                    imageSectionAlignment = alignment,
                    imageSectionSize = fromIntegral size,
                    imageSectionBytes = bytes,
                    imageSectionRelocations = placed
                  }
          )
          sectionRelocations
      pure (Right Image {imageSections = imageSections, imageSymbols = symbols})
  where
    readSymbol identifier =
      SymbolRow
        <$> readColumn (objectSymbolOffsets object) identifier
        <*> readColumn (objectSymbolSections object) identifier
        <*> readColumn (objectSymbolGlobals object) identifier

-- | A symbol row, read back for layout.
data SymbolRow = SymbolRow
  { symbolRowOffset :: !Int,
    symbolRowSection :: !Int8,
    symbolRowGlobal :: !Bool
  }

-- | The relocations of every section, in offset order, after patching the
-- fixups the object resolves itself.
resolveSections :: Object s -> (Int -> Text) -> MU.MVector s Bool -> [Section s] -> ST s (Either ObjectError [(Section s, [(Word64, FixupKind, Int, Int64)])])
resolveSections object nameOf relocated = go []
  where
    go done sections =
      case sections of
        [] -> pure (Right (reverse done))
        section : rest -> do
          count <- readPrimArray (sectionCounters section) counterFixups
          result <- resolveFrom section 0 count []
          case result of
            Left err -> pure (Left err)
            Right relocations -> go ((section, relocations) : done) rest
    resolveFrom section index count relocations
      | index == count = pure (Right (reverse relocations))
      | otherwise = do
          fixup <- readFixup section index
          result <-
            if fixupRowTarget fixup < 0
              then fmap (const Nothing) <$> resolveLocal object section fixup
              else do
                let identifier = fixupRowTarget fixup
                definedIn <- readColumn (objectSymbolSections object) identifier
                global <- readColumn (objectSymbolGlobals object) identifier
                let sameSection = fromIntegral definedIn == fromEnum (sectionRole section)
                if canResolve (fixupRowKind fixup) && not global && sameSection
                  then do
                    target <- readColumn (objectSymbolOffsets object) identifier
                    fmap (const Nothing) <$> patchLocal section (nameOf identifier) target fixup
                  else do
                    MU.unsafeWrite relocated identifier True
                    pure (Right (Just (fromIntegral (fixupRowOffset fixup), fixupRowKind fixup, identifier, fixupRowAddend fixup)))
          case result of
            Left err -> pure (Left err)
            Right Nothing -> resolveFrom section (index + 1) count relocations
            Right (Just relocation) -> resolveFrom section (index + 1) count (relocation : relocations)
