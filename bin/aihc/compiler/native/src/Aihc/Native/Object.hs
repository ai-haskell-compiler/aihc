{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Direct native object generation into mutable section buffers.
--
-- An 'Object' owns one buffer per section: a run of 64 KB pinned chunks
-- that the assembler writes machine words into as it walks the instruction
-- stream, together with unboxed tables of the labels it defined and the
-- fixups it still owes. Nothing is allocated per instruction beyond the
-- bytes themselves: a word is a poke, a label is a table write, and a fixup
-- is a table row. Symbols are interned to numbers as they arrive, so the
-- layout that turns the buffers into an 'Image' indexes arrays rather than
-- comparing names, and a fixup this object resolves on its own is patched
-- into the buffer in place rather than copied through a patch list.
module Aihc.Native.Object
  ( Object,
    newObject,
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
import Data.Array (listArray, (!))
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOArray, IOUArray, newArray, newArray_)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64, Int8)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.ForeignPtr (unsafeWithForeignPtr)

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

-- Tables

-- | A growable array of fixed-size unboxed rows.
data Table = Table
  { tableStride :: !Int,
    tableBuffer :: !(IORef (ForeignPtr Word8)),
    -- | The row count, then the capacity in rows.
    tableMeta :: !(IOUArray Int Int)
  }

newTable :: Int -> Int -> IO Table
newTable stride capacity = do
  buffer <- mallocForeignPtrBytes (stride * capacity)
  meta <- newArray (0, 1) 0
  unsafeWrite meta 1 capacity
  Table stride <$> newIORef buffer <*> pure meta

tableLength :: Table -> IO Int
tableLength table = unsafeRead (tableMeta table) 0

setTableLength :: Table -> Int -> IO ()
setTableLength table = unsafeWrite (tableMeta table) 0

-- | Make room for the given number of rows. New rows hold garbage.
tableReserve :: Table -> Int -> IO ()
tableReserve table count = do
  capacity <- unsafeRead (tableMeta table) 1
  when (count > capacity) $ do
    let capacity' = max count (2 * capacity)
    old <- readIORef (tableBuffer table)
    new <- mallocForeignPtrBytes (tableStride table * capacity')
    withForeignPtr old $ \source -> withForeignPtr new $ \target -> copyBytes target source (tableStride table * capacity)
    writeIORef (tableBuffer table) new
    unsafeWrite (tableMeta table) 1 capacity'

-- | Add a row and return its index.
tablePush :: Table -> IO Int
tablePush table = do
  count <- tableLength table
  tableReserve table (count + 1)
  setTableLength table (count + 1)
  pure count

-- | Run an action on the bytes of a row.
tableAt :: Table -> Int -> (Ptr Word8 -> IO value) -> IO value
tableAt table row action = do
  buffer <- readIORef (tableBuffer table)
  unsafeWithForeignPtr buffer (\pointer -> action (pointer `plusPtr` (row * tableStride table)))
{-# INLINE tableAt #-}

-- Symbol rows: the offset at 0, the section at 8 (-1 when undefined), and
-- whether the symbol is global at 9.
symbolStride :: Int
symbolStride = 16

-- Local rows: the offset at 0 and the section at 8 (-1 when undefined).
localStride :: Int
localStride = 16

-- Fixup rows: the offset at 0, the addend at 8, the target at 16 (a symbol
-- number, or @-1 - local@ for a private label), and the kind at 20.
fixupStride :: Int
fixupStride = 24

undefinedSection :: Int8
undefinedSection = -1

-- Sections

chunkBytes :: Int
chunkBytes = 65536

chunkShift :: Int
chunkShift = 16

chunkMask :: Int
chunkMask = chunkBytes - 1

-- | The bytes of one section and the tables recorded over them.
data Section = Section
  { sectionRole :: !SectionRole,
    -- | The size, the alignment power, the chunk count, the number of
    -- leading fixups that 'sealFunction' has already examined, and the
    -- capacity of the chunk array.
    sectionCounters :: !(IOUArray Int Int),
    sectionChunks :: !(IORef (IOArray Int (ForeignPtr Word8))),
    sectionFixups :: !Table
  }

counterSize, counterAlignment, counterChunks, counterSealed, counterCapacity :: Int
counterSize = 0
counterAlignment = 1
counterChunks = 2
counterSealed = 3
counterCapacity = 4

newSection :: SectionRole -> IO Section
newSection role = do
  counters <- newArray (0, 4) 0
  unsafeWrite counters counterCapacity 4
  chunks <- newArray_ (0, 3) >>= newIORef
  Section role counters chunks <$> newTable fixupStride 64

sectionSize :: Section -> IO Int
sectionSize section = unsafeRead (sectionCounters section) counterSize

-- | The chunk with the given index, allocated when it is the next one.
chunkAt :: Section -> Int -> IO (ForeignPtr Word8)
chunkAt section index = do
  count <- unsafeRead (sectionCounters section) counterChunks
  chunks <- readIORef (sectionChunks section)
  if index < count
    then unsafeRead chunks index
    else do
      chunk <- mallocForeignPtrBytes chunkBytes
      capacity <- unsafeRead (sectionCounters section) counterCapacity
      grown <-
        if count < capacity
          then pure chunks
          else do
            larger <- newArray_ (0, 2 * capacity - 1)
            mapM_ (\position -> unsafeRead chunks position >>= unsafeWrite larger position) [0 .. count - 1]
            writeIORef (sectionChunks section) larger
            unsafeWrite (sectionCounters section) counterCapacity (2 * capacity)
            pure larger
      unsafeWrite grown count chunk
      unsafeWrite (sectionCounters section) counterChunks (count + 1)
      pure chunk

pokeLittleEndian :: Ptr Word8 -> Int -> Word64 -> IO ()
pokeLittleEndian pointer width value =
  case width of
    4 | targetByteOrder == LittleEndian -> pokeByteOff pointer 0 (fromIntegral value :: Word32)
    8 | targetByteOrder == LittleEndian -> pokeByteOff pointer 0 value
    _ -> go 0
  where
    go index =
      when (index < width) $ do
        pokeByteOff pointer index (fromIntegral (value `shiftR` (8 * index)) :: Word8)
        go (index + 1)

peekLittleEndian :: Ptr Word8 -> Int -> IO Word64
peekLittleEndian pointer width =
  case width of
    4 | targetByteOrder == LittleEndian -> fromIntegral <$> (peekByteOff pointer 0 :: IO Word32)
    8 | targetByteOrder == LittleEndian -> peekByteOff pointer 0
    _ -> go 0 0
  where
    go index !value
      | index < width = do
          byte <- peekByteOff pointer index :: IO Word8
          go (index + 1) (value .|. (fromIntegral byte `shiftL` (8 * index)))
      | otherwise = pure value

-- | Append one byte.
emitByte :: Section -> Word8 -> IO ()
emitByte section value = do
  size <- sectionSize section
  chunk <- chunkAt section (size `shiftR` chunkShift)
  unsafeWithForeignPtr chunk $ \pointer -> pokeByteOff pointer (size .&. chunkMask) value
  unsafeWrite (sectionCounters section) counterSize (size + 1)

-- | Append a little-endian word.
emitWord :: Section -> Int -> Word64 -> IO ()
emitWord section width value = do
  size <- sectionSize section
  let within = size .&. chunkMask
  if within + width <= chunkBytes
    then do
      chunk <- chunkAt section (size `shiftR` chunkShift)
      unsafeWithForeignPtr chunk $ \pointer -> pokeLittleEndian (pointer `plusPtr` within) width value
      unsafeWrite (sectionCounters section) counterSize (size + width)
    else mapM_ (\index -> emitByte section (fromIntegral (value `shiftR` (8 * index)))) [0 .. width - 1]

-- | Append bytes.
emitBytes :: Section -> ByteString -> IO ()
emitBytes section value =
  BSU.unsafeUseAsCStringLen value $ \(source, total) ->
    let go done =
          when (done < total) $ do
            size <- sectionSize section
            let within = size .&. chunkMask
                count = min (total - done) (chunkBytes - within)
            chunk <- chunkAt section (size `shiftR` chunkShift)
            unsafeWithForeignPtr chunk $ \pointer -> copyBytes (pointer `plusPtr` within) (castPtr source `plusPtr` done) count
            unsafeWrite (sectionCounters section) counterSize (size + count)
            go (done + count)
     in go 0

-- | Read a little-endian word at an offset.
readWordAt :: Section -> Int -> Int -> IO Word64
readWordAt section offset width = do
  let within = offset .&. chunkMask
  if within + width <= chunkBytes
    then do
      chunk <- chunkAt section (offset `shiftR` chunkShift)
      unsafeWithForeignPtr chunk $ \pointer -> peekLittleEndian (pointer `plusPtr` within) width
    else do
      let byteAt index = do
            chunk <- chunkAt section ((offset + index) `shiftR` chunkShift)
            unsafeWithForeignPtr chunk $ \pointer -> peekByteOff pointer ((offset + index) .&. chunkMask) :: IO Word8
      bytes <- mapM byteAt [0 .. width - 1]
      pure (foldr (\byte value -> (value `shiftL` 8) .|. fromIntegral byte) 0 bytes)

-- | Write a little-endian word at an offset.
writeWordAt :: Section -> Int -> Int -> Word64 -> IO ()
writeWordAt section offset width value = do
  let within = offset .&. chunkMask
  if within + width <= chunkBytes
    then do
      chunk <- chunkAt section (offset `shiftR` chunkShift)
      unsafeWithForeignPtr chunk $ \pointer -> pokeLittleEndian (pointer `plusPtr` within) width value
    else
      mapM_
        ( \index -> do
            chunk <- chunkAt section ((offset + index) `shiftR` chunkShift)
            unsafeWithForeignPtr chunk $ \pointer -> pokeByteOff pointer ((offset + index) .&. chunkMask) (fromIntegral (value `shiftR` (8 * index)) :: Word8)
        )
        [0 .. width - 1]

-- | Every byte of the section, sharing the chunk memory.
sectionBytes :: Section -> IO BL.ByteString
sectionBytes section = do
  size <- sectionSize section
  count <- unsafeRead (sectionCounters section) counterChunks
  chunks <- readIORef (sectionChunks section)
  pieces <-
    mapM
      ( \index -> do
          chunk <- unsafeRead chunks index
          let width = if index == count - 1 then size - index * chunkBytes else chunkBytes
          pure (BSI.fromForeignPtr chunk 0 width)
      )
      [0 .. count - 1]
  pure (BL.fromChunks pieces)

-- Objects

-- | An object under construction.
data Object = Object
  { objectCurrent :: !(IORef (Maybe Section)),
    objectSections :: !(IORef (Map SectionRole Section)),
    -- | The sections in the order they were first selected, latest first.
    objectOrder :: !(IORef [SectionRole]),
    objectSymbolIds :: !(IORef (Map Text Int)),
    -- | The text of every symbol, latest first.
    objectSymbolNames :: !(IORef [Text]),
    objectSymbols :: !Table,
    -- | Private labels, by number.
    objectLocals :: !Table
  }

newObject :: IO Object
newObject =
  Object
    <$> newIORef Nothing
    <*> newIORef Map.empty
    <*> newIORef []
    <*> newIORef Map.empty
    <*> newIORef []
    <*> newTable symbolStride 256
    <*> newTable localStride 256

currentSectionRole :: Object -> IO (Maybe SectionRole)
currentSectionRole object = fmap sectionRole <$> readIORef (objectCurrent object)

selectSection :: SectionRole -> Object -> IO ()
selectSection role object = do
  current <- readIORef (objectCurrent object)
  case current of
    Just section | sectionRole section == role -> pure ()
    _ -> do
      sections <- readIORef (objectSections object)
      section <- case Map.lookup role sections of
        Just section -> pure section
        Nothing -> do
          section <- newSection role
          writeIORef (objectSections object) (Map.insert role section sections)
          modifyIORef' (objectOrder object) (role :)
          pure section
      writeIORef (objectCurrent object) (Just section)

-- | The number of a symbol, assigned on first sight.
internSymbol :: Object -> Text -> IO Int
internSymbol object name = do
  ids <- readIORef (objectSymbolIds object)
  case Map.lookup name ids of
    Just identifier -> pure identifier
    Nothing -> do
      identifier <- tablePush (objectSymbols object)
      tableAt (objectSymbols object) identifier $ \row -> do
        pokeByteOff row 0 (0 :: Word64)
        pokeByteOff row 8 undefinedSection
        pokeByteOff row 9 (0 :: Word8)
      writeIORef (objectSymbolIds object) (Map.insert name identifier ids)
      modifyIORef' (objectSymbolNames object) (name :)
      pure identifier

addGlobal :: Text -> Object -> IO ()
addGlobal name object = do
  identifier <- internSymbol object name
  tableAt (objectSymbols object) identifier $ \row -> pokeByteOff row 9 (1 :: Word8)

-- | Make sure the local table reaches the given number, marking new rows
-- undefined.
reserveLocal :: Object -> Int -> IO ()
reserveLocal object identifier = do
  count <- tableLength (objectLocals object)
  when (identifier >= count) $ do
    tableReserve (objectLocals object) (identifier + 1)
    mapM_ (\row -> tableAt (objectLocals object) row (\pointer -> pokeByteOff pointer 8 undefinedSection)) [count .. identifier]
    setTableLength (objectLocals object) (identifier + 1)

localText :: Int -> Text
localText identifier = ".L" <> T.pack (show identifier)

defineLabel :: Object -> Section -> Name -> IO (Either ObjectError ())
defineLabel object section name = do
  size <- sectionSize section
  case name of
    SymbolName text -> do
      identifier <- internSymbol object text
      tableAt (objectSymbols object) identifier $ \row -> do
        defined <- peekByteOff row 8 :: IO Int8
        if defined /= undefinedSection
          then pure (Left (ObjectDuplicateSymbol text))
          else do
            pokeByteOff row 0 (fromIntegral size :: Word64)
            pokeByteOff row 8 (fromIntegral (fromEnum (sectionRole section)) :: Int8)
            pure ok
    LocalName identifier _ -> do
      reserveLocal object identifier
      tableAt (objectLocals object) identifier $ \row -> do
        defined <- peekByteOff row 8 :: IO Int8
        if defined /= undefinedSection
          then pure (Left (ObjectDuplicateSymbol (localText identifier)))
          else do
            pokeByteOff row 0 (fromIntegral size :: Word64)
            pokeByteOff row 8 (fromIntegral (fromEnum (sectionRole section)) :: Int8)
            pure ok

-- | Append one item to the current section.
emitItem :: Object -> Item -> IO (Either ObjectError ())
emitItem object item = do
  current <- readIORef (objectCurrent object)
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
          row <- tablePush (sectionFixups section)
          tableAt (sectionFixups section) row $ \pointer -> do
            pokeByteOff pointer 0 (fromIntegral size :: Word64)
            pokeByteOff pointer 8 (fixupAddend fixup)
            pokeByteOff pointer 16 (fromIntegral target :: Int32)
            pokeByteOff pointer 20 (fromIntegral (fromEnum (fixupKind fixup)) :: Word8)
          pure ok

-- | Append a run of items to the current section.
emitItems :: Object -> [Item] -> IO (Either ObjectError ())
emitItems object = go
  where
    go items =
      case items of
        [] -> pure ok
        item : rest -> do
          result <- emitItem object item
          case result of
            Left err -> pure (Left err)
            Right () -> go rest

-- | Pad the current section to a power-of-two boundary with copies of the
-- fill, and record the alignment.
emitAlign :: Object -> Int -> ByteString -> IO (Either ObjectError ())
emitAlign object alignmentPower fill = do
  current <- readIORef (objectCurrent object)
  case current of
    Nothing -> pure (Left ObjectNoSection)
    Just section
      | alignmentPower < 0 || alignmentPower > 30 -> pure (Left (ObjectInvalidAlignment alignmentPower))
      | BS.null fill -> pure (Left (ObjectInvalidInput "empty alignment fill"))
      | otherwise -> do
          size <- sectionSize section
          alignment <- unsafeRead (sectionCounters section) counterAlignment
          unsafeWrite (sectionCounters section) counterAlignment (max alignment alignmentPower)
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
sealFunction :: Object -> IO (Either ObjectError ())
sealFunction object = do
  current <- readIORef (objectCurrent object)
  case current of
    Nothing -> pure ok
    Just section -> do
      sealed <- unsafeRead (sectionCounters section) counterSealed
      count <- tableLength (sectionFixups section)
      let go source target
            | source == count = do
                setTableLength (sectionFixups section) target
                unsafeWrite (sectionCounters section) counterSealed target
                pure ok
            | otherwise = do
                fixup <- readFixup (sectionFixups section) source
                if fixupRowTarget fixup < 0
                  then do
                    result <- resolveLocal object section fixup
                    case result of
                      Left err -> pure (Left err)
                      Right () -> go (source + 1) target
                  else do
                    when (source /= target) (writeFixup (sectionFixups section) target fixup)
                    go (source + 1) (target + 1)
      go sealed sealed

-- | A fixup row, read back for resolution.
data FixupRow = FixupRow
  { fixupRowOffset :: !Int,
    fixupRowAddend :: !Int64,
    fixupRowTarget :: !Int,
    fixupRowKind :: !FixupKind
  }

readFixup :: Table -> Int -> IO FixupRow
readFixup table row =
  tableAt table row $ \pointer -> do
    offset <- peekByteOff pointer 0 :: IO Word64
    addend <- peekByteOff pointer 8 :: IO Int64
    target <- peekByteOff pointer 16 :: IO Int32
    kind <- peekByteOff pointer 20 :: IO Word8
    pure (FixupRow (fromIntegral offset) addend (fromIntegral target) (toEnum (fromIntegral kind)))

writeFixup :: Table -> Int -> FixupRow -> IO ()
writeFixup table row fixup =
  tableAt table row $ \pointer -> do
    pokeByteOff pointer 0 (fromIntegral (fixupRowOffset fixup) :: Word64)
    pokeByteOff pointer 8 (fixupRowAddend fixup)
    pokeByteOff pointer 16 (fromIntegral (fixupRowTarget fixup) :: Int32)
    pokeByteOff pointer 20 (fromIntegral (fromEnum (fixupRowKind fixup)) :: Word8)

-- | Patch a fixup that names a private label of this section.
resolveLocal :: Object -> Section -> FixupRow -> IO (Either ObjectError ())
resolveLocal object section fixup = do
  let identifier = -1 - fixupRowTarget fixup
  count <- tableLength (objectLocals object)
  (defined, target) <-
    if identifier < count
      then tableAt (objectLocals object) identifier $ \row -> (,) <$> (peekByteOff row 8 :: IO Int8) <*> (peekByteOff row 0 :: IO Word64)
      else pure (undefinedSection, 0)
  if defined == undefinedSection
    then pure (Left (ObjectMissingSymbol (localText identifier)))
    else
      if fromIntegral defined /= fromEnum (sectionRole section) || not (canResolve (fixupRowKind fixup))
        then pure (Left (ObjectInvalidFixup (fixupRowKind fixup)))
        else patchLocal section (localText identifier) (fromIntegral target) fixup

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
patchLocal :: Section -> Text -> Int -> FixupRow -> IO (Either ObjectError ())
patchLocal section name target fixup = do
  instruction <- fromIntegral <$> readWordAt section offset 4 :: IO Word32
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

-- | A symbol row, read back for layout.
data SymbolRow = SymbolRow
  { symbolRowOffset :: !Word64,
    symbolRowSection :: !Int8,
    symbolRowGlobal :: !Bool
  }

readSymbol :: Table -> Int -> IO SymbolRow
readSymbol table row =
  tableAt table row $ \pointer -> do
    offset <- peekByteOff pointer 0 :: IO Word64
    section <- peekByteOff pointer 8 :: IO Int8
    global <- peekByteOff pointer 9 :: IO Word8
    pure (SymbolRow offset section (global /= 0))

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
layoutObject :: Object -> IO (Either ObjectError Image)
layoutObject object = do
  order <- reverse <$> readIORef (objectOrder object)
  sections <- readIORef (objectSections object)
  symbolCount <- tableLength (objectSymbols object)
  names <- listArray (0, symbolCount - 1) . reverse <$> readIORef (objectSymbolNames object)
  relocated <- newArray (0, max 0 (symbolCount - 1)) False :: IO (IOUArray Int Bool)
  resolved <- resolveSections object (names !) relocated [sections Map.! role | role <- order]
  case resolved of
    Left err -> pure (Left err)
    Right sectionRelocations -> do
      ids <- Map.toAscList <$> readIORef (objectSymbolIds object)
      rows <- mapM (\(_, identifier) -> readSymbol (objectSymbols object) identifier) ids
      let candidates = zip ids rows
      needed <- filterMaybe relocated candidates
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
                then Symbol label (symbolRowGlobal row) (Just (toEnum (fromIntegral (symbolRowSection row)))) (symbolRowOffset row)
                else Symbol label True Nothing 0
            | (label, (_, row)) <- ordered
            ]
      positions <- newArray (0, max 0 (symbolCount - 1)) (-1) :: IO (IOUArray Int Int)
      mapM_ (\(position, (_, ((_, identifier), _))) -> unsafeWrite positions identifier position) (zip [0 ..] ordered)
      imageSections <-
        mapM
          ( \(section, relocations) -> do
              size <- sectionSize section
              alignment <- unsafeRead (sectionCounters section) counterAlignment
              bytes <- sectionBytes section
              placed <- mapM (\(offset, kind, identifier, addend) -> (\position -> Relocation offset kind position addend) <$> unsafeRead positions identifier) relocations
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
    filterMaybe relocated =
      filterM
        ( \((_, identifier), row) ->
            if symbolRowGlobal row
              then pure True
              else unsafeRead relocated identifier
        )

-- | The relocations of every section, in offset order, after patching the
-- fixups the object resolves itself.
resolveSections :: Object -> (Int -> Text) -> IOUArray Int Bool -> [Section] -> IO (Either ObjectError [(Section, [(Word64, FixupKind, Int, Int64)])])
resolveSections object nameOf relocated = go []
  where
    go done sections =
      case sections of
        [] -> pure (Right (reverse done))
        section : rest -> do
          count <- tableLength (sectionFixups section)
          result <- resolveFrom section 0 count []
          case result of
            Left err -> pure (Left err)
            Right relocations -> go ((section, relocations) : done) rest
    resolveFrom section index count relocations
      | index == count = pure (Right (reverse relocations))
      | otherwise = do
          fixup <- readFixup (sectionFixups section) index
          result <-
            if fixupRowTarget fixup < 0
              then fmap (const Nothing) <$> resolveLocal object section fixup
              else do
                let identifier = fixupRowTarget fixup
                row <- readSymbol (objectSymbols object) identifier
                let sameSection = fromIntegral (symbolRowSection row) == fromEnum (sectionRole section)
                if canResolve (fixupRowKind fixup) && not (symbolRowGlobal row) && sameSection
                  then fmap (const Nothing) <$> patchLocal section (nameOf identifier) (fromIntegral (symbolRowOffset row)) fixup
                  else do
                    unsafeWrite relocated identifier True
                    pure (Right (Just (fromIntegral (fixupRowOffset fixup), fixupRowKind fixup, identifier, fixupRowAddend fixup)))
          case result of
            Left err -> pure (Left err)
            Right Nothing -> resolveFrom section (index + 1) count relocations
            Right (Just relocation) -> resolveFrom section (index + 1) count (relocation : relocations)
