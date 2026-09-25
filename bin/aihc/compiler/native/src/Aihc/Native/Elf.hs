{-# LANGUAGE OverloadedStrings #-}

-- | Write AMD64 ELF relocatable objects.
module Aihc.Native.Elf
  ( writeAmd64Elf,
  )
where

import Aihc.Native.Object
import Control.Monad (replicateM_, when)
import Data.Array (Array, listArray, (!))
import Data.Binary.Put
import Data.Bits (countTrailingZeros, shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word32, Word64)

writeAmd64Elf :: Image -> Either ObjectError BL.ByteString
writeAmd64Elf image = writeImage (imageMetadata image) (map imageSectionBytes (imageSections image))

-- | Each atom of the image, a defined symbol up to the next symbol of its
-- section, becomes one ELF section. A linker keeps or discards an object
-- one section at a time, so @--gc-sections@ can then remove a function
-- that nothing reaches. The pieces of one image section share its name.
--
-- A relocation that names a symbol private to the object names the section
-- symbol of the piece that holds it instead, with the distance into the
-- piece added to the addend. So the object writes no name that only this
-- object reads, and the symbol table holds only the section symbols and the
-- global names.
writeImage :: Image -> [BL.ByteString] -> Either ObjectError BL.ByteString
writeImage image payloads = do
  mapM_ validateRelocations (imageSections image)
  let symbols = listArray (0, length (imageSymbols image) - 1) (imageSymbols image) :: Array Int Symbol
      pieces = splitSections image payloads
      pieceIndexes = zip [1 :: Word32 ..] pieces
      -- The piece of a section role that holds an offset: the last one that
      -- starts at or before it.
      pieceStarts =
        Map.fromListWith
          Map.union
          [(pieceRole piece, Map.singleton (pieceStart piece) index) | (index, piece) <- pieceIndexes]
      pieceAt role offset = Map.lookupLE offset (Map.findWithDefault Map.empty role pieceStarts)
      -- A relocation target: a named symbol, or a piece and the distance into it.
      target relocation =
        let symbol = symbols ! relocationSymbol relocation
         in case symbolSection symbol of
              Just role
                | not (symbolGlobal symbol),
                  Just (start, index) <- pieceAt role (symbolOffset symbol) ->
                    Left (index, fromIntegral (symbolOffset symbol - start) :: Int64)
              _ -> Right (relocationSymbol relocation)
      relocationTargets =
        [ target relocation
        | piece <- pieces,
          relocation <- pieceRelocations piece
        ]
      sectionSymbols = Set.toAscList (Set.fromList [index | Left (index, _) <- relocationTargets])
      namedSymbols = [(index, symbol) | (index, symbol) <- zip [0 ..] (imageSymbols image), symbolGlobal symbol]
      -- Index zero is the null symbol, so the table starts at one.
      sectionSymbolIndexes = Map.fromList (zip sectionSymbols [1 :: Word32 ..])
      namedIndexes = IntMap.fromList (zip (map fst namedSymbols) [fromIntegral (1 + length sectionSymbols) ..])
      relocationEntry relocation =
        case target relocation of
          Left (index, distance) -> (sectionSymbolIndexes Map.! index, relocationAddend relocation + distance)
          Right index -> (namedIndexes IntMap.! index, relocationAddend relocation)
      symbolStrings = buildStringTable (map (symbolName . snd) namedSymbols)
      relocationSections =
        [ RelocationDescription
            { relocationName = ".rela" <> pieceName piece,
              relocationTargetIndex = index,
              relocationValues = [(relocationOffset relocation - pieceStart piece, relocationKind relocation, relocationEntry relocation) | relocation <- pieceRelocations piece]
            }
        | (index, piece) <- pieceIndexes,
          not (null (pieceRelocations piece))
        ]
      pieceCount = length pieces
      relocationCount = length relocationSections
      symbolTableIndex = fromIntegral (1 + pieceCount + relocationCount)
      stringTableIndex = symbolTableIndex + 1
      sectionStringTableIndex = stringTableIndex + 1
      sectionNames =
        map pieceName pieces
          <> map relocationName relocationSections
          <> [".symtab", ".strtab", ".shstrtab"]
      sectionStrings = buildStringTable sectionNames
      (baseEnd, placedPieces) = mapAccumL placePiece 64 pieces
      (relocationEnd, placedRelocations) = mapAccumL placeRelocationSection (alignUp 8 baseEnd) relocationSections
      symbolOffset' = alignUp 8 relocationEnd
      symbolSize = fromIntegral ((1 + length sectionSymbols + length namedSymbols) * 24)
      stringOffset = symbolOffset' + symbolSize
      sectionStringOffset = stringOffset + fromIntegral (BS.length (snd symbolStrings))
      sectionHeaderOffset = alignUp 8 (sectionStringOffset + fromIntegral (BS.length (snd sectionStrings)))
      sectionCount = 1 + length sectionNames
      pieceOf role offset = maybe (0, 0) (\(start, index) -> (index, offset - start)) (pieceAt role offset)
  when (sectionCount >= 0xff00) (Left (ObjectSizeOverflow "ELF section count"))
  pure . runPut $ do
    putHeader sectionHeaderOffset sectionCount sectionStringTableIndex
    _ <- putPieceContents 64 placedPieces
    putPadding (alignUp 8 baseEnd - baseEnd)
    _ <- putRelocationContents (alignUp 8 baseEnd) placedRelocations
    putPadding (symbolOffset' - relocationEnd)
    putNullSymbol
    mapM_ putSectionSymbol sectionSymbols
    mapM_ (putNamedSymbol pieceOf (fst symbolStrings) . snd) namedSymbols
    putByteString (snd symbolStrings)
    putByteString (snd sectionStrings)
    putPadding (sectionHeaderOffset - sectionStringOffset - fromIntegral (BS.length (snd sectionStrings)))
    putNullSectionHeader
    mapM_ (putPieceSectionHeader (fst sectionStrings)) placedPieces
    mapM_ (putRelocationSectionHeader (fst sectionStrings) symbolTableIndex) placedRelocations
    putTableSectionHeader (fst sectionStrings Map.! ".symtab") 2 symbolOffset' symbolSize stringTableIndex (fromIntegral (1 + length sectionSymbols)) 8 24
    putTableSectionHeader (fst sectionStrings Map.! ".strtab") 3 stringOffset (fromIntegral (BS.length (snd symbolStrings))) 0 0 1 0
    putTableSectionHeader (fst sectionStrings Map.! ".shstrtab") 3 sectionStringOffset (fromIntegral (BS.length (snd sectionStrings))) 0 0 1 0

-- | One atom, or a run of atoms, of an image section.
data Piece = Piece
  { pieceRole :: !SectionRole,
    pieceName :: !Text,
    pieceType :: !Word32,
    pieceFlags :: !Word64,
    -- | The alignment power.
    pieceAlignment :: !Int,
    pieceStart :: !Word64,
    pieceSize :: !Word64,
    pieceBytes :: !BL.ByteString,
    -- | The relocations inside the piece, at their offsets in the image
    -- section.
    pieceRelocations :: ![Relocation]
  }

-- | The most sections of one kind that an object holds. The pieces and
-- their relocation sections must stay below the 0xff00 section indexes
-- that ELF gives without the extended numbering. Past this count,
-- neighbouring atoms share one piece.
maximumPieces :: Int
maximumPieces = 16000

-- | Divide each image section at the offsets of its defined symbols.
splitSections :: Image -> [BL.ByteString] -> [Piece]
splitSections image payloads =
  concat
    [ zipWith3 (piece section) bounds (drop 1 bounds <> [imageSectionSize section]) (slices bytes bounds)
    | (section, bytes) <- zip (imageSections image) payloads,
      let bounds = boundaries section
    ]
  where
    starts =
      Map.fromListWith
        Set.union
        [(role, Set.singleton (symbolOffset symbol)) | symbol <- imageSymbols image, Just role <- [symbolSection symbol]]
    total = sum [Set.size offsets | offsets <- Map.elems starts]
    stride = max 1 ((total + maximumPieces - 1) `div` maximumPieces)
    boundaries section =
      let size = imageSectionSize section
          offsets = Set.toAscList (Set.insert 0 (Set.filter (< size) (Map.findWithDefault Set.empty (imageSectionRole section) starts)))
       in [offset | (index, offset) <- zip [0 :: Int ..] offsets, index `mod` stride == 0]
    slices bytes bounds =
      [ BL.take (fromIntegral (end - start)) (BL.drop (fromIntegral start) bytes)
      | (start, end) <- zip bounds (drop 1 bounds <> [fromIntegral (BL.length bytes)])
      ]
    piece section start end bytes =
      let (name, sectionType, flags) = sectionKind (imageSectionRole section)
          alignment = imageSectionAlignment section
       in Piece
            { pieceRole = imageSectionRole section,
              pieceName = name,
              pieceType = sectionType,
              pieceFlags = flags,
              pieceAlignment = if start == 0 then alignment else min alignment (countTrailingZeros start),
              pieceStart = start,
              pieceSize = end - start,
              pieceBytes = bytes,
              pieceRelocations = [relocation | relocation <- imageSectionRelocations section, relocationOffset relocation >= start, relocationOffset relocation < end]
            }

data PlacedPiece = PlacedPiece
  { placedPiece :: !Piece,
    placedPieceOffset :: !Word64
  }

-- | The relocations of one piece: each at its offset in the piece, with the
-- symbol index and the addend it is written with.
data RelocationDescription = RelocationDescription
  { relocationName :: !Text,
    relocationTargetIndex :: !Word32,
    relocationValues :: ![(Word64, FixupKind, (Word32, Int64))]
  }

data PlacedRelocationSection = PlacedRelocationSection
  { placedRelocationDescription :: !RelocationDescription,
    placedRelocationOffset :: !Word64
  }

-- | The name, the type, and the flags of the ELF sections of a role.
sectionKind :: SectionRole -> (Text, Word32, Word64)
sectionKind role =
  case role of
    TextSection -> (".text", 1, 0x6)
    TextConstantsSection -> (".rodata", 1, 0x2)
    ReadOnlySection -> (".rodata", 1, 0x2)
    DataSection -> (".data", 1, 0x3)
    NoExecuteStackSection -> (".note.GNU-stack", 1, 0)

validateRelocations :: ImageSection -> Either ObjectError ()
validateRelocations section = mapM_ validate (imageSectionRelocations section)
  where
    validate relocation =
      case relocationKind relocation of
        Absolute64 -> pure ()
        X86Pc32 -> pure ()
        X86Plt32 -> pure ()
        kind -> Left (ObjectInvalidFixup kind)

placePiece :: Word64 -> Piece -> (Word64, PlacedPiece)
placePiece offset piece =
  let alignment = 1 `shiftL` pieceAlignment piece
      placed = alignUp alignment offset
   in (placed + pieceSize piece, PlacedPiece piece placed)

placeRelocationSection :: Word64 -> RelocationDescription -> (Word64, PlacedRelocationSection)
placeRelocationSection offset description =
  let placed = alignUp 8 offset
      size = fromIntegral (length (relocationValues description) * 24)
   in (placed + size, PlacedRelocationSection description placed)

putHeader :: Word64 -> Int -> Word32 -> Put
putHeader sectionHeaderOffset sectionCount sectionStringIndex = do
  putByteString (BS.pack [0x7f, 0x45, 0x4c, 0x46, 2, 1, 1, 0])
  replicateM_ 8 (putWord8 0)
  putWord16le 1
  putWord16le 62
  putWord32le 1
  putWord64le 0
  putWord64le 0
  putWord64le sectionHeaderOffset
  putWord32le 0
  putWord16le 64
  putWord16le 0
  putWord16le 0
  putWord16le 64
  putWord16le (fromIntegral sectionCount)
  putWord16le (fromIntegral sectionStringIndex)

putPieceContents :: Word64 -> [PlacedPiece] -> PutM Word64
putPieceContents offset pieces =
  case pieces of
    [] -> pure offset
    placed : rest -> do
      putPadding (placedPieceOffset placed - offset)
      putLazyByteString (pieceBytes (placedPiece placed))
      putPieceContents (placedPieceOffset placed + pieceSize (placedPiece placed)) rest

putRelocationContents :: Word64 -> [PlacedRelocationSection] -> PutM Word64
putRelocationContents offset sections =
  case sections of
    [] -> pure offset
    section : rest -> do
      putPadding (placedRelocationOffset section - offset)
      mapM_ putRelocation (relocationValues (placedRelocationDescription section))
      let next = placedRelocationOffset section + fromIntegral (length (relocationValues (placedRelocationDescription section)) * 24)
      putRelocationContents next rest

putRelocation :: (Word64, FixupKind, (Word32, Int64)) -> Put
putRelocation (offset, kind, (symbolIndex, addend)) = do
  let relocationType =
        case kind of
          Absolute64 -> 1
          X86Pc32 -> 2
          X86Plt32 -> 4
          _ -> 0
  putWord64le offset
  putWord64le (fromIntegral symbolIndex `shiftL` 32 .|. relocationType)
  putInt64le addend

putNullSymbol :: Put
putNullSymbol = replicateM_ 24 (putWord8 0)

-- | The local symbol that stands for the start of one piece.
putSectionSymbol :: Word32 -> Put
putSectionSymbol index = do
  putWord32le 0
  putWord8 3
  putWord8 0
  putWord16le (fromIntegral index)
  putWord64le 0
  putWord64le 0

-- | A global symbol: defined in a piece, at its offset in the piece, or
-- undefined.
putNamedSymbol :: (SectionRole -> Word64 -> (Word32, Word64)) -> Map Text Word32 -> Symbol -> Put
putNamedSymbol pieceOf stringIndexes symbol = do
  putWord32le (stringIndexes Map.! symbolName symbol)
  putWord8 0x10
  putWord8 0
  case symbolSection symbol of
    Nothing -> do
      putWord16le 0
      putWord64le 0
    Just role -> do
      let (index, offset) = pieceOf role (symbolOffset symbol)
      putWord16le (fromIntegral index)
      putWord64le offset
  putWord64le 0

putNullSectionHeader :: Put
putNullSectionHeader = replicateM_ 64 (putWord8 0)

putPieceSectionHeader :: Map Text Word32 -> PlacedPiece -> Put
putPieceSectionHeader names placed = do
  let piece = placedPiece placed
  putWord32le (names Map.! pieceName piece)
  putWord32le (pieceType piece)
  putWord64le (pieceFlags piece)
  putWord64le 0
  putWord64le (placedPieceOffset placed)
  putWord64le (pieceSize piece)
  putWord32le 0
  putWord32le 0
  putWord64le (1 `shiftL` pieceAlignment piece)
  putWord64le 0

putRelocationSectionHeader :: Map Text Word32 -> Word32 -> PlacedRelocationSection -> Put
putRelocationSectionHeader names symbolTableIndex section = do
  let description = placedRelocationDescription section
  putWord32le (names Map.! relocationName description)
  putWord32le 4
  putWord64le 0
  putWord64le 0
  putWord64le (placedRelocationOffset section)
  putWord64le (fromIntegral (length (relocationValues description) * 24))
  putWord32le symbolTableIndex
  putWord32le (relocationTargetIndex description)
  putWord64le 8
  putWord64le 24

putTableSectionHeader :: Word32 -> Word32 -> Word64 -> Word64 -> Word32 -> Word32 -> Word64 -> Word64 -> Put
putTableSectionHeader name sectionType offset size link info alignment entrySize = do
  putWord32le name
  putWord32le sectionType
  putWord64le 0
  putWord64le 0
  putWord64le offset
  putWord64le size
  putWord32le link
  putWord32le info
  putWord64le alignment
  putWord64le entrySize

buildStringTable :: [Text] -> (Map Text Word32, ByteString)
buildStringTable names =
  let uniqueNames = Map.keys (Map.fromList [(name, ()) | name <- names])
      (_, entries) = mapAccumL add 1 uniqueNames
      table = BS.cons 0 (BS.concat [bytes <> BS.singleton 0 | (_, bytes) <- entries])
   in (Map.fromList [(name, offset) | (name, (offset, _)) <- zip uniqueNames entries], table)
  where
    add offset name =
      let bytes = Text.encodeUtf8 name
       in (offset + BS.length bytes + 1, (fromIntegral offset, bytes))

putPadding :: Word64 -> Put
putPadding count = replicateM_ (fromIntegral count) (putWord8 0)

alignUp :: Word64 -> Word64 -> Word64
alignUp alignment value = (value + alignment - 1) .&. (maxBound - (alignment - 1))
