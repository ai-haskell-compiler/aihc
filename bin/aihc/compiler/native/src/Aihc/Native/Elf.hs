{-# LANGUAGE OverloadedStrings #-}

-- | Write AMD64 ELF relocatable objects.
module Aihc.Native.Elf
  ( writeAmd64Elf,
  )
where

import Aihc.Native.Object
import Control.Monad (replicateM_)
import Data.Binary.Put
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (mapAccumL, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word32, Word64)

writeAmd64Elf :: Image -> Either ObjectError BL.ByteString
writeAmd64Elf image = do
  baseSections <- mapM describeSection (imageSections image)
  mapM_ validateSectionRelocations baseSections
  let orderedSymbols = orderSymbols (imageSymbols image)
      symbolIndexes = Map.fromList [(symbolName symbol, index) | (index, symbol) <- zip [1 :: Word32 ..] orderedSymbols]
      symbolStrings = buildStringTable (map symbolName orderedSymbols)
      relocationSections =
        [ RelocationDescription
            { relocationName = ".rela" <> descriptionName section,
              relocationTargetIndex = index,
              relocationValues = imageSectionRelocations (descriptionImage section)
            }
        | (index, section) <- zip [1 :: Word32 ..] baseSections,
          not (null (imageSectionRelocations (descriptionImage section)))
        ]
      baseCount = length baseSections
      relocationCount = length relocationSections
      symbolTableIndex = fromIntegral (1 + baseCount + relocationCount)
      stringTableIndex = symbolTableIndex + 1
      sectionStringTableIndex = stringTableIndex + 1
      sectionNames =
        map descriptionName baseSections
          <> map relocationName relocationSections
          <> [".symtab", ".strtab", ".shstrtab"]
      sectionStrings = buildStringTable sectionNames
      (baseEnd, placedBaseSections) = mapAccumL placeBaseSection 64 baseSections
      (relocationEnd, placedRelocations) = mapAccumL placeRelocationSection (alignUp 8 baseEnd) relocationSections
      symbolOffset = alignUp 8 relocationEnd
      symbolSize = fromIntegral ((1 + length orderedSymbols) * 24)
      stringOffset = symbolOffset + symbolSize
      sectionStringOffset = stringOffset + fromIntegral (BS.length (snd symbolStrings))
      sectionHeaderOffset = alignUp 8 (sectionStringOffset + fromIntegral (BS.length (snd sectionStrings)))
      sectionCount = 1 + length sectionNames
      localCount = length (filter (not . symbolGlobal) orderedSymbols)
  pure . runPut $ do
    putHeader sectionHeaderOffset sectionCount sectionStringTableIndex
    _ <- putBaseContents 64 placedBaseSections
    putPadding (alignUp 8 baseEnd - baseEnd)
    _ <- putRelocationContents symbolIndexes (alignUp 8 baseEnd) placedRelocations
    putPadding (symbolOffset - relocationEnd)
    putNullSymbol
    mapM_ (putSymbol placedBaseSections (fst symbolStrings)) orderedSymbols
    putByteString (snd symbolStrings)
    putByteString (snd sectionStrings)
    putPadding (sectionHeaderOffset - sectionStringOffset - fromIntegral (BS.length (snd sectionStrings)))
    putNullSectionHeader
    mapM_ (putBaseSectionHeader (fst sectionStrings)) placedBaseSections
    mapM_ (putRelocationSectionHeader (fst sectionStrings) symbolTableIndex) placedRelocations
    putTableSectionHeader (fst sectionStrings Map.! ".symtab") 2 symbolOffset symbolSize stringTableIndex (fromIntegral (1 + localCount)) 8 24
    putTableSectionHeader (fst sectionStrings Map.! ".strtab") 3 stringOffset (fromIntegral (BS.length (snd symbolStrings))) 0 0 1 0
    putTableSectionHeader (fst sectionStrings Map.! ".shstrtab") 3 sectionStringOffset (fromIntegral (BS.length (snd sectionStrings))) 0 0 1 0

data SectionDescription = SectionDescription
  { descriptionImage :: !ImageSection,
    descriptionName :: !Text,
    descriptionType :: !Word32,
    descriptionFlags :: !Word64
  }

data PlacedBaseSection = PlacedBaseSection
  { placedBaseDescription :: !SectionDescription,
    placedBaseOffset :: !Word64
  }

data RelocationDescription = RelocationDescription
  { relocationName :: !Text,
    relocationTargetIndex :: !Word32,
    relocationValues :: ![Relocation]
  }

data PlacedRelocationSection = PlacedRelocationSection
  { placedRelocationDescription :: !RelocationDescription,
    placedRelocationOffset :: !Word64
  }

describeSection :: ImageSection -> Either ObjectError SectionDescription
describeSection section =
  case imageSectionRole section of
    TextSection -> description ".text" 1 0x6
    TextConstantsSection -> description ".rodata" 1 0x2
    ReadOnlySection -> description ".rodata" 1 0x2
    DataSection -> description ".data" 1 0x3
    NoExecuteStackSection -> description ".note.GNU-stack" 1 0
  where
    description name sectionType flags = pure (SectionDescription section name sectionType flags)

validateSectionRelocations :: SectionDescription -> Either ObjectError ()
validateSectionRelocations section = mapM_ validate (imageSectionRelocations (descriptionImage section))
  where
    validate relocation =
      case relocationKind relocation of
        Absolute64 -> pure ()
        X86Pc32 -> pure ()
        X86Plt32 -> pure ()
        kind -> Left (ObjectInvalidFixup kind)

placeBaseSection :: Word64 -> SectionDescription -> (Word64, PlacedBaseSection)
placeBaseSection offset description =
  let section = descriptionImage description
      alignment = 1 `shiftL` imageSectionAlignment section
      placed = alignUp alignment offset
      size = fromIntegral (BL.length (imageSectionBytes section))
   in (placed + size, PlacedBaseSection description placed)

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

putBaseContents :: Word64 -> [PlacedBaseSection] -> PutM Word64
putBaseContents offset sections =
  case sections of
    [] -> pure offset
    section : rest -> do
      putPadding (placedBaseOffset section - offset)
      let bytes = imageSectionBytes (descriptionImage (placedBaseDescription section))
          next = placedBaseOffset section + fromIntegral (BL.length bytes)
      putLazyByteString bytes
      putBaseContents next rest

putRelocationContents :: Map Text Word32 -> Word64 -> [PlacedRelocationSection] -> PutM Word64
putRelocationContents indexes offset sections =
  case sections of
    [] -> pure offset
    section : rest -> do
      putPadding (placedRelocationOffset section - offset)
      mapM_ (putRelocation indexes) (relocationValues (placedRelocationDescription section))
      let next = placedRelocationOffset section + fromIntegral (length (relocationValues (placedRelocationDescription section)) * 24)
      putRelocationContents indexes next rest

putRelocation :: Map Text Word32 -> Relocation -> Put
putRelocation indexes relocation = do
  let symbolIndex = indexes Map.! relocationTarget relocation
      relocationType =
        case relocationKind relocation of
          Absolute64 -> 1
          X86Pc32 -> 2
          X86Plt32 -> 4
          _ -> 0
  putWord64le (relocationOffset relocation)
  putWord64le (fromIntegral symbolIndex `shiftL` 32 .|. relocationType)
  putInt64le (relocationAddend relocation)

putNullSymbol :: Put
putNullSymbol = replicateM_ 24 (putWord8 0)

putSymbol :: [PlacedBaseSection] -> Map Text Word32 -> Symbol -> Put
putSymbol sections stringIndexes symbol = do
  putWord32le (stringIndexes Map.! symbolName symbol)
  putWord8 (if symbolGlobal symbol then 0x10 else 0)
  putWord8 0
  case symbolSection symbol of
    Nothing -> do
      putWord16le 0
      putWord64le 0
    Just role -> do
      putWord16le (fromIntegral (findSectionIndex role sections))
      putWord64le (symbolOffset symbol)
  putWord64le 0

findSectionIndex :: SectionRole -> [PlacedBaseSection] -> Int
findSectionIndex role sections =
  case [index | (index, section) <- zip [1 :: Int ..] sections, imageSectionRole (descriptionImage (placedBaseDescription section)) == role] of
    index : _ -> index
    [] -> 0

putNullSectionHeader :: Put
putNullSectionHeader = replicateM_ 64 (putWord8 0)

putBaseSectionHeader :: Map Text Word32 -> PlacedBaseSection -> Put
putBaseSectionHeader names section = do
  let description = placedBaseDescription section
      imageSection = descriptionImage description
  putWord32le (names Map.! descriptionName description)
  putWord32le (descriptionType description)
  putWord64le (descriptionFlags description)
  putWord64le 0
  putWord64le (placedBaseOffset section)
  putWord64le (fromIntegral (BL.length (imageSectionBytes imageSection)))
  putWord32le 0
  putWord32le 0
  putWord64le (1 `shiftL` imageSectionAlignment imageSection)
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

orderSymbols :: [Symbol] -> [Symbol]
orderSymbols symbols = sortOn symbolName locals <> sortOn symbolName globals
  where
    locals = filter (not . symbolGlobal) symbols
    globals = filter symbolGlobal symbols

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
