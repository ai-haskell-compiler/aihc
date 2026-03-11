-- | This module contains data structures for the Clustal tools
--   For more information on Clustal tools consult: <http://www.clustal.org/>

module Biobase.Clustal.Types where
import qualified Data.Vector as V
import qualified Data.Text as T

-- | Data type for clustal summary, containing information about the alignment process, usually printed to STDOUT
data ClustalSummary = ClustalSummary
  {
    clustalw2version :: T.Text,
    sequenceFormat :: T.Text,
    parametersOfInputSequences :: [SequenceParameters],
    pairwiseAlignmentSummaries :: [PairwiseAlignmentSummary],
    guideTreeFileName :: T.Text,
    groupNumber :: Int,
    groupSummaries :: [GroupSummary],
    alignmentScore :: Int,
    alignmentFileName :: T.Text
  }
  deriving (Show, Eq)

data SequenceParameters = SequenceParameters
  {
     inputSequenceIndex :: Int,
     inputSequenceIdentifier :: T.Text,
     inputSequenceLength :: Int
  }
  deriving (Show, Eq)

data PairwiseAlignmentSummary = PairwiseAlignmentSummary
  {
     firstSequenceIndex :: Int,
     secondSequenceIndex :: Int,
     pairwiseAlignmentScore :: Int
  }
  deriving (Show, Eq)

data GroupSummary = GroupSummary
  {
     alignmentGroupIndex :: Int,
     numberOfAlignedSequences :: Maybe Int,
     groupScore :: Maybe Int
  }
  deriving (Show, Eq)

-- | Data structure for Clustal alignment format
data ClustalAlignment = ClustalAlignment
  {
    alignmentEntries :: [ClustalAlignmentEntry],
    conservationTrack :: T.Text
  }
  deriving (Eq)

instance Show ClustalAlignment where
  show (ClustalAlignment _alignmentEntries _conservationTrack )
    | not (null _alignmentEntries) = header ++ alignmentString
    | otherwise = header
    where header = "CLUSTAL W (1.8) multiple sequence alignment\n\n\n"
          longestSequenceIdLength =  (maximum (map T.length (map entrySequenceIdentifier _alignmentEntries))) + 1
          totalSequenceLength = T.length (entryAlignedSequence (head _alignmentEntries))
          alignmentString = showAlignment totalSequenceLength longestSequenceIdLength 0 _alignmentEntries _conservationTrack

showAlignment :: Int -> Int -> Int -> [ClustalAlignmentEntry] -> T.Text -> String
showAlignment totalSequenceLength longestSequenceIdLength currentWindowPosition _alignmentEntries _conservationTrack
  | totalSequenceLength == 0 = []
  | currentWindowPosition < totalSequenceLength = showAlignmentBlock longestSequenceIdLength currentWindowPosition _alignmentEntries _conservationTrack ++ (showAlignment totalSequenceLength longestSequenceIdLength (currentWindowPosition + 60) _alignmentEntries _conservationTrack)
  | currentWindowPosition == totalSequenceLength = []
  | otherwise = ""

showAlignmentBlock :: Int -> Int -> [ClustalAlignmentEntry] -> T.Text -> String
showAlignmentBlock longestSequenceIdLength currentWindowPosition _alignmentEntries _conservationTrack = blockString
  where blockString = entries ++ extraTrack ++ "\n"
        entries = concatMap (showAlignmentLine longestSequenceIdLength currentWindowPosition) _alignmentEntries
        extraTrack = concat (replicate longestSequenceIdLength " ") ++ V.toList (V.slice currentWindowPosition 60  (V.fromList (T.unpack _conservationTrack))) ++ "\n"

showAlignmentLine :: Int -> Int -> ClustalAlignmentEntry -> String
showAlignmentLine longestSequenceIdLength currentWindowPosition _alignmentEntry = T.unpack (entrySequenceIdentifier _alignmentEntry) ++ concat (replicate (longestSequenceIdLength - T.length (entrySequenceIdentifier _alignmentEntry)) " ") ++ V.toList (V.slice currentWindowPosition 60  (V.fromList (T.unpack (entryAlignedSequence _alignmentEntry)))) ++ "\n"

data ClustalAlignmentEntry = ClustalAlignmentEntry
  {
    entrySequenceIdentifier :: T.Text,
    entryAlignedSequence :: T.Text
  }
  deriving (Show, Eq)

data ClustalAlignmentSlice = ClustalAlignmentSlice
  {
    entrySlices :: [ClustalAlignmentEntrySlice],
    conservationTrackSlice :: String
  }
  deriving (Show, Eq)

data ClustalAlignmentEntrySlice = ClustalAlignmentEntrySlice
   {
     entrySequenceSliceIdentifier :: String,
     entryAlignedSliceSequence :: String,
     spacerLength :: Int
   }
   deriving (Show, Eq)

-- | Data structure for structural Clustal alignment format
data StructuralClustalAlignment = StructuralClustalAlignment
  {
    structuralAlignmentEntries :: [ClustalAlignmentEntry],
    secondaryStructureTrack :: T.Text,
    energy :: Double
  }
  deriving (Eq)

instance Show StructuralClustalAlignment where
  show (StructuralClustalAlignment _alignmentEntries _secondaryStructureTrack _energy)
    | not (null _alignmentEntries) = header ++ alignmentString
    | otherwise = header
    where header = "CLUSTAL W \n\n"
          longestSequenceIdLength =  (maximum (map T.length (map entrySequenceIdentifier _alignmentEntries))) + 1
          totalSequenceLength = T.length (entryAlignedSequence (head _alignmentEntries))
          alignmentString = showAlignment totalSequenceLength longestSequenceIdLength 0 _alignmentEntries _secondaryStructureTrack

-- data StructuralClustalAlignmentSlice = StructuralClustalAlignmentSlice
--   {
--     structuralEntrySlices :: [StructuralClustalAlignmentEntrySlice]
--   }
--   deriving (Show, Eq)

data StructuralClustalAlignmentEntrySlice = StructuralClustalAlignmentEntrySlice
  {
    structuralEntrySequenceSliceIdentifier :: String,
    structuralEntryAlignedSliceSequence :: String
  }
  deriving (Show, Eq)
