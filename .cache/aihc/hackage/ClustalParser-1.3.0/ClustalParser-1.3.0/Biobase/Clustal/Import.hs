-- | Parse Clustal output
--   For more information on Clustal tools consult: <http://www.clustal.org/>
module Biobase.Clustal.Import (
                       parseClustalAlignment,
                       readClustalAlignment,
                       parseStructuralClustalAlignment,
                       readStructuralClustalAlignment,
                       parseClustalSummary,
                       readClustalSummary,
                       module Biobase.Clustal.Types
                      ) where

import Biobase.Clustal.Types
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List
import qualified Data.Text as T
readDouble :: String -> Double
readDouble = read

readInt :: String -> Int
readInt = read

-- | Parse the input as ClustalSummary datatype
genParserClustalSummary :: GenParser Char st ClustalSummary
genParserClustalSummary = do
  newline
  newline
  newline
  space
  string "CLUSTAL "
  version <- many1 (noneOf " ")
  many1 (noneOf "\n")
  newline
  newline
  newline
  string "Sequence format is "
  sequenceFormat' <- many1 (noneOf "\n")
  newline
  sequenceParametersList <- many1 (try genParserSequenceParameters)
  string "Start of Pairwise alignments"
  newline
  string "Aligning..."
  newline
  newline
  pairwiseAlignmentSummaryList <- many1 genParserPairwiseAlignmentSummary
  string "Guide tree file created:   ["
  guideTreeFileName' <- many1 (noneOf "]")
  char ']'
  newline
  newline
  string "There are "
  numberOfGroups <- many1 digit
  string " groups"
  newline
  string "Start of Multiple Alignment"
  newline
  newline
  string "Aligning..."
  newline
  groupSummaryList <- many1 genParserGroupSummary
  string "Alignment Score "
  alignmentScore' <- many1 digit
  newline
  newline
  string "CLUSTAL-Alignment file created  ["
  alignmentFileName' <- many1 (noneOf "]")
  char ']'
  newline
  newline
  eof
  return $ ClustalSummary (T.pack version) (T.pack sequenceFormat') sequenceParametersList pairwiseAlignmentSummaryList (T.pack guideTreeFileName') (readInt numberOfGroups) groupSummaryList (readInt alignmentScore') (T.pack alignmentFileName')

genParserGroupSummary :: GenParser Char st GroupSummary
genParserGroupSummary = do
  string "Group "
  groupIndex <- many1 digit
  string ":"
  optional space
  optional (string "Sequences:")
  many1 space
  sequenceNumber <- optionMaybe (many1 digit)
  optional (many1 space)
  string "Score:" <|>  string "Delayed"
  groupScore' <- optionMaybe (many1 digit)
  newline
  return $ GroupSummary (readInt groupIndex) (liftM readInt sequenceNumber) (liftM readInt groupScore')

genParserPairwiseAlignmentSummary :: GenParser Char st PairwiseAlignmentSummary
genParserPairwiseAlignmentSummary = do
  string "Sequences ("
  firstSeqIndex <- many1 digit
  string ":"
  secondSeqIndex <- many1 digit
  string ") Aligned. Score:"
  many1 space
  pairwiseScore <- many1 digit
  newline
  return $ PairwiseAlignmentSummary (readInt firstSeqIndex) (readInt secondSeqIndex) (readInt pairwiseScore)

genParserSequenceParameters :: GenParser Char st SequenceParameters
genParserSequenceParameters = do
  string "Sequence "
  sequenceIndexParam <- many1 digit
  string ": "
  sequenceIdentifierParam <- many1 (noneOf " ")
  spaces
  sequenceLengthParam <- many1 digit
  space
  string "bp"
  newline
  return $ SequenceParameters (readInt sequenceIndexParam) (T.pack sequenceIdentifierParam) (readInt sequenceLengthParam)

-- | Parse the input as ClustalAlignment datatype
genParserClustalAlignment :: GenParser Char st ClustalAlignment
genParserClustalAlignment = do
  string "CLUSTAL"
  many1 (noneOf "\n")
  many1 (try newline)
  alignmentSlices <- many1 (try genParserClustalAlignmentSlice)
  optional newline
  eof
  return (mergealignmentSlices alignmentSlices)

mergealignmentSlices :: [ClustalAlignmentSlice] -> ClustalAlignment
mergealignmentSlices slices = alignment
  where entrySlicesList = concatMap entrySlices slices -- list of lists of entry slices
        sequenceIdentifiers = nub (map entrySequenceSliceIdentifier entrySlicesList)
        mergedAlignmentEntries = map (constructAlignmentEntries entrySlicesList) sequenceIdentifiers
        mergedConservationTrack = concatMap conservationTrackSlice slices
        alignment = ClustalAlignment mergedAlignmentEntries (T.pack mergedConservationTrack)

constructAlignmentEntries ::  [ClustalAlignmentEntrySlice] -> String -> ClustalAlignmentEntry
constructAlignmentEntries slices entryIdentifier= entry
  where currentSlices = filter (\a -> entrySequenceSliceIdentifier a == entryIdentifier) slices
        entrySequence = concatMap entryAlignedSliceSequence currentSlices
        entry = ClustalAlignmentEntry (T.pack entryIdentifier) (T.pack entrySequence)

genParserClustalAlignmentSlice :: GenParser Char st ClustalAlignmentSlice
genParserClustalAlignmentSlice = do
  entrySlices' <- many1 genParserClustalEntrySlice
  --extract length of identifier and spacer to determine offset of conservation track
  let offsetLenght = length (entrySequenceSliceIdentifier (head entrySlices')) + spacerLength (head entrySlices')
  conservationTrackSliceChoice  <- choice [(lookAhead (string "\n")), (try (genParserConservationTrackSlice offsetLenght))]
  --spacerAndConservationTrackSlice <- many1 (noneOf "\n")
  --let conservationTrackSlice' = drop offsetLenght spacerAndConservationTrackSlice
  --newline
  let conservationTrackSlice' = if (conservationTrackSliceChoice == "\n") then "" else conservationTrackSliceChoice
  optional newline
  return $ ClustalAlignmentSlice entrySlices' conservationTrackSlice'

genParserConservationTrackSlice :: Int -> GenParser Char st String
genParserConservationTrackSlice offsetLenght = do
  spacerAndConservationTrackSlice <- many1 (noneOf "\n")
  let conservationTrackSlice' = drop offsetLenght spacerAndConservationTrackSlice
  newline
  return $ conservationTrackSlice'

genParserClustalEntrySlice :: GenParser Char st ClustalAlignmentEntrySlice
genParserClustalEntrySlice = do
  sliceIdentifier <- many1 (noneOf " \n")
  spacer <- many1 (char ' ')
  sliceSequence <- parseNucleotideAlignmentEntry
  newline
  return $ ClustalAlignmentEntrySlice sliceIdentifier sliceSequence (length spacer)

--Structural Clustal Parser functions

-- | Parse the input as ClustalAlignment datatype as used in mlocarna
genParserStructuralClustalAlignment :: GenParser Char st StructuralClustalAlignment
genParserStructuralClustalAlignment = do
  genParseMlocarnaHeader
  alignmentSlices <- many1 (try genParserStructuralClustalAlignmentSlice)
  secondaryStructure <- genSecondaryStructure
  energy' <- genParseEnergy
  eof
  return (mergeStructuralAlignmentSlices (concat alignmentSlices) secondaryStructure energy')

genSecondaryStructure :: GenParser Char st String
genSecondaryStructure = do
  string "alifold"
  secondaryStructure <- many1 (try genSecondaryStructureSlice)
  return (concat secondaryStructure)

genSecondaryStructureSlice :: GenParser Char st String
genSecondaryStructureSlice = do
  many1 space
  secondaryStructureSlice <- many1 (try (oneOf ".()"))
  choice [try (string "\n"),try (string " ")]
  return secondaryStructureSlice

genParseEnergy :: GenParser Char st Double
genParseEnergy = do
  string "("
  many space
  energy' <- many1 (noneOf " ")
  optional space
  char ('=')
  many1 (noneOf "\n")
  newline
  return (readDouble energy')

genParseMlocarnaHeader :: GenParser Char st String
genParseMlocarnaHeader = do
  string "mLocARNA"
  many1 (choice [alphaNum,oneOf "-: ()."])
  newline
  string "Copyright"
  many1 (choice [alphaNum,char ' '])
  newline
  newline
  string "Compute pair probs ..."
  newline
  optional (try (string "Compute pairwise alignments ... "))
  optional (try newline)
  string "Perform progressive alignment ..."
  many1 newline
  return ""

mergeStructuralAlignmentSlices :: [StructuralClustalAlignmentEntrySlice] -> String -> Double -> StructuralClustalAlignment
mergeStructuralAlignmentSlices slices secondaryStructure energy' = alignment
  where sequenceIdentifiers = nub (map structuralEntrySequenceSliceIdentifier slices)
        mergedAlignmentEntries = map (constructStructuralAlignmentEntries slices) sequenceIdentifiers
        alignment = StructuralClustalAlignment mergedAlignmentEntries (T.pack secondaryStructure) energy'

constructStructuralAlignmentEntries ::  [StructuralClustalAlignmentEntrySlice] -> String -> ClustalAlignmentEntry
constructStructuralAlignmentEntries slices entryIdentifier= entry
  where currentSlices = filter (\a -> structuralEntrySequenceSliceIdentifier a == entryIdentifier) slices
        entrySequence = concatMap structuralEntryAlignedSliceSequence currentSlices
        entry = ClustalAlignmentEntry (T.pack entryIdentifier) (T.pack entrySequence)

genParserStructuralClustalAlignmentSlice :: GenParser Char st [StructuralClustalAlignmentEntrySlice]
genParserStructuralClustalAlignmentSlice = do
  entrySlices' <- many1 (try genParserStructuralClustalEntrySlice)
  optional newline
  return entrySlices'

genParserStructuralClustalEntrySlice :: GenParser Char st StructuralClustalAlignmentEntrySlice
genParserStructuralClustalEntrySlice = do
  sliceIdentifier <- many1 (noneOf " ")
  many1 (char ' ')
  sliceSequence <- parseNucleotideAlignmentEntry
  newline
  return $ StructuralClustalAlignmentEntrySlice (filter (/='\n') sliceIdentifier) sliceSequence

-- exported functions

-- | Parse Clustal alignment (.aln) from String
parseClustalAlignment :: String -> Either ParseError ClustalAlignment
parseClustalAlignment = parse genParserClustalAlignment "genParserClustalAlignment"

-- | Parse Clustal alignment (.aln) from filehandle
readClustalAlignment :: String ->  IO (Either ParseError ClustalAlignment)
readClustalAlignment = parseFromFile genParserClustalAlignment

-- | Parse Clustal alignment (.aln) with secondary structure in dot-bracket notation from String (as produced by mlocarna)
parseStructuralClustalAlignment :: String ->  Either ParseError StructuralClustalAlignment
parseStructuralClustalAlignment = parse genParserStructuralClustalAlignment "genParserStructuralClustalAlignment"

-- | Parse Clustal alignment (.aln) with secondary structure in dot-bracket notation from filehandle (as produced by mlocarna)
readStructuralClustalAlignment :: String ->  IO (Either ParseError StructuralClustalAlignment)
readStructuralClustalAlignment = parseFromFile genParserStructuralClustalAlignment

-- |  Parse Clustal summary (printed to STDOUT) from String
parseClustalSummary :: String ->   Either ParseError ClustalSummary
parseClustalSummary = parse genParserClustalSummary "genParserClustalSummary"

-- | Parse Clustal summary (printed to STDOUT) from file
readClustalSummary ::  String -> IO (Either ParseError ClustalSummary)
readClustalSummary = parseFromFile genParserClustalSummary

-- | Parse nucleotide sequence. Allowed letters according to IUPAC
--parseNucleotideSequence :: GenParser Char st String
--parseNucleotideSequence = do
--  nucleotideSequence <- many1 (oneOf "RYSWKMBDHVNATUGCryswkmbdhvnatugc")
--  return $ nucleotideSequence

-- | Parse nucleotide alignment entry. Allowed letters according to IUPAC and commonly used gap characters
parseNucleotideAlignmentEntry :: GenParser Char st String
parseNucleotideAlignmentEntry = do
  entry <- many1 (oneOf "~_-.RYSWKMBDHVNATUGCryswkmbdhvnatugc")
  return $ entry

-- | Parse protein amino acid code sequence. Allowed letters according to IUPAC
--parseProteinSequence :: GenParser Char st String
--parseProteinSequence = do
--  proteinSequence <- many1 (oneOf "ABCDEFGHIKLMNPQRSTVWXYZabcdefghiklmnpqrstvwxyz")
--  return $ proteinSequence

-- | Parse protein amino acid code alignment entry. Allowed letters according to IUPAC and commonly used gap characters
--parseProteinAlignmentEntry :: GenParser Char st String
--parseProteinAlignmentEntry = do
--  entry <- many1 (oneOf "~_-.ABCDEFGHIKLMNPQRSTVWXYZabcdefghiklmnpqrstvwxyz")
--  return $ entry
