-- | The text that the explorer shows for one stage of one module, and how a
-- definition in one stage finds the related definition in another stage.
module Aihc.Dev.Explore.Document
  ( Stage (..),
    stageLabel,
    Definition (..),
    DefinitionKind (..),
    Segment (..),
    Document (..),
    Section (..),
    documentFromSections,
    emptyDocument,
    definitionAt,
    Match (..),
    findRelated,
    findDefinitionNamed,
  )
where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

-- | A view of the explorer, in the order of the pipeline.
data Stage
  = StageHaskell
  | StageFc
  | StageGrin
  | StageCpsGrin
  | StageGcGrin
  deriving (Eq, Ord, Show, Enum, Bounded)

stageLabel :: Stage -> Text
stageLabel stage =
  case stage of
    StageHaskell -> "Haskell"
    StageFc -> "System FC"
    StageGrin -> "GRIN"
    StageCpsGrin -> "GRIN-CPS"
    StageGcGrin -> "GRIN-GC"

-- | A top-level definition: the module that its name comes from, if the name
-- has a module, its name without the module, and its kind.
data Definition = Definition
  { definitionModule :: !(Maybe Text),
    definitionName :: !Text,
    definitionKind :: !DefinitionKind
  }
  deriving (Eq, Ord, Show)

-- | A code definition, such as a function, is a better match than a data
-- definition with the same name, such as the closure of a GRIN global.
data DefinitionKind
  = DefinitionCode
  | DefinitionData
  deriving (Eq, Ord, Show)

-- | A part of a line with one highlight class. The class is a TextMate scope
-- such as @keyword.operator@, or empty for plain text.
data Segment = Segment
  { segmentText :: !Text,
    segmentClass :: !Text
  }
  deriving (Eq, Show)

-- | The lines of a document, the highlighted segments of each line, and the
-- first line of each definition, in line order. The segments are lazy: the
-- explorer highlights the lines that it shows.
data Document = Document
  { documentLines :: !(Vector Text),
    documentSegments :: Vector [Segment],
    documentDefinitions :: ![(Int, Definition)]
  }

emptyDocument :: Document
emptyDocument = Document V.empty V.empty []

-- | A part of a document that the highlighter can start at, and its
-- definition, if it has one.
data Section = Section
  { sectionDefinition :: !(Maybe Definition),
    sectionLines :: ![Text]
  }

-- | Join the sections with a blank line between each two. The highlighter
-- receives the lines of one section and gives the segments of each line.
documentFromSections :: ([Text] -> [[Segment]]) -> [Section] -> Document
documentFromSections highlight sections =
  Document
    { documentLines = V.fromList (concat blocks),
      documentSegments = V.fromList (concat highlighted),
      documentDefinitions = [(line, definition) | (line, Section (Just definition) _) <- zip starts sections]
    }
  where
    blocks = zipWith (\index section -> [T.empty | index > (0 :: Int)] <> sectionLines section) [0 ..] sections
    highlighted = zipWith (\index section -> [[] | index > (0 :: Int)] <> padded (sectionLines section)) [0 ..] sections
    padded sectionText = zipWith const (highlight sectionText <> repeat []) sectionText
    -- The first line of each section, after the blank line in front of it.
    starts = zipWith (\index offset -> offset + (if index > (0 :: Int) then 1 else 0)) [0 ..] (scanl (+) 0 (map length blocks))

-- | The definition that contains a line: the last definition that starts at
-- or before the line.
definitionAt :: Document -> Int -> Maybe Definition
definitionAt document line =
  fmap snd (listToMaybe (reverse (takeWhile ((<= line) . fst) (documentDefinitions document))))

-- | How a definition of one stage matches a definition of another stage.
data Match
  = -- | The same module and the same name.
    MatchExact
  | -- | The same module and a related name.
    MatchRelated
  | -- | The first definition of the same module.
    MatchModule
  deriving (Eq, Show)

-- | The first line of the definition in a document that is related to a
-- definition of another stage, and how it matches.
--
-- The search is by name only:
--
-- 1. a definition with the same module and the same name;
-- 2. a definition of the same module whose name contains the other name, or
--    that the other name contains, such as @$wf@ for @f@; the longest
--    common name wins;
-- 3. the first definition of the same module.
findRelated :: Document -> Text -> Definition -> Maybe (Int, Match)
findRelated document currentModule target =
  case exact of
    Just line -> Just (line, MatchExact)
    Nothing -> case related of
      Just line -> Just (line, MatchRelated)
      Nothing -> fmap (,MatchModule) sameModule
  where
    targetModule = fromMaybe currentModule (definitionModule target)
    targetName = baseName (definitionName target)
    candidates = [(line, definition) | (line, definition) <- documentDefinitions document, moduleOf definition == targetModule]
    moduleOf definition = fromMaybe currentModule (definitionModule definition)
    exact = fmap snd (listToMaybe (sortOn fst [(definitionKind definition, line) | (line, definition) <- candidates, baseName (definitionName definition) == targetName]))
    related =
      fmap fst . listToMaybe . sortOn (Down . snd) $
        [ (line, T.length (shorter name))
        | (line, definition) <- candidates,
          let name = baseName (definitionName definition),
          T.length (shorter name) >= 2,
          name `T.isInfixOf` targetName || targetName `T.isInfixOf` name
        ]
    shorter name = if T.length name < T.length targetName then name else targetName
    sameModule = listToMaybe (map fst candidates)

-- | The first line of the first definition with this name, in any module.
findDefinitionNamed :: Document -> Text -> Maybe Int
findDefinitionNamed document name =
  listToMaybe [line | (line, definition) <- documentDefinitions document, definitionName definition == name]

-- | The name without the @$@ that GRIN puts in front of a function name.
baseName :: Text -> Text
baseName name = fromMaybe name (T.stripPrefix "$" name)
