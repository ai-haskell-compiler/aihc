-- | The Haskell view of the explorer: highlighting from the tokens of the
-- AIHC lexer, and the top-level definitions of a module.
module Aihc.Dev.Explore.Haskell
  ( haskellDocument,
  )
where

import Aihc.Dev.Explore.Document (Definition (..), DefinitionKind (..), Document (..), Segment (..))
import Aihc.Parser.Syntax (sourceSpanEndCol, sourceSpanEndLine, sourceSpanStartCol, sourceSpanStartLine)
import Aihc.Parser.Token (LexToken (..), LexTokenKind (..), TokenOrigin (..), lexModuleTokensWithExtensions)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

-- | The document of a Haskell source text. A top-level definition starts at
-- a token in the first column: the name of a binding or of its signature,
-- or the name of a declared type or class.
haskellDocument :: Text -> Document
haskellDocument source =
  Document
    { documentLines = V.fromList sourceLines,
      documentSegments = V.fromList (zipWith lineSegments [1 ..] sourceLines),
      documentDefinitions = definitions tokens
    }
  where
    sourceLines = T.lines source
    tokens = filter ((== FromSource) . lexTokenOrigin) (lexModuleTokensWithExtensions [] source)
    -- The colored column ranges of each line. A token on more than one line,
    -- such as a block comment, colors each of its lines.
    ranges =
      IntMap.fromListWith
        (<>)
        [ (line, [(from, to, highlightClass)])
        | token <- tokens,
          let highlightClass = tokenClass (lexTokenKind token),
          not (T.null highlightClass),
          let tokenSpan = lexTokenSpan token
              startLine = sourceSpanStartLine tokenSpan
              startCol = sourceSpanStartCol tokenSpan
              endLine = sourceSpanEndLine tokenSpan
              endCol = sourceSpanEndCol tokenSpan,
          line <- [startLine .. endLine],
          let from = if line == startLine then startCol else 1
              to = if line == endLine then endCol else maxBound
        ]
    lineSegments line = cut 1 (sortOn (\(from, _, _) -> from) (IntMap.findWithDefault [] line ranges))

-- | Cut a line into segments at the column ranges. The columns start at one.
cut :: Int -> [(Int, Int, Text)] -> Text -> [Segment]
cut column ranges text
  | T.null text = []
  | otherwise = case ranges of
      [] -> [Segment text ""]
      (from, to, highlightClass) : rest
        | to <= column -> cut column rest text
        | from > column ->
            let (plain, after) = T.splitAt (from - column) text
             in Segment plain "" : cut from ranges after
        | otherwise ->
            let width = if to == maxBound then T.length text else to - column
                (colored, after) = T.splitAt width text
             in Segment colored highlightClass : cut (column + T.length colored) rest after

-- | The top-level definitions, in line order. The lines start at zero.
definitions :: [LexToken] -> [(Int, Definition)]
definitions tokens =
  case tokens of
    first : rest
      | Just (line, name) <- definitionStart first rest -> (line, Definition Nothing name DefinitionCode) : definitions rest
      | otherwise -> definitions rest
    [] -> []
  where
    definitionStart token rest
      | sourceSpanStartCol (lexTokenSpan token) /= 1 = Nothing
      | otherwise =
          let line = sourceSpanStartLine (lexTokenSpan token) - 1
           in case (lexTokenKind token, map lexTokenKind (take 2 rest)) of
                (TkVarId name, _) -> Just (line, name)
                (TkSpecialLParen {}, TkVarSym name : _) -> Just (line, name)
                (TkSpecialLParen {}, TkConSym name : _) -> Just (line, name)
                (kind, TkConId name : _) | declaresType kind -> Just (line, name)
                _ -> Nothing
    declaresType kind = case kind of
      TkKeywordData {} -> True
      TkKeywordNewtype {} -> True
      TkKeywordType {} -> True
      TkKeywordClass {} -> True
      _ -> False

-- | The highlight class of a token, as a TextMate scope.
tokenClass :: LexTokenKind -> Text
tokenClass kind =
  case kind of
    TkVarId {} -> ""
    TkQVarId {} -> ""
    TkConId {} -> "entity.name.type"
    TkQConId {} -> "entity.name.type"
    TkVarSym {} -> "keyword.operator"
    TkQVarSym {} -> "keyword.operator"
    TkConSym {} -> "keyword.operator"
    TkQConSym {} -> "keyword.operator"
    TkInteger {} -> "constant.numeric"
    TkFloat {} -> "constant.numeric"
    TkChar {} -> "string"
    TkCharHash {} -> "string"
    TkString {} -> "string"
    TkStringHash {} -> "string"
    TkPragma {} -> "meta.preprocessor"
    TkPragmaOpen {} -> "meta.preprocessor"
    TkPragmaClose {} -> "meta.preprocessor"
    TkLineComment {} -> "comment"
    TkBlockComment {} -> "comment"
    TkError {} -> "invalid"
    TkReservedDotDot {} -> "keyword.operator"
    TkReservedColon {} -> "keyword.operator"
    TkReservedDoubleColon {} -> "keyword.operator"
    TkReservedEquals {} -> "keyword.operator"
    TkReservedBackslash {} -> "keyword.operator"
    TkReservedPipe {} -> "keyword.operator"
    TkReservedLeftArrow {} -> "keyword.operator"
    TkReservedRightArrow {} -> "keyword.operator"
    TkReservedAt {} -> "keyword.operator"
    TkReservedDoubleArrow {} -> "keyword.operator"
    _
      | isKeyword kind -> "keyword"
      | otherwise -> ""

isKeyword :: LexTokenKind -> Bool
isKeyword kind =
  case kind of
    TkKeywordBy {} -> True
    TkKeywordCase {} -> True
    TkKeywordClass {} -> True
    TkKeywordData {} -> True
    TkKeywordDefault {} -> True
    TkKeywordDeriving {} -> True
    TkKeywordDo {} -> True
    TkKeywordElse {} -> True
    TkKeywordForall {} -> True
    TkKeywordForeign {} -> True
    TkKeywordIf {} -> True
    TkKeywordImport {} -> True
    TkKeywordIn {} -> True
    TkKeywordInfix {} -> True
    TkKeywordInfixl {} -> True
    TkKeywordInfixr {} -> True
    TkKeywordInstance {} -> True
    TkKeywordLet {} -> True
    TkKeywordMdo {} -> True
    TkKeywordModule {} -> True
    TkKeywordNewtype {} -> True
    TkKeywordOf {} -> True
    TkKeywordPattern {} -> True
    TkKeywordProc {} -> True
    TkKeywordRec {} -> True
    TkKeywordThen {} -> True
    TkKeywordType {} -> True
    TkKeywordUsing {} -> True
    TkKeywordWhere {} -> True
    TkQualifiedDo {} -> True
    TkQualifiedMdo {} -> True
    _ -> False
