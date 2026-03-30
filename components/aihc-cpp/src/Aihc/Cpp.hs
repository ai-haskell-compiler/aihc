{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Cpp
-- Description : Pure Haskell C preprocessor for Haskell source files
-- License     : Unlicense
module Aihc.Cpp
  ( preprocess,
    Config (..),
    defaultConfig,
    Step (..),
    Result (..),
    IncludeRequest (..),
    IncludeKind (..),
    Diagnostic (..),
    Severity (..),
  )
where

import Aihc.Cpp.Evaluator (evalCondition)
import Aihc.Cpp.Parser (Directive (..), parseDirective)
import Aihc.Cpp.Scanner (expandLineBySpan, lineScanFinalCDepth, lineScanFinalHsDepth, lineScanSpans, scanLine)
import Aihc.Cpp.Types
  ( CondFrame (..),
    Config (..),
    Continuation,
    Diagnostic (..),
    EngineState (..),
    IncludeKind (..),
    IncludeRequest (..),
    LineContext (..),
    MacroDef (..),
    Result (..),
    Severity (..),
    Step (..),
    currentActive,
    defaultConfig,
    emptyState,
    mkFrame,
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory, (</>))

preprocess :: Config -> Text -> Step
preprocess cfg input =
  processFile (configInputFile cfg) (joinMultiline 1 (T.lines input)) [] initialState finish
  where
    initialState =
      let st0 = emitLine (linePragma 1 (configInputFile cfg)) (emptyState (configInputFile cfg))
       in st0
            { stMacros = M.map ObjectMacro (configMacros cfg)
            }

    finish st =
      let out = T.intercalate "\n" (reverse (stOutputRev st))
          outWithTrailingNewline =
            if T.null out
              then out
              else out <> "\n"
       in Done
            Result
              { resultOutput = outWithTrailingNewline,
                resultDiagnostics = reverse (stDiagnosticsRev st)
              }

joinMultiline :: Int -> [Text] -> [(Int, Int, Text)]
joinMultiline _ [] = []
joinMultiline n (l : ls)
  | "\\" `T.isSuffixOf` l && "#" `T.isPrefixOf` T.stripStart l =
      let (content, rest, extraLines) = pull (T.init l) ls
          spanLen = extraLines + 1
       in (n, spanLen, content) : joinMultiline (n + spanLen) rest
  | otherwise = (n, 1, l) : joinMultiline (n + 1) ls
  where
    pull acc [] = (acc, [], 0)
    pull acc (x : xs)
      | "\\" `T.isSuffixOf` x =
          let (res, r, c) = pull (acc <> T.init x) xs
           in (res, r, c + 1)
      | otherwise = (acc <> x, xs, 1)

splitLines :: Text -> [Text]
splitLines txt
  | T.null txt = []
  | otherwise = T.splitOn "\n" txt

processFile :: FilePath -> [(Int, Int, Text)] -> [CondFrame] -> EngineState -> Continuation -> Step
processFile _ [] _ st k = k st
processFile filePath ((lineNo, lineSpan, line) : restLines) stack st k =
  let lineScan = scanLine (stHsBlockCommentDepth st) (stCBlockCommentDepth st) line
      startsInBlockComment = stHsBlockCommentDepth st > 0 || stCBlockCommentDepth st > 0
      parsedDirective =
        if startsInBlockComment
          then Nothing
          else parseDirective line
      nextLineNo = case restLines of
        (n, _, _) : _ -> n
        [] -> lineNo + lineSpan
      advanceLineState st' =
        st'
          { stCurrentLine = nextLineNo,
            stHsBlockCommentDepth = lineScanFinalHsDepth lineScan,
            stCBlockCommentDepth = lineScanFinalCDepth lineScan
          }
      continue st' = processFile filePath restLines stack (advanceLineState st') k
      continueWith stack' st' = processFile filePath restLines stack' (advanceLineState st') k
      ctx =
        LineContext
          { lcFilePath = filePath,
            lcLineNo = lineNo,
            lcLineSpan = lineSpan,
            lcNextLineNo = nextLineNo,
            lcRestLines = restLines,
            lcStack = stack,
            lcContinue = continue,
            lcContinueWith = continueWith,
            lcDone = k
          }
   in if stSkippingDanglingElse st
        then recoverDanglingElse ctx parsedDirective st
        else case parsedDirective of
          Nothing ->
            if currentActive stack
              then continue (emitLine (expandLineBySpan st (lineScanSpans lineScan)) st)
              else continue (emitBlankLines lineSpan st)
          Just directive ->
            handleDirective ctx st directive

recoverDanglingElse :: LineContext -> Maybe Directive -> EngineState -> Step
recoverDanglingElse ctx parsedDirective st =
  case parsedDirective of
    Just DirEndIf ->
      lcContinue
        ctx
        ( addDiag
            Warning
            "unmatched #endif"
            (lcFilePath ctx)
            (lcLineNo ctx)
            (st {stSkippingDanglingElse = False})
        )
    Just _ ->
      lcContinue ctx st
    Nothing ->
      lcContinue ctx (emitDirectiveBlank ctx st)

handleDirective :: LineContext -> EngineState -> Directive -> Step
handleDirective ctx st directive =
  case directive of
    DirDefineObject name value ->
      mutateMacrosWhenActive ctx st (M.insert name (ObjectMacro value))
    DirDefineFunction name params body ->
      mutateMacrosWhenActive ctx st (M.insert name (FunctionMacro params body))
    DirUndef name ->
      mutateMacrosWhenActive ctx st (M.delete name)
    DirInclude kind includeTarget ->
      handleIncludeDirective ctx st kind includeTarget
    DirIf expr ->
      pushConditionalFrame ctx st (evalCondition st expr)
    DirIfDef name ->
      pushConditionalFrame ctx st (M.member name (stMacros st))
    DirIfNDef name ->
      pushConditionalFrame ctx st (not (M.member name (stMacros st)))
    DirElif expr ->
      handleElifDirective ctx st expr
    DirElse ->
      handleElseDirective ctx st
    DirEndIf ->
      case lcStack ctx of
        [] ->
          lcContinue
            ctx
            (addDiag Warning "unmatched #endif" (lcFilePath ctx) (lcLineNo ctx) st)
        _ : rest ->
          continueBlankWithStack ctx rest st
    DirWarning msg ->
      addDiagnosticWhenActive ctx Warning msg st
    DirError msg ->
      if currentActive (lcStack ctx)
        then
          lcDone
            ctx
            (emitDirectiveBlank ctx (addDiag Error msg (lcFilePath ctx) (lcLineNo ctx) st))
        else continueBlank ctx st
    DirLine n mPath ->
      handleLineDirective ctx st n mPath
    DirUnsupported name ->
      addDiagnosticWhenActive ctx Warning ("unsupported directive: " <> name) st

emitDirectiveBlank :: LineContext -> EngineState -> EngineState
emitDirectiveBlank ctx = emitBlankLines (lcLineSpan ctx)

continueBlank :: LineContext -> EngineState -> Step
continueBlank ctx st = lcContinue ctx (emitDirectiveBlank ctx st)

continueBlankWithStack :: LineContext -> [CondFrame] -> EngineState -> Step
continueBlankWithStack ctx stack st = lcContinueWith ctx stack (emitDirectiveBlank ctx st)

mutateMacrosWhenActive :: LineContext -> EngineState -> (Map Text MacroDef -> Map Text MacroDef) -> Step
mutateMacrosWhenActive ctx st mutate =
  if currentActive (lcStack ctx)
    then continueBlank ctx (st {stMacros = mutate (stMacros st)})
    else continueBlank ctx st

addDiagnosticWhenActive :: LineContext -> Severity -> Text -> EngineState -> Step
addDiagnosticWhenActive ctx severity message st =
  if currentActive (lcStack ctx)
    then continueBlank ctx (addDiag severity message (lcFilePath ctx) (lcLineNo ctx) st)
    else continueBlank ctx st

pushConditionalFrame :: LineContext -> EngineState -> Bool -> Step
pushConditionalFrame ctx st cond =
  let frame = mkFrame (currentActive (lcStack ctx)) cond
   in continueBlankWithStack ctx (frame : lcStack ctx) st

handleElifDirective :: LineContext -> EngineState -> Text -> Step
handleElifDirective ctx st expr =
  case lcStack ctx of
    [] ->
      continueBlank
        ctx
        (addDiag Error "#elif without matching #if" (lcFilePath ctx) (lcLineNo ctx) st)
    f : rest ->
      if frameInElse f
        then
          continueBlank
            ctx
            (addDiag Error "#elif after #else" (lcFilePath ctx) (lcLineNo ctx) st)
        else
          let anyTaken = frameConditionTrue f
              newCond = not anyTaken && evalCondition st expr
              f' =
                f
                  { frameConditionTrue = anyTaken || newCond,
                    frameCurrentActive = frameOuterActive f && newCond
                  }
           in continueBlankWithStack ctx (f' : rest) st

handleElseDirective :: LineContext -> EngineState -> Step
handleElseDirective ctx st =
  case lcStack ctx of
    [] ->
      lcContinue ctx (st {stSkippingDanglingElse = True})
    f : rest ->
      if frameInElse f
        then
          continueBlank
            ctx
            (addDiag Error "duplicate #else in conditional block" (lcFilePath ctx) (lcLineNo ctx) st)
        else
          let newCurrent = frameOuterActive f && not (frameConditionTrue f)
              f' =
                f
                  { frameInElse = True,
                    frameCurrentActive = newCurrent
                  }
           in continueBlankWithStack ctx (f' : rest) st

handleIncludeDirective :: LineContext -> EngineState -> IncludeKind -> Text -> Step
handleIncludeDirective ctx st kind includeTarget
  | not (currentActive (lcStack ctx)) = continueBlank ctx st
  | otherwise = NeedInclude includeReq nextStep
  where
    includePathText = T.unpack includeTarget
    includeReq =
      IncludeRequest
        { includePath = includePathText,
          includeKind = kind,
          includeFrom = lcFilePath ctx,
          includeLine = lcLineNo ctx
        }

    nextStep Nothing =
      lcContinue
        ctx
        (addDiag Error ("missing include: " <> includeTarget) (lcFilePath ctx) (lcLineNo ctx) st)
    nextStep (Just includeText) =
      let includeFilePath =
            case kind of
              IncludeLocal -> takeDirectory (lcFilePath ctx) </> includePathText
              IncludeSystem -> includePathText
          stWithIncludePragma =
            emitLine (linePragma 1 includeFilePath) (st {stCurrentFile = includeFilePath, stCurrentLine = 1})
          resumeParent stAfterInclude =
            processFile
              (lcFilePath ctx)
              (lcRestLines ctx)
              (lcStack ctx)
              ( emitLine
                  (linePragma (lcNextLineNo ctx) (lcFilePath ctx))
                  (stAfterInclude {stCurrentFile = lcFilePath ctx, stCurrentLine = lcNextLineNo ctx})
              )
              (lcDone ctx)
       in processFile includeFilePath (joinMultiline 1 (splitLines includeText)) [] stWithIncludePragma resumeParent

handleLineDirective :: LineContext -> EngineState -> Int -> Maybe FilePath -> Step
handleLineDirective ctx st lineNumber maybePath
  | not (currentActive (lcStack ctx)) = continueBlank ctx st
  | otherwise =
      let stWithFile =
            case maybePath of
              Just path -> st {stCurrentFile = path}
              Nothing -> st
          stWithLinePragma =
            emitLine
              (linePragma lineNumber (stCurrentFile stWithFile))
              (stWithFile {stCurrentLine = lineNumber})
       in processFile
            (lcFilePath ctx)
            (lcRestLines ctx)
            (lcStack ctx)
            (stWithLinePragma {stCurrentLine = lineNumber})
            (lcDone ctx)

emitLine :: Text -> EngineState -> EngineState
emitLine line st = st {stOutputRev = line : stOutputRev st}

emitBlankLines :: Int -> EngineState -> EngineState
emitBlankLines n st
  | n <= 0 = st
  | otherwise = st {stOutputRev = replicate n "" <> stOutputRev st}

addDiag :: Severity -> Text -> FilePath -> Int -> EngineState -> EngineState
addDiag sev msg filePath lineNo st =
  st
    { stDiagnosticsRev =
        Diagnostic
          { diagSeverity = sev,
            diagMessage = msg,
            diagFile = filePath,
            diagLine = lineNo
          }
          : stDiagnosticsRev st
    }

linePragma :: Int -> FilePath -> Text
linePragma n path = "#line " <> T.pack (show n) <> " \"" <> T.pack path <> "\""
