{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Cpp
-- Description : Pure Haskell C preprocessor for Haskell source files
-- License     : Unlicense
--
-- This module provides a C preprocessor implementation designed for
-- preprocessing Haskell source files that use CPP extensions.
--
-- The main entry point is 'preprocess', which takes a 'Config' and
-- source text, returning a 'Step' that either completes with a 'Result'
-- or requests an include file to be resolved.
module Aihc.Cpp
  ( -- * Preprocessing
    preprocess,

    -- * Configuration
    Config (..),
    defaultConfig,

    -- * Results
    Step (..),
    Result (..),

    -- * Include Handling
    IncludeRequest (..),
    IncludeKind (..),

    -- * Diagnostics
    Diagnostic (..),
    Severity (..),
  )
where

import Aihc.Cpp.Evaluator (evalCondition)
import Aihc.Cpp.Parser (Directive (..), parseDirective)
import Aihc.Cpp.Scanner (expandLineBySpanMultiline, lineScanFinalCDepth, lineScanFinalHsDepth, lineScanSpans, scanLine)
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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import System.FilePath (takeDirectory, (</>))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.Text.IO as T

-- | Preprocess C preprocessor directives in the input text.
--
-- This function handles:
--
-- * Macro definitions (@#define@) and expansion
-- * Conditional compilation (@#if@, @#ifdef@, @#ifndef@, @#elif@, @#else@, @#endif@)
-- * File inclusion (@#include@)
-- * Diagnostics (@#warning@, @#error@)
-- * Line control (@#line@)
-- * Predefined macros (@__FILE__@, @__LINE__@, @__DATE__@, @__TIME__@)
--
-- === Macro expansion
--
-- Object-like macros are expanded in the output:
--
-- >>> let Done r = preprocess defaultConfig "#define FOO 42\nThe answer is FOO"
-- >>> T.putStr (resultOutput r)
-- #line 1 "<input>"
-- <BLANKLINE>
-- The answer is 42
--
-- Function-like macros are also supported:
--
-- >>> let Done r = preprocess defaultConfig "#define MAX(a,b) ((a) > (b) ? (a) : (b))\nMAX(3, 5)"
-- >>> T.putStr (resultOutput r)
-- #line 1 "<input>"
-- <BLANKLINE>
-- ((3) > (5) ? (3) : (5))
--
-- === Conditional compilation
--
-- Conditional directives control which sections of code are included:
--
-- >>> :{
-- let Done r = preprocess defaultConfig
--       "#define DEBUG 1\n#if DEBUG\ndebug mode\n#else\nrelease mode\n#endif"
-- in T.putStr (resultOutput r)
-- :}
-- #line 1 "<input>"
-- <BLANKLINE>
-- <BLANKLINE>
-- debug mode
-- <BLANKLINE>
-- <BLANKLINE>
-- <BLANKLINE>
--
-- === Include handling
--
-- When an @#include@ directive is encountered, 'preprocess' returns a
-- 'NeedInclude' step. The caller must provide the contents of the included
-- file:
--
-- >>> :{
-- let NeedInclude req k = preprocess defaultConfig "#include \"header.h\"\nmain code"
--     Done r = k (Just "-- header content")
-- in T.putStr (resultOutput r)
-- :}
-- #line 1 "<input>"
-- #line 1 "./header.h"
-- -- header content
-- #line 2 "<input>"
-- main code
--
-- If the include file is not found, pass 'Nothing' to emit an error:
--
-- >>> :{
-- let NeedInclude _ k = preprocess defaultConfig "#include \"missing.h\""
--     Done r = k Nothing
-- in do
--   T.putStr (resultOutput r)
--   mapM_ print (resultDiagnostics r)
-- :}
-- #line 1 "<input>"
-- Diagnostic {diagSeverity = Error, diagMessage = "missing include: missing.h", diagFile = "<input>", diagLine = 1}
--
-- === Diagnostics
--
-- The @#warning@ directive emits a warning:
--
-- >>> :{
-- let Done r = preprocess defaultConfig "#warning This is a warning"
-- in do
--   T.putStr (resultOutput r)
--   mapM_ print (resultDiagnostics r)
-- :}
-- #line 1 "<input>"
-- <BLANKLINE>
-- Diagnostic {diagSeverity = Warning, diagMessage = "This is a warning", diagFile = "<input>", diagLine = 1}
--
-- The @#error@ directive emits an error and stops preprocessing:
--
-- >>> :{
-- let Done r = preprocess defaultConfig "#error Build failed\nthis line is not processed"
-- in do
--   T.putStr (resultOutput r)
--   mapM_ print (resultDiagnostics r)
-- :}
-- #line 1 "<input>"
-- <BLANKLINE>
-- Diagnostic {diagSeverity = Error, diagMessage = "Build failed", diagFile = "<input>", diagLine = 1}
preprocess :: Config -> Text -> Step
preprocess cfg input =
  processFile (configInputFile cfg) (splitInputLines 1 input) [] initialState finish
  where
    initialState =
      let st0 = emitLine (linePragma 1 (configInputFile cfg)) (emptyState (configInputFile cfg))
       in st0
            { stMacros = M.map ObjectMacro (configMacros cfg)
            }

    finish st =
      let out = TL.toStrict (TB.toLazyText (stOutput st))
          outWithTrailingNewline =
            if T.null out
              then out
              else out <> "\n"
       in Done
            Result
              { resultOutput = outWithTrailingNewline,
                resultDiagnostics = reverse (stDiagnosticsRev st)
              }

-- | Split input text into lines, joining backslash-continuation lines
-- for directives. Each entry is (lineNo, lineSpan, lineText).
-- Walks the text directly without an intermediate @[Text]@ list.
-- Behaves like @T.lines@ — a trailing newline does NOT produce an
-- empty trailing entry.
splitInputLines :: Int -> Text -> [(Int, Int, Text)]
splitInputLines _ txt | T.null txt = []
splitInputLines n txt =
  let (line, rest) = T.break (== '\n') txt
      rest' = if T.null rest then rest else T.drop 1 rest -- skip the '\n'
   in if "\\" `T.isSuffixOf` line && "#" `T.isPrefixOf` T.stripStart line
        then
          let (content, remaining, extraLines) = pullContinuation (T.init line) rest'
              spanLen = extraLines + 1
           in (n, spanLen, content) : splitInputLines (n + spanLen) remaining
        else (n, 1, line) : splitInputLines (n + 1) rest'

-- | Like 'splitInputLines' but behaves like @T.splitOn \"\\n\"@ —
-- a trailing newline DOES produce an empty trailing entry.
-- Used for @#include@ file content where the trailing newline represents
-- an additional source line.
splitIncludeLines :: Int -> Text -> [(Int, Int, Text)]
splitIncludeLines _ txt | T.null txt = []
splitIncludeLines n txt =
  let (line, rest) = T.break (== '\n') txt
   in if T.null rest
        then -- No newline found: this is the last segment
          if "\\" `T.isSuffixOf` line && "#" `T.isPrefixOf` T.stripStart line
            then
              let (content, _, extraLines) = pullContinuation (T.init line) T.empty
                  spanLen = extraLines + 1
               in [(n, spanLen, content)]
            else [(n, 1, line)]
        else
          let rest' = T.drop 1 rest -- skip the '\n'
           in if "\\" `T.isSuffixOf` line && "#" `T.isPrefixOf` T.stripStart line
                then
                  let (content, remaining, extraLines) = pullContinuation (T.init line) rest'
                      spanLen = extraLines + 1
                   in (n, spanLen, content) : splitIncludeLines (n + spanLen) remaining
                else -- Include the empty trailing entry for a final newline
                  (n, 1, line) : splitIncludeLinesAfterNewline (n + 1) rest'

-- | Helper for 'splitIncludeLines' that handles the case after a
-- newline has been consumed. Unlike the main function, an empty
-- input here produces a single empty entry (representing the line
-- after a trailing newline).
splitIncludeLinesAfterNewline :: Int -> Text -> [(Int, Int, Text)]
splitIncludeLinesAfterNewline n txt
  | T.null txt = [(n, 1, "")]
  | otherwise = splitIncludeLines n txt

pullContinuation :: Text -> Text -> (Text, Text, Int)
pullContinuation acc remaining
  | T.null remaining = (acc, remaining, 0)
  | otherwise =
      let (nextLine, rest2) = T.break (== '\n') remaining
          rest2' = if T.null rest2 then rest2 else T.drop 1 rest2
       in if "\\" `T.isSuffixOf` nextLine
            then
              let (res, r, c) = pullContinuation (acc <> T.init nextLine) rest2'
               in (res, r, c + 1)
            else (acc <> nextLine, rest2', 1)

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
              then
                -- Try multi-line expansion: look ahead at future lines
                let futureCodeLines = [l | (_, _, l) <- restLines]
                    (expanded, extraConsumed) =
                      expandLineBySpanMultiline st (lineScanSpans lineScan) futureCodeLines
                 in if extraConsumed > 0
                      then
                        -- Skip consumed continuation lines
                        let consumed = take extraConsumed restLines
                            remaining = drop extraConsumed restLines
                            -- Scan consumed lines to update comment depths
                            (finalHs, finalC) =
                              foldl'
                                ( \(!hs, !c) (_, _, l) ->
                                    let ls = scanLine hs c l
                                     in (lineScanFinalHsDepth ls, lineScanFinalCDepth ls)
                                )
                                (lineScanFinalHsDepth lineScan, lineScanFinalCDepth lineScan)
                                consumed
                            nextLineNo' = case remaining of
                              (n, _, _) : _ -> n
                              [] -> lineNo + lineSpan + sum [s | (_, s, _) <- consumed]
                            st' =
                              (emitLine expanded st)
                                { stCurrentLine = nextLineNo',
                                  stHsBlockCommentDepth = finalHs,
                                  stCBlockCommentDepth = finalC
                                }
                         in processFile filePath remaining stack st' k
                      else continue (emitLine expanded st)
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
       in processFile includeFilePath (splitIncludeLines 1 includeText) [] stWithIncludePragma resumeParent

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
emitLine line st =
  let sep = if stOutputLineCount st > 0 then TB.singleton '\n' else mempty
   in st
        { stOutput = stOutput st <> sep <> TB.fromText line,
          stOutputLineCount = stOutputLineCount st + 1
        }

emitBlankLines :: Int -> EngineState -> EngineState
emitBlankLines n st
  | n <= 0 = st
  | otherwise =
      let newlines = mconcat (replicate n (TB.singleton '\n'))
       in st
            { stOutput = stOutput st <> newlines,
              stOutputLineCount = stOutputLineCount st + n
            }

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
