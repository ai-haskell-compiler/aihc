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
-- source 'ByteString', returning a 'Step' that either completes with a
-- 'Result' or requests an include file to be resolved.
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

import Aihc.Cpp.Cursor
  ( Cursor (..),
    atEnd,
    findNewline,
    fromByteString,
    lineSlice,
    peekByte,
    peekByteAt,
    skipNewline,
    skipWhile,
    toText,
  )
import Aihc.Cpp.Evaluator (evalCondition)
import Aihc.Cpp.Parser (Directive (..), parseDirective)
import Aihc.Cpp.Scanner (expandLineBySpanMultiline, lineScanFinalCDepth, lineScanFinalHsDepth, lineScanSpans, scanLine, scanLineDepthOnly)
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
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
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

-- | Preprocess C preprocessor directives in the input.
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
-- file as a 'ByteString':
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
preprocess :: Config -> ByteString -> Step
preprocess cfg input =
  let cursor = fromByteString input
   in processFile (configInputFile cfg) cursor False [] 1 initialState finish
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

-- | Find the next line in the cursor, handling backslash-continuation
-- for directive lines. Returns (lineCursor, lineSpan, restCursor) where:
--   * lineCursor is a sub-cursor bounded to the logical line content
--   * lineSpan is the number of physical lines consumed (>= 1)
--   * restCursor is positioned after the line (past the newline)
--
-- Backslash-continuation is only applied when the line starts with '#'
-- (after optional whitespace), matching CPP semantics.
-- For continuation lines, a new ByteString is allocated with the
-- backslash-newline sequences removed.
nextLine :: Cursor -> (Cursor, Int, Cursor)
nextLine cur =
  let eol = findNewline cur
      lineStart = curPos cur
      lineEnd = curPos eol
      rest = fromMaybe eol (skipNewline eol)
   in if lineEnd > lineStart
        && peekByteAt (lineEnd - 1) cur == Just 0x5C -- '\' at end
        && isDirectiveLine cur lineEnd
        then -- Backslash continuation: join lines, stripping '\' and '\n'
          joinContinuationLines cur lineStart lineEnd rest
        else (lineSlice lineEnd cur, 1, rest)

-- | Check if the bytes from curPos to lineEnd start with '#' after
-- optional whitespace. This determines whether backslash-continuation
-- applies.
isDirectiveLine :: Cursor -> Int -> Bool
isDirectiveLine cur lineEnd =
  let trimmed = skipWhile isSpaceOrTab cur
   in curPos trimmed < lineEnd
        && peekByte trimmed == Just 0x23 -- '#'
  where
    isSpaceOrTab b = b == 0x20 || b == 0x09

-- | Join backslash-continuation lines into a single logical line.
-- Builds a new ByteString with '\<newline>' sequences removed.
-- Returns (joinedCursor, physicalLineCount, restCursor).
joinContinuationLines :: Cursor -> Int -> Int -> Cursor -> (Cursor, Int, Cursor)
joinContinuationLines origCur lineStart firstLineEnd firstRest =
  let buf = curBuf origCur
      -- First segment: from lineStart to firstLineEnd - 1 (exclude '\')
      firstSegment = BSB.byteString (sliceBS lineStart (firstLineEnd - 1) buf)
   in go firstSegment 1 firstRest
  where
    go !acc !spanCount !rest
      | atEnd rest =
          -- No more input; finalize
          let joined = BSL.toStrict (BSB.toLazyByteString acc)
           in (fromByteString joined, spanCount, rest)
      | otherwise =
          let eol = findNewline rest
              segStart = curPos rest
              segEnd = curPos eol
              rest' = fromMaybe eol (skipNewline eol)
           in if segEnd > segStart
                && peekByteAt (segEnd - 1) origCur == Just 0x5C -- ends with '\'
                then
                  -- Another continuation line: append without the trailing '\'
                  let segment = BSB.byteString (sliceBS segStart (segEnd - 1) (curBuf origCur))
                   in go (acc <> segment) (spanCount + 1) rest'
                else
                  -- Last line of continuation
                  let segment = BSB.byteString (sliceBS segStart segEnd (curBuf origCur))
                      joined = BSL.toStrict (BSB.toLazyByteString (acc <> segment))
                   in (fromByteString joined, spanCount + 1, rest')

-- | Slice a ByteString from position @start@ to @end@ (exclusive).
sliceBS :: Int -> Int -> ByteString -> ByteString
sliceBS start end bs = BS.take (end - start) (BS.drop start bs)
{-# INLINE sliceBS #-}

-- | Process a file from a cursor. The @trailingNewline@ flag controls
-- whether a trailing newline in the input produces an extra empty line
-- (used for include files to match @splitOn \"\\n\"@ semantics).
processFile :: FilePath -> Cursor -> Bool -> [CondFrame] -> Int -> EngineState -> Continuation -> Step
processFile _ cursor trailingNl _ _ st k
  | atEnd cursor =
      if trailingNl
        then -- Include file: trailing newline produces one more empty line
          k (emitBlankLines 1 st)
        else k st
processFile filePath cursor trailingNl stack !lineNo st k =
  let (lineCur, lineSpan, restCursor) = nextLine cursor
      -- Detect if this line was followed by a newline (vs EOF).
      -- If so and restCursor is at EOF, the file had a trailing newline.
      hasTrailingNl = trailingNl && not (atEnd restCursor) || (trailingNl && atEnd restCursor)
      -- Actually: trailingNl flag is set at processFile entry for includes.
      -- We just propagate it. The check at atEnd above handles the final empty line.
      lineText = toText lineCur
      startsInBlockComment = stHsBlockCommentDepth st > 0 || stCBlockCommentDepth st > 0
      parsedDirective =
        if startsInBlockComment
          then Nothing
          else parseDirective lineText
      isActive = currentActive stack
      nextLineNo = lineNo + lineSpan
   in if not isActive && not (stSkippingDanglingElse st)
        then -- === Fast path for inactive branches ===
        -- Only track comment depth; skip full span scanning and macro expansion.
          let (finalHs, finalC) = scanLineDepthOnly (stHsBlockCommentDepth st) (stCBlockCommentDepth st) lineCur
              advanceSt st' =
                st'
                  { stCurrentLine = nextLineNo,
                    stHsBlockCommentDepth = finalHs,
                    stCBlockCommentDepth = finalC
                  }
              continueInactive st' = processFile filePath restCursor hasTrailingNl stack nextLineNo (advanceSt st') k
              continueInactiveWith stack' st' = processFile filePath restCursor hasTrailingNl stack' nextLineNo (advanceSt st') k
           in case parsedDirective of
                Nothing ->
                  continueInactive (emitBlankLines lineSpan st)
                Just directive ->
                  let ctx =
                        LineContext
                          { lcFilePath = filePath,
                            lcLineNo = lineNo,
                            lcLineSpan = lineSpan,
                            lcNextLineNo = nextLineNo,
                            lcRestCursor = restCursor,
                            lcStack = stack,
                            lcContinue = continueInactive,
                            lcContinueWith = continueInactiveWith,
                            lcDone = k
                          }
                   in handleDirective ctx st directive
        else -- === Normal path: full scan + expansion ===
          let lineScan = scanLine (stHsBlockCommentDepth st) (stCBlockCommentDepth st) lineCur
              advanceLineState st' =
                st'
                  { stCurrentLine = nextLineNo,
                    stHsBlockCommentDepth = lineScanFinalHsDepth lineScan,
                    stCBlockCommentDepth = lineScanFinalCDepth lineScan
                  }
              continue st' = processFile filePath restCursor hasTrailingNl stack nextLineNo (advanceLineState st') k
              continueWith stack' st' = processFile filePath restCursor hasTrailingNl stack' nextLineNo (advanceLineState st') k
              ctx =
                LineContext
                  { lcFilePath = filePath,
                    lcLineNo = lineNo,
                    lcLineSpan = lineSpan,
                    lcNextLineNo = nextLineNo,
                    lcRestCursor = restCursor,
                    lcStack = stack,
                    lcContinue = continue,
                    lcContinueWith = continueWith,
                    lcDone = k
                  }
           in if stSkippingDanglingElse st
                then recoverDanglingElse ctx parsedDirective st
                else case parsedDirective of
                  Nothing ->
                    -- Try multi-line expansion: look ahead at future lines via cursor
                    let (expanded, extraConsumed) =
                          expandLineBySpanMultiline st (lineScanSpans lineScan) restCursor
                     in if extraConsumed > 0
                          then
                            -- Skip consumed continuation lines by advancing the cursor
                            let (remainingCursor, totalExtraSpan) =
                                  skipNLines extraConsumed restCursor
                                -- Scan consumed lines to update comment depths
                                (finalHs, finalC) =
                                  scanConsumedLines
                                    (lineScanFinalHsDepth lineScan)
                                    (lineScanFinalCDepth lineScan)
                                    restCursor
                                    extraConsumed
                                nextLineNo' = nextLineNo + totalExtraSpan
                                st' =
                                  (emitLine expanded st)
                                    { stCurrentLine = nextLineNo',
                                      stHsBlockCommentDepth = finalHs,
                                      stCBlockCommentDepth = finalC
                                    }
                             in processFile filePath remainingCursor hasTrailingNl stack nextLineNo' st' k
                          else continue (emitLine expanded st)
                  Just directive ->
                    handleDirective ctx st directive

-- | Skip N lines from a cursor, returning (rest cursor, total lines skipped).
skipNLines :: Int -> Cursor -> (Cursor, Int)
skipNLines 0 cur = (cur, 0)
skipNLines n cur = go 0 0 cur
  where
    go !count !consumed !c
      | count >= n = (c, consumed)
      | atEnd c = (c, consumed)
      | otherwise =
          let eol = findNewline c
              rest = fromMaybe eol (skipNewline eol)
           in go (count + 1) (consumed + 1) rest

-- | Scan consumed lines to update comment depths.
scanConsumedLines :: Int -> Int -> Cursor -> Int -> (Int, Int)
scanConsumedLines !hsDepth !cDepth _cur 0 = (hsDepth, cDepth)
scanConsumedLines !hsDepth !cDepth cur remaining
  | atEnd cur = (hsDepth, cDepth)
  | otherwise =
      let eol = findNewline cur
          lineCur = lineSlice (curPos eol) cur
          (hsDepth', cDepth') = scanLineDepthOnly hsDepth cDepth lineCur
          rest = fromMaybe eol (skipNewline eol)
       in scanConsumedLines hsDepth' cDepth' rest (remaining - 1)

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
    DirPragmaOnce ->
      handlePragmaOnceDirective ctx st
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

handlePragmaOnceDirective :: LineContext -> EngineState -> Step
handlePragmaOnceDirective ctx st =
  if currentActive (lcStack ctx)
    then continueBlank ctx (st {stPragmaOnceFiles = S.insert (lcFilePath ctx) (stPragmaOnceFiles st)})
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
  | S.member includeFilePath (stPragmaOnceFiles st) = continueBlank ctx st
  | otherwise = NeedInclude includeReq nextStep
  where
    includePathText = T.unpack includeTarget
    includeFilePath =
      case kind of
        IncludeLocal -> takeDirectory (lcFilePath ctx) </> includePathText
        IncludeSystem -> includePathText
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
    nextStep (Just includeBytes) =
      let includeCursor = fromByteString includeBytes
          -- Include files treat trailing newlines as producing an extra
          -- empty line (matching splitOn "\n" semantics).
          includeHasTrailingNl = not (BS.null includeBytes) && BS.last includeBytes == 0x0A
          stWithIncludePragma =
            emitLine (linePragma 1 includeFilePath) (st {stCurrentFile = includeFilePath, stCurrentLine = 1})
          resumeParent stAfterInclude =
            processFile
              (lcFilePath ctx)
              (lcRestCursor ctx)
              False
              (lcStack ctx)
              (lcNextLineNo ctx)
              ( emitLine
                  (linePragma (lcNextLineNo ctx) (lcFilePath ctx))
                  (stAfterInclude {stCurrentFile = lcFilePath ctx, stCurrentLine = lcNextLineNo ctx})
              )
              (lcDone ctx)
       in processFile includeFilePath includeCursor includeHasTrailingNl [] 1 stWithIncludePragma resumeParent

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
            (lcRestCursor ctx)
            False
            (lcStack ctx)
            lineNumber
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
