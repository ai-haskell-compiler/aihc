{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cpp
-- Description : Pure Haskell C preprocessor for Haskell source files
-- License     : Unlicense
--
-- This module provides a C preprocessor implementation designed for
-- preprocessing Haskell source files that use CPP extensions.
--
-- The main entry point is 'preprocess', which takes a 'Config' and
-- source text, returning a 'Step' that either completes with a 'Result'
-- or requests an include file to be resolved.
module Cpp
  ( -- * Preprocessing
    preprocess,

    -- * Configuration
    Config (..),
    defaultConfig,

    -- * Results
    Step (..),
    Result (..),

    -- * Include handling
    IncludeRequest (..),
    IncludeKind (..),

    -- * Diagnostics
    Diagnostic (..),
    Severity (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR
import GHC.Generics (Generic)
import System.FilePath (takeDirectory, (</>))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.Text.IO as T

-- | Configuration for the C preprocessor.
data Config = Config
  { -- | The name of the input file, used in @#line@ directives and
    -- @__FILE__@ expansion.
    configInputFile :: FilePath,
    -- | User-defined macros. These are expanded as object-like macros.
    -- Note that the values should include any necessary quoting. For
    -- example, to define a string macro, use @"\"value\""@.
    configMacros :: !(Map Text Text)
  }

data MacroDef
  = ObjectMacro !Text
  | FunctionMacro ![Text] !Text
  deriving (Eq, Show)

-- | Default configuration with sensible defaults.
--
-- * 'configInputFile' is set to @\"\<input\>\"@
-- * 'configMacros' includes @__DATE__@ and @__TIME__@ set to the Unix epoch
--
-- To customize the date and time macros:
--
-- >>> import qualified Data.Map.Strict as M
-- >>> let cfg = defaultConfig { configMacros = M.fromList [("__DATE__", "\"Mar 15 2026\""), ("__TIME__", "\"14:30:00\"")] }
-- >>> configMacros cfg
-- fromList [("__DATE__","\"Mar 15 2026\""),("__TIME__","\"14:30:00\"")]
--
-- To add additional macros while keeping the defaults:
--
-- >>> import qualified Data.Map.Strict as M
-- >>> let cfg = defaultConfig { configMacros = M.insert "VERSION" "42" (configMacros defaultConfig) }
-- >>> M.lookup "VERSION" (configMacros cfg)
-- Just "42"
defaultConfig :: Config
defaultConfig =
  Config
    { configInputFile = "<input>",
      configMacros =
        M.fromList
          [ ("__DATE__", "\"Jan  1 1970\""),
            ("__TIME__", "\"00:00:00\"")
          ]
    }

-- | The kind of @#include@ directive.
data IncludeKind = IncludeLocal | IncludeSystem deriving (Eq, Show, Generic, NFData)

-- | Information about a pending @#include@ that needs to be resolved.
data IncludeRequest = IncludeRequest
  { -- | The path specified in the include directive.
    includePath :: !FilePath,
    -- | Whether this is a local (@\"...\"@) or system (@\<...\>@) include.
    includeKind :: !IncludeKind,
    -- | The file that contains the @#include@ directive.
    includeFrom :: !FilePath,
    -- | The line number of the @#include@ directive.
    includeLine :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

-- | Severity level for diagnostics.
data Severity = Warning | Error deriving (Eq, Show, Generic, NFData)

-- | A diagnostic message emitted during preprocessing.
data Diagnostic = Diagnostic
  { -- | The severity of the diagnostic.
    diagSeverity :: !Severity,
    -- | The diagnostic message text.
    diagMessage :: !Text,
    -- | The file where the diagnostic occurred.
    diagFile :: !FilePath,
    -- | The line number where the diagnostic occurred.
    diagLine :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

-- | The result of preprocessing.
data Result = Result
  { -- | The preprocessed output text.
    resultOutput :: !Text,
    -- | Any diagnostics (warnings or errors) emitted during preprocessing.
    resultDiagnostics :: ![Diagnostic]
  }
  deriving (Eq, Show, Generic, NFData)

-- | A step in the preprocessing process. Either preprocessing is complete
-- ('Done') or an @#include@ directive needs to be resolved ('NeedInclude').
data Step
  = -- | Preprocessing is complete.
    Done !Result
  | -- | An @#include@ directive was encountered. The caller must provide
    -- the contents of the included file (or 'Nothing' if not found),
    -- and preprocessing will continue.
    NeedInclude !IncludeRequest !(Maybe Text -> Step)

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

data EngineState = EngineState
  { stMacros :: !(Map Text MacroDef),
    stOutputRev :: ![Text],
    stDiagnosticsRev :: ![Diagnostic],
    stSkippingDanglingElse :: !Bool,
    stHsBlockCommentDepth :: !Int,
    stCBlockCommentDepth :: !Int,
    stCurrentFile :: !FilePath,
    stCurrentLine :: !Int
  }

emptyState :: FilePath -> EngineState
emptyState filePath =
  EngineState
    { stMacros = M.empty,
      stOutputRev = [],
      stDiagnosticsRev = [],
      stSkippingDanglingElse = False,
      stHsBlockCommentDepth = 0,
      stCBlockCommentDepth = 0,
      stCurrentFile = filePath,
      stCurrentLine = 1
    }

data CondFrame = CondFrame
  { frameOuterActive :: !Bool,
    frameConditionTrue :: !Bool,
    frameInElse :: !Bool,
    frameCurrentActive :: !Bool
  }

currentActive :: [CondFrame] -> Bool
currentActive [] = True
currentActive (f : _) = frameCurrentActive f

type Continuation = EngineState -> Step

data LineContext = LineContext
  { lcFilePath :: !FilePath,
    lcLineNo :: !Int,
    lcLineSpan :: !Int,
    lcNextLineNo :: !Int,
    lcRestLines :: ![(Int, Int, Text)],
    lcStack :: ![CondFrame],
    lcContinue :: EngineState -> Step,
    lcContinueWith :: [CondFrame] -> EngineState -> Step,
    lcDone :: Continuation
  }

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

mkFrame :: Bool -> Bool -> CondFrame
mkFrame outer cond =
  CondFrame
    { frameOuterActive = outer,
      frameConditionTrue = cond,
      frameInElse = False,
      frameCurrentActive = outer && cond
    }

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

builderToText :: TB.Builder -> Text
builderToText = TL.toStrict . TB.toLazyText

trimSpacesText :: Text -> Text
trimSpacesText = T.dropWhileEnd isSpace . T.dropWhile isSpace

data LineSpan = LineSpan
  { lineSpanInBlockComment :: !Bool,
    lineSpanText :: !Text
  }

data LineScan = LineScan
  { lineScanSpans :: ![LineSpan],
    lineScanFinalHsDepth :: !Int,
    lineScanFinalCDepth :: !Int
  }

expandLineBySpan :: EngineState -> [LineSpan] -> Text
expandLineBySpan st =
  T.concat . map expandSpan
  where
    expandSpan lineChunk
      | lineSpanInBlockComment lineChunk = lineSpanText lineChunk
      | otherwise = expandMacros st (lineSpanText lineChunk)

scanLine :: Int -> Int -> Text -> LineScan
scanLine hsDepth0 cDepth0 input =
  let (spansRev, currentBuilder, hasCurrent, currentInComment, finalHsDepth, finalCDepth) =
        go hsDepth0 cDepth0 False False False [] mempty False (hsDepth0 > 0 || cDepth0 > 0) input
      spans = reverse (flushSpan spansRev currentBuilder hasCurrent currentInComment)
   in LineScan
        { lineScanSpans = spans,
          lineScanFinalHsDepth = finalHsDepth,
          lineScanFinalCDepth = finalCDepth
        }
  where
    flushSpan :: [LineSpan] -> TB.Builder -> Bool -> Bool -> [LineSpan]
    flushSpan spansRev currentBuilder hasCurrent inComment =
      if not hasCurrent
        then spansRev
        else LineSpan {lineSpanInBlockComment = inComment, lineSpanText = builderToText currentBuilder} : spansRev

    appendWithMode ::
      [LineSpan] ->
      TB.Builder ->
      Bool ->
      Bool ->
      Bool ->
      Text ->
      ([LineSpan], TB.Builder, Bool, Bool)
    appendWithMode spansRev currentBuilder hasCurrent currentInComment newInComment chunk
      | T.null chunk = (spansRev, currentBuilder, hasCurrent, currentInComment)
      | not hasCurrent = (spansRev, TB.fromText chunk, True, newInComment)
      | currentInComment == newInComment = (spansRev, currentBuilder <> TB.fromText chunk, True, currentInComment)
      | otherwise =
          let spansRev' = flushSpan spansRev currentBuilder hasCurrent currentInComment
           in (spansRev', TB.fromText chunk, True, newInComment)

    go ::
      Int ->
      Int ->
      Bool ->
      Bool ->
      Bool ->
      [LineSpan] ->
      TB.Builder ->
      Bool ->
      Bool ->
      Text ->
      ([LineSpan], TB.Builder, Bool, Bool, Int, Int)
    go hsDepth cDepth inString inChar escaped spansRev currentBuilder hasCurrent currentInComment remaining =
      case T.uncons remaining of
        Nothing -> (spansRev, currentBuilder, hasCurrent, currentInComment, hsDepth, cDepth)
        Just (c1, rest1) ->
          case T.uncons rest1 of
            Nothing ->
              let outChar = if cDepth > 0 then " " else T.singleton c1
                  inCommentNow = hsDepth > 0 || cDepth > 0
                  (spansRev', currentBuilder', hasCurrent', currentInComment') =
                    appendWithMode spansRev currentBuilder hasCurrent currentInComment inCommentNow outChar
               in (spansRev', currentBuilder', hasCurrent', currentInComment', hsDepth, cDepth)
            Just (c2, rest2) ->
              if cDepth > 0
                then
                  if c1 == '*' && c2 == '/'
                    then
                      let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                            appendWithMode spansRev currentBuilder hasCurrent currentInComment True "  "
                       in go hsDepth 0 False False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                    else
                      let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                            appendWithMode spansRev currentBuilder hasCurrent currentInComment True " "
                       in go hsDepth cDepth False False False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                else
                  if not inString && not inChar && hsDepth == 0 && c1 == '-' && c2 == '-'
                    then
                      let lineTail = T.cons c1 (T.cons c2 rest2)
                          (spansRev', currentBuilder', hasCurrent', currentInComment') =
                            appendWithMode spansRev currentBuilder hasCurrent currentInComment True lineTail
                       in (spansRev', currentBuilder', hasCurrent', currentInComment', hsDepth, cDepth)
                    else
                      if inString
                        then
                          let escaped' = not escaped && c1 == '\\'
                              inString' = escaped || c1 /= '"'
                              (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                appendWithMode spansRev currentBuilder hasCurrent currentInComment (hsDepth > 0) (T.singleton c1)
                           in go hsDepth cDepth inString' False escaped' spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                        else
                          if inChar
                            then
                              let escaped' = not escaped && c1 == '\\'
                                  inChar' = escaped || c1 /= '\''
                                  (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                    appendWithMode spansRev currentBuilder hasCurrent currentInComment (hsDepth > 0) (T.singleton c1)
                               in go hsDepth cDepth False inChar' escaped' spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                            else
                              if hsDepth == 0 && c1 == '"'
                                then
                                  let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                        appendWithMode spansRev currentBuilder hasCurrent currentInComment False (T.singleton c1)
                                   in go hsDepth cDepth True False False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                                else
                                  if hsDepth == 0 && c1 == '\''
                                    then
                                      let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                            appendWithMode spansRev currentBuilder hasCurrent currentInComment False (T.singleton c1)
                                       in go hsDepth cDepth False True False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                                    else
                                      if hsDepth > 0 && c1 == '-' && c2 == '}'
                                        then
                                          let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                appendWithMode spansRev currentBuilder hasCurrent currentInComment True "-}"
                                              hsDepth' = hsDepth - 1
                                           in go hsDepth' cDepth False False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                                        else
                                          if c1 == '{' && c2 == '-' && not ("#" `T.isPrefixOf` rest2)
                                            then
                                              let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                    appendWithMode spansRev currentBuilder hasCurrent currentInComment True "{-"
                                               in go (hsDepth + 1) cDepth False False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                                            else
                                              if hsDepth == 0 && c1 == '/' && c2 == '*'
                                                then
                                                  let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                        appendWithMode spansRev currentBuilder hasCurrent currentInComment True "  "
                                                   in go hsDepth 1 False False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                                                else
                                                  let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                        appendWithMode spansRev currentBuilder hasCurrent currentInComment (hsDepth > 0) (T.singleton c1)
                                                   in go hsDepth cDepth False False False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)

data Directive
  = DirDefineObject !Text !Text
  | DirDefineFunction !Text ![Text] !Text
  | DirUndef !Text
  | DirInclude !IncludeKind !Text
  | DirIf !Text
  | DirIfDef !Text
  | DirIfNDef !Text
  | DirElif !Text
  | DirElse
  | DirEndIf
  | DirLine !Int !(Maybe FilePath)
  | DirWarning !Text
  | DirError !Text
  | DirUnsupported !Text

parseDirective :: Text -> Maybe Directive
parseDirective raw =
  let trimmed = T.stripStart raw
   in if "#" `T.isPrefixOf` trimmed
        then
          let body = T.stripStart (T.drop 1 trimmed)
           in case T.uncons body of
                Just (c, _) | isLetter c || isDigit c -> parseDirectiveBody body
                _ -> Nothing
        else Nothing

parseDirectiveBody :: Text -> Maybe Directive
parseDirectiveBody body =
  let (name, rest0) = T.span isIdentChar body
      rest = T.stripStart rest0
   in if T.null name
        then case T.uncons body of
          Just (c, _) | isDigit c -> parseLineDirective body
          _ -> Nothing
        else case name of
          "define" -> parseDefine rest
          "undef" -> DirUndef <$> parseIdentifier rest
          "include" -> parseInclude rest
          "if" -> Just (DirIf rest)
          "ifdef" -> DirIfDef <$> parseIdentifier rest
          "ifndef" -> DirIfNDef <$> parseIdentifier rest
          "isndef" -> Just (DirUnsupported "isndef")
          "elif" -> Just (DirElif rest)
          "elseif" -> Just (DirElif rest)
          "else" -> Just DirElse
          "endif" -> Just DirEndIf
          "line" -> parseLineDirective rest
          "warning" -> Just (DirWarning rest)
          "error" -> Just (DirError rest)
          _ -> Just (DirUnsupported name)

parseLineDirective :: Text -> Maybe Directive
parseLineDirective body =
  case TR.decimal body of
    Left _ -> Nothing
    Right (lineNumber, rest0) ->
      let rest = T.stripStart rest0
       in case parseQuotedText rest of
            Nothing -> Just (DirLine lineNumber Nothing)
            Just path -> Just (DirLine lineNumber (Just (T.unpack path)))

parseDefine :: Text -> Maybe Directive
parseDefine rest = do
  let (name, rest0) = T.span isIdentChar rest
  if T.null name
    then Nothing
    else case T.uncons rest0 of
      Just ('(', afterOpen) ->
        let (params, restAfterParams) = parseDefineParams afterOpen
         in case params of
              Nothing -> Just (DirUnsupported "define-function-macro")
              Just names -> Just (DirDefineFunction name names (T.stripStart restAfterParams))
      _ -> Just (DirDefineObject name (T.stripStart rest0))

parseDefineParams :: Text -> (Maybe [Text], Text)
parseDefineParams input =
  let (inside, suffix) = T.breakOn ")" input
   in if T.null suffix
        then (Nothing, "")
        else
          let rawParams = T.splitOn "," inside
              params = map (T.takeWhile isIdentChar . T.strip) rawParams
           in if T.null (T.strip inside)
                then (Just [], T.drop 1 suffix)
                else
                  if any T.null params
                    then (Nothing, T.drop 1 suffix)
                    else (Just params, T.drop 1 suffix)

parseIdentifier :: Text -> Maybe Text
parseIdentifier txt =
  let ident = T.takeWhile isIdentChar (T.stripStart txt)
   in if T.null ident then Nothing else Just ident

parseInclude :: Text -> Maybe Directive
parseInclude txt =
  case T.uncons (T.stripStart txt) of
    Just ('"', rest) ->
      let (path, suffix) = T.breakOn "\"" rest
       in if T.null suffix then Nothing else Just (DirInclude IncludeLocal path)
    Just ('<', rest) ->
      let (path, suffix) = T.breakOn ">" rest
       in if T.null suffix then Nothing else Just (DirInclude IncludeSystem path)
    _ -> Nothing

parseQuotedText :: Text -> Maybe Text
parseQuotedText txt = do
  ('"', rest) <- T.uncons txt
  let (path, suffix) = T.breakOn "\"" rest
  if T.null suffix then Nothing else Just path

expandMacros :: EngineState -> Text -> Text
expandMacros st = applyDepth (32 :: Int)
  where
    applyDepth 0 t = t
    applyDepth n t =
      let next = expandOnce st t
       in if next == t then t else applyDepth (n - 1) next

expandOnce :: EngineState -> Text -> Text
expandOnce st = go False False False
  where
    macros = stMacros st
    go :: Bool -> Bool -> Bool -> Text -> Text
    go _ _ _ txt
      | T.null txt = ""
    go inString inChar escaped txt =
      case T.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | inString ->
              let escaped' = c == '\\' && not escaped
                  inString' = not (c == '"' && not escaped)
               in T.cons c (go inString' False escaped' rest)
          | inChar ->
              let escaped' = c == '\\' && not escaped
                  inChar' = not (c == '\'' && not escaped)
               in T.cons c (go False inChar' escaped' rest)
          | c == '"' ->
              T.cons c (go True False False rest)
          | c == '\'' ->
              T.cons c (go False True False rest)
          | isIdentStart c ->
              expandIdentifier txt
          | otherwise ->
              T.cons c (go False False False rest)

    expandIdentifier :: Text -> Text
    expandIdentifier input =
      let (ident, rest) = T.span isIdentChar input
       in case ident of
            "__LINE__" -> T.pack (show (stCurrentLine st)) <> go False False False rest
            "__FILE__" -> T.pack (show (stCurrentFile st)) <> go False False False rest
            _ ->
              case M.lookup ident macros of
                Just (ObjectMacro replacement) ->
                  replacement <> go False False False rest
                Just (FunctionMacro params body) ->
                  case parseCallArgs rest of
                    Nothing -> ident <> go False False False rest
                    Just (args, restAfter)
                      | length args == length params ->
                          let body' = substituteParams (M.fromList (zip params args)) body
                           in body' <> go False False False restAfter
                      | otherwise -> ident <> go False False False rest
                Nothing -> ident <> go False False False rest

    parseCallArgs :: Text -> Maybe ([Text], Text)
    parseCallArgs input = do
      ('(', rest) <- T.uncons input
      parseArgs 0 [] mempty rest

    parseArgs :: Int -> [Text] -> TB.Builder -> Text -> Maybe ([Text], Text)
    parseArgs depth argsRev current remaining =
      case T.uncons remaining of
        Nothing -> Nothing
        Just (ch, rest)
          | ch == '(' ->
              parseArgs (depth + 1) argsRev (current <> TB.singleton ch) rest
          | ch == ')' && depth > 0 ->
              parseArgs (depth - 1) argsRev (current <> TB.singleton ch) rest
          | ch == ')' && depth == 0 ->
              let arg = trimSpacesText (builderToText current)
                  argsRev' =
                    if T.null arg && null argsRev
                      then argsRev
                      else arg : argsRev
               in Just (reverse argsRev', rest)
          | ch == ',' && depth == 0 ->
              let arg = trimSpacesText (builderToText current)
               in parseArgs depth (arg : argsRev) mempty rest
          | otherwise ->
              parseArgs depth argsRev (current <> TB.singleton ch) rest

substituteParams :: Map Text Text -> Text -> Text
substituteParams subs = go False False False
  where
    go :: Bool -> Bool -> Bool -> Text -> Text
    go _ _ _ txt
      | T.null txt = ""
    go inDouble inSingle escaped txt =
      case T.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | inDouble ->
              T.cons c $
                case c of
                  '\\' ->
                    if escaped
                      then go True inSingle False rest
                      else go True inSingle True rest
                  '"' ->
                    if escaped
                      then go True inSingle False rest
                      else go False inSingle False rest
                  _ -> go True inSingle False rest
          | inSingle ->
              T.cons c $
                case c of
                  '\\' ->
                    if escaped
                      then go inDouble True False rest
                      else go inDouble True True rest
                  '\'' ->
                    if escaped
                      then go inDouble True False rest
                      else go inDouble False False rest
                  _ -> go inDouble True False rest
          | c == '"' ->
              T.cons c (go True False False rest)
          | c == '\'' ->
              T.cons c (go False True False rest)
          | isIdentStart c ->
              let (ident, rest') = T.span isIdentChar txt
               in M.findWithDefault ident ident subs <> go False False False rest'
          | otherwise ->
              T.cons c (go False False False rest)

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isLetter c

isIdentChar :: Char -> Bool
isIdentChar c = c == '_' || isAlphaNum c

--------------------------------------------------------------------------------
-- Expression Evaluation
--------------------------------------------------------------------------------

evalCondition :: EngineState -> Text -> Bool
evalCondition st expr = eval expr /= 0
  where
    macros = stMacros st
    eval = evalNumeric . replaceRemainingWithZero . expandMacros st . replaceDefined macros

evalNumeric :: Text -> Integer
evalNumeric input =
  let tokens = tokenize input
   in case parseExpr tokens of
        (val, _) -> val

data Token = TOp Text | TNum Integer | TIdent Text | TOpenParen | TCloseParen deriving (Show)

tokenize :: Text -> [Token]
tokenize input =
  case T.uncons input of
    Nothing -> []
    Just (c, rest)
      | isSpace c ->
          tokenize (T.dropWhile isSpace rest)
      | isDigit c ->
          let (num, remaining) = T.span isDigit input
           in case TR.decimal num of
                Right (value, _) -> TNum value : tokenize remaining
                Left _ -> tokenize remaining
      | isIdentStart c ->
          let (ident, remaining) = T.span isIdentChar input
           in TIdent ident : tokenize remaining
      | c == '(' ->
          TOpenParen : tokenize rest
      | c == ')' ->
          TCloseParen : tokenize rest
      | otherwise ->
          let (op, remaining) = T.span isOpChar input
           in if T.null op
                then tokenize rest
                else TOp op : tokenize remaining

isOpChar :: Char -> Bool
isOpChar c =
  c == '+'
    || c == '-'
    || c == '*'
    || c == '/'
    || c == '%'
    || c == '&'
    || c == '|'
    || c == '!'
    || c == '='
    || c == '<'
    || c == '>'

parseExpr :: [Token] -> (Integer, [Token])
parseExpr = parseOr

binary :: ([Token] -> (Integer, [Token])) -> [Text] -> [Token] -> (Integer, [Token])
binary next ops ts =
  let (v1, ts1) = next ts
   in go v1 ts1
  where
    go v1 (TOp op : ts2)
      | op `elem` ops =
          let (v2, ts3) = next ts2
           in go (apply op v1 v2) ts3
    go v1 ts2 = (v1, ts2)

    apply "||" a b = if a /= 0 || b /= 0 then 1 else 0
    apply "&&" a b = if a /= 0 && b /= 0 then 1 else 0
    apply "==" a b = if a == b then 1 else 0
    apply "!=" a b = if a /= b then 1 else 0
    apply "<" a b = if a < b then 1 else 0
    apply ">" a b = if a > b then 1 else 0
    apply "<=" a b = if a <= b then 1 else 0
    apply ">=" a b = if a >= b then 1 else 0
    apply "+" a b = a + b
    apply "-" a b = a - b
    apply "*" a b = a * b
    apply "/" a b = if b == 0 then 0 else a `div` b
    apply "%" a b = if b == 0 then 0 else a `mod` b
    apply _ a _ = a

parseOr, parseAnd, parseEq, parseRel, parseAdd, parseMul :: [Token] -> (Integer, [Token])
parseOr = binary parseAnd ["||"]
parseAnd = binary parseEq ["&&"]
parseEq = binary parseRel ["==", "!="]
parseRel = binary parseAdd ["<", ">", "<=", ">="]
parseAdd = binary parseMul ["+", "-"]
parseMul = binary parseUnary ["*", "/", "%"]

parseUnary :: [Token] -> (Integer, [Token])
parseUnary (TOp "!" : ts) = let (v, ts') = parseUnary ts in (if v == 0 then 1 else 0, ts')
parseUnary (TOp "-" : ts) = let (v, ts') = parseUnary ts in (-v, ts')
parseUnary ts = parseAtom ts

parseAtom :: [Token] -> (Integer, [Token])
parseAtom (TNum n : ts) = (n, ts)
parseAtom (TIdent _ : ts) = (0, ts)
parseAtom (TOpenParen : ts) =
  let (v, ts1) = parseExpr ts
   in case ts1 of
        TCloseParen : ts2 -> (v, ts2)
        _ -> (v, ts1)
parseAtom ts = (0, ts)

replaceDefined :: Map Text MacroDef -> Text -> Text
replaceDefined macros = go
  where
    go txt =
      case T.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | "defined" `T.isPrefixOf` txt && not (nextCharIsIdent (T.drop 7 txt)) ->
              expandDefined (T.dropWhile isSpace (T.drop 7 txt))
          | otherwise ->
              T.cons c (go rest)

    expandDefined rest =
      case T.uncons rest of
        Just ('(', restAfterOpen) ->
          let rest' = T.dropWhile isSpace restAfterOpen
              (name, restAfterName0) = T.span isIdentChar rest'
              restAfterName = T.dropWhile isSpace restAfterName0
           in case T.uncons restAfterName of
                Just (')', restAfterClose) ->
                  boolLiteral (M.member name macros) <> go restAfterClose
                _ ->
                  boolLiteral False <> go restAfterName
        _ ->
          let (name, restAfterName) = T.span isIdentChar rest
           in if T.null name
                then boolLiteral False <> go rest
                else boolLiteral (M.member name macros) <> go restAfterName

    boolLiteral True = " 1 "
    boolLiteral False = " 0 "

    nextCharIsIdent remaining =
      case T.uncons remaining of
        Just (c, _) -> isIdentChar c
        Nothing -> False

replaceRemainingWithZero :: Text -> Text
replaceRemainingWithZero = go
  where
    go txt =
      case T.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | isIdentStart c ->
              let (_, remaining) = T.span isIdentChar txt
               in " 0 " <> go remaining
          | otherwise ->
              T.cons c (go rest)
