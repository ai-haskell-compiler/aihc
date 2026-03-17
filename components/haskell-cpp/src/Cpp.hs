{-# LANGUAGE OverloadedStrings #-}

module Cpp
  ( Config (..),
    defaultConfig,
    Step (..),
    Result (..),
    IncludeRequest (..),
    IncludeKind (..),
    Diagnostic (..),
    Severity (..),
    preprocess,
  )
where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import System.FilePath (takeDirectory, (</>))

data Config = Config
  { configInputFile :: FilePath,
    configDateTime :: !(Text, Text),
    configMacros :: !(Map Text Text)
  }

data MacroDef
  = ObjectMacro !Text
  | FunctionMacro ![Text] !Text
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {configInputFile = "<input>", configDateTime = ("Jan  1 1970", "00:00:00"), configMacros = M.empty}

data IncludeKind = IncludeLocal | IncludeSystem deriving (Eq, Show)

data IncludeRequest = IncludeRequest
  { includePath :: !FilePath,
    includeKind :: !IncludeKind,
    includeFrom :: !FilePath,
    includeLine :: !Int
  }
  deriving (Eq, Show)

data Severity = Warning | Error deriving (Eq, Show)

data Diagnostic = Diagnostic
  { diagSeverity :: !Severity,
    diagMessage :: !Text,
    diagFile :: !FilePath,
    diagLine :: !Int
  }
  deriving (Eq, Show)

data Result = Result
  { resultOutput :: !Text,
    resultDiagnostics :: ![Diagnostic]
  }
  deriving (Eq, Show)

data Step
  = Done !Result
  | NeedInclude !IncludeRequest !(Maybe Text -> Step)

preprocess :: Config -> Text -> Step
preprocess cfg input =
  processFile (configInputFile cfg) (joinMultiline 1 (T.lines input)) [] initialState finish
  where
    initialState =
      let st0 = emitLine (linePragma 1 (configInputFile cfg)) (emptyState (configInputFile cfg))
          (date, time) = configDateTime cfg
       in st0
            { stMacros =
                M.insert "__DATE__" (ObjectMacro ("\"" <> date <> "\"")) $
                  M.insert "__TIME__" (ObjectMacro ("\"" <> time <> "\"")) $
                    M.map ObjectMacro (configMacros cfg)
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

processFile :: FilePath -> [(Int, Int, Text)] -> [CondFrame] -> EngineState -> Continuation -> Step
processFile _ [] _ st k = k st
processFile filePath ((lineNo, lineSpan, line) : restLines) stack st k =
  let active = currentActive stack
      lineScan = scanLine (stHsBlockCommentDepth st) (stCBlockCommentDepth st) line
      startsInBlockComment = stHsBlockCommentDepth st > 0 || stCBlockCommentDepth st > 0
      nextHsBlockCommentDepth = lineScanFinalHsDepth lineScan
      nextCBlockCommentDepth = lineScanFinalCDepth lineScan
      parsedDirective =
        if startsInBlockComment
          then Nothing
          else parseDirective line
      nextLineNo = case restLines of
        (n, _, _) : _ -> n
        [] -> lineNo + lineSpan
      emitDirectiveBlank = emitBlankLines lineSpan
      emitExpandedLine st' = emitLine (expandLineBySpan st' (lineScanSpans lineScan)) st'
      continue st' =
        processFile
          filePath
          restLines
          stack
          ( st'
              { stCurrentLine = nextLineNo,
                stHsBlockCommentDepth = nextHsBlockCommentDepth,
                stCBlockCommentDepth = nextCBlockCommentDepth
              }
          )
          k
      continueWith stack' st' =
        processFile
          filePath
          restLines
          stack'
          ( st'
              { stCurrentLine = nextLineNo,
                stHsBlockCommentDepth = nextHsBlockCommentDepth,
                stCBlockCommentDepth = nextCBlockCommentDepth
              }
          )
          k
      recoverDanglingElse =
        case parsedDirective of
          Just DirEndIf ->
            continue
              ( addDiag
                  Warning
                  "unmatched #endif"
                  filePath
                  lineNo
                  (st {stSkippingDanglingElse = False})
              )
          Just _ ->
            continue st
          Nothing ->
            continue (emitBlankLines lineSpan st)
      handleDirective directive =
        case directive of
          DirDefineObject name value ->
            if currentActive stack
              then continue (emitDirectiveBlank (st {stMacros = M.insert name (ObjectMacro value) (stMacros st)}))
              else continue (emitDirectiveBlank st)
          DirDefineFunction name params body ->
            if currentActive stack
              then continue (emitDirectiveBlank (st {stMacros = M.insert name (FunctionMacro params body) (stMacros st)}))
              else continue (emitDirectiveBlank st)
          DirUndef name ->
            if currentActive stack
              then continue (emitDirectiveBlank (st {stMacros = M.delete name (stMacros st)}))
              else continue (emitDirectiveBlank st)
          DirInclude kind includeTarget ->
            if currentActive stack
              then
                let req =
                      IncludeRequest
                        { includePath = includeTarget,
                          includeKind = kind,
                          includeFrom = filePath,
                          includeLine = lineNo
                        }
                    nextStep mContent =
                      case mContent of
                        Nothing ->
                          continue
                            (addDiag Error ("missing include: " <> T.pack includeTarget) filePath lineNo st)
                        Just includeText ->
                          let includeFilePath =
                                case kind of
                                  IncludeLocal -> takeDirectory filePath </> includeTarget
                                  IncludeSystem -> includeTarget
                              stWithIncludePragma = emitLine (linePragma 1 includeFilePath) (st {stCurrentFile = includeFilePath, stCurrentLine = 1})
                              resumeParent stAfterInclude =
                                processFile
                                  filePath
                                  restLines
                                  stack
                                  (emitLine (linePragma nextLineNo filePath) (stAfterInclude {stCurrentFile = filePath, stCurrentLine = nextLineNo}))
                                  k
                           in processFile includeFilePath (joinMultiline 1 (splitLines includeText)) [] stWithIncludePragma resumeParent
                 in NeedInclude req nextStep
              else continue (emitDirectiveBlank st)
          DirIf expr ->
            let outer = currentActive stack
                cond = evalCondition st expr
                frame = mkFrame outer cond
             in continueWith (frame : stack) (emitDirectiveBlank st)
          DirIfDef name ->
            let outer = currentActive stack
                cond = M.member name (stMacros st)
                frame = mkFrame outer cond
             in continueWith (frame : stack) (emitDirectiveBlank st)
          DirIfNDef name ->
            let outer = currentActive stack
                cond = not (M.member name (stMacros st))
                frame = mkFrame outer cond
             in continueWith (frame : stack) (emitDirectiveBlank st)
          DirElif expr ->
            case stack of
              [] ->
                continue
                  (emitDirectiveBlank (addDiag Error "#elif without matching #if" filePath lineNo st))
              f : rest ->
                if frameInElse f
                  then
                    continue
                      (emitDirectiveBlank (addDiag Error "#elif after #else" filePath lineNo st))
                  else
                    let anyTaken = frameConditionTrue f
                        newCond = not anyTaken && evalCondition st expr
                        f' =
                          f
                            { frameConditionTrue = anyTaken || newCond,
                              frameCurrentActive = frameOuterActive f && newCond
                            }
                     in continueWith (f' : rest) (emitDirectiveBlank st)
          DirElse ->
            case stack of
              [] ->
                continue (st {stSkippingDanglingElse = True})
              f : rest ->
                if frameInElse f
                  then
                    continue
                      (emitDirectiveBlank (addDiag Error "duplicate #else in conditional block" filePath lineNo st))
                  else
                    let newCurrent = frameOuterActive f && not (frameConditionTrue f)
                        f' =
                          f
                            { frameInElse = True,
                              frameCurrentActive = newCurrent
                            }
                     in continueWith (f' : rest) (emitDirectiveBlank st)
          DirEndIf ->
            case stack of
              [] ->
                continue
                  (addDiag Warning "unmatched #endif" filePath lineNo st)
              _ : rest -> continueWith rest (emitDirectiveBlank st)
          DirWarning msg ->
            if currentActive stack
              then continue (emitDirectiveBlank (addDiag Warning msg filePath lineNo st))
              else continue (emitDirectiveBlank st)
          DirError msg ->
            if currentActive stack
              then k (emitDirectiveBlank (addDiag Error msg filePath lineNo st))
              else continue (emitDirectiveBlank st)
          DirLine n mPath ->
            if currentActive stack
              then
                let st'' = case mPath of
                      Just p -> st {stCurrentFile = p}
                      Nothing -> st
                    st''' = emitLine (linePragma n (stCurrentFile st'')) (st'' {stCurrentLine = n})
                 in processFile filePath restLines stack (st''' {stCurrentLine = n}) k
              else continue (emitDirectiveBlank st)
          DirUnsupported name ->
            if currentActive stack
              then
                continue
                  (emitDirectiveBlank (addDiag Warning ("unsupported directive: " <> name) filePath lineNo st))
              else continue (emitDirectiveBlank st)
   in if stSkippingDanglingElse st
        then recoverDanglingElse
        else case parsedDirective of
          Nothing ->
            if active
              then continue (emitExpandedLine st)
              else continue (emitBlankLines lineSpan st)
          Just directive ->
            handleDirective directive

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
        go hsDepth0 cDepth0 False False [] mempty False (hsDepth0 > 0 || cDepth0 > 0) input
      spans = reverse (flushSpan spansRev currentBuilder hasCurrent currentInComment)
   in LineScan
        { lineScanSpans = spans,
          lineScanFinalHsDepth = finalHsDepth,
          lineScanFinalCDepth = finalCDepth
        }
  where
    builderToText :: TB.Builder -> Text
    builderToText = TL.toStrict . TB.toLazyText

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
      [LineSpan] ->
      TB.Builder ->
      Bool ->
      Bool ->
      Text ->
      ([LineSpan], TB.Builder, Bool, Bool, Int, Int)
    go hsDepth cDepth inString escaped spansRev currentBuilder hasCurrent currentInComment remaining =
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
                       in go hsDepth 0 False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                    else
                      let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                            appendWithMode spansRev currentBuilder hasCurrent currentInComment True " "
                       in go hsDepth cDepth False False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                else
                  if not inString && hsDepth == 0 && c1 == '-' && c2 == '-'
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
                           in go hsDepth cDepth inString' escaped' spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                        else
                          if hsDepth == 0 && c1 == '"'
                            then
                              let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                    appendWithMode spansRev currentBuilder hasCurrent currentInComment False (T.singleton c1)
                               in go hsDepth cDepth True False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                            else
                              if hsDepth > 0 && c1 == '-' && c2 == '}'
                                then
                                  let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                        appendWithMode spansRev currentBuilder hasCurrent currentInComment True "-}"
                                      hsDepth' = hsDepth - 1
                                   in go hsDepth' cDepth False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                                else
                                  if c1 == '{' && c2 == '-' && not ("#" `T.isPrefixOf` rest2)
                                    then
                                      let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                            appendWithMode spansRev currentBuilder hasCurrent currentInComment True "{-"
                                       in go (hsDepth + 1) cDepth False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                                    else
                                      if hsDepth == 0 && c1 == '/' && c2 == '*'
                                        then
                                          let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                appendWithMode spansRev currentBuilder hasCurrent currentInComment True "  "
                                           in go hsDepth 1 False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                                        else
                                          let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                appendWithMode spansRev currentBuilder hasCurrent currentInComment (hsDepth > 0) (T.singleton c1)
                                           in go hsDepth cDepth False False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)

data Directive
  = DirDefineObject !Text !Text
  | DirDefineFunction !Text ![Text] !Text
  | DirUndef !Text
  | DirInclude !IncludeKind !FilePath
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
  let (nStr, rest0) = T.span isDigit body
      rest = T.stripStart rest0
   in if T.null nStr
        then Nothing
        else
          let n = read (T.unpack nStr)
           in case T.uncons rest of
                Just ('"', rest1) ->
                  let (path, suffix) = T.breakOn "\"" rest1
                   in if T.null suffix
                        then Just (DirLine n Nothing)
                        else Just (DirLine n (Just (T.unpack path)))
                _ -> Just (DirLine n Nothing)

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
       in if T.null suffix then Nothing else Just (DirInclude IncludeLocal (T.unpack path))
    Just ('<', rest) ->
      let (path, suffix) = T.breakOn ">" rest
       in if T.null suffix then Nothing else Just (DirInclude IncludeSystem (T.unpack path))
    _ -> Nothing

expandMacros :: EngineState -> Text -> Text
expandMacros st = applyDepth (32 :: Int)
  where
    applyDepth 0 t = t
    applyDepth n t =
      let next = expandOnce st t
       in if next == t then t else applyDepth (n - 1) next

expandOnce :: EngineState -> Text -> Text
expandOnce st input = T.pack (go False False (T.unpack input))
  where
    macros = stMacros st
    go _ _ [] = []
    go True escaped (c : cs) =
      c
        : case c of
          '\\' ->
            if escaped
              then go True False cs
              else go True True cs
          '"' ->
            if escaped
              then go True False cs
              else go False False cs
          _ -> go True False cs
    go False _ (c : cs)
      | c == '"' = c : go True False cs
      | isIdentStart c =
          let (ident, rest) = span isIdentChar (c : cs)
              identTxt = T.pack ident
           in case identTxt of
                "__LINE__" -> show (stCurrentLine st) ++ go False False rest
                "__FILE__" -> show (stCurrentFile st) ++ go False False rest
                _ ->
                  case M.lookup identTxt macros of
                    Just (ObjectMacro repl) -> T.unpack repl ++ go False False rest
                    Just (FunctionMacro params body) ->
                      case parseCallArgs rest of
                        Nothing -> ident ++ go False False rest
                        Just (args, restAfter) ->
                          if length args == length params
                            then
                              let body' =
                                    substituteParams
                                      (M.fromList (zip params (map T.pack args)))
                                      body
                               in T.unpack body' ++ go False False restAfter
                            else ident ++ go False False rest
                    Nothing -> ident ++ go False False rest
      | otherwise = c : go False False cs

    parseCallArgs :: String -> Maybe ([String], String)
    parseCallArgs ('(' : xs) = parseArgs 0 [] [] xs
    parseCallArgs _ = Nothing

    parseArgs :: Int -> [String] -> String -> String -> Maybe ([String], String)
    parseArgs _ acc current [] =
      let current' = trimSpaces (reverse current)
       in if null current' && null acc
            then Just ([], [])
            else Nothing
    parseArgs depth acc current (ch : rest)
      | ch == '(' = parseArgs (depth + 1) acc (ch : current) rest
      | ch == ')' && depth > 0 = parseArgs (depth - 1) acc (ch : current) rest
      | ch == ')' && depth == 0 =
          let arg = reverse current
              arg' = trimSpaces arg
              args = if null arg' && null acc then acc else acc ++ [arg']
           in Just (args, rest)
      | ch == ',' && depth == 0 =
          let arg = reverse current
              arg' = trimSpaces arg
           in parseArgs depth (acc ++ [arg']) [] rest
      | otherwise = parseArgs depth acc (ch : current) rest

    trimSpaces :: String -> String
    trimSpaces = dropWhileEnd isSpace . dropWhile isSpace

substituteParams :: Map Text Text -> Text -> Text
substituteParams subs = T.pack . go False False False . T.unpack
  where
    go _ _ _ [] = []
    go True inSingle escaped (c : cs) =
      c
        : case c of
          '\\' ->
            if escaped
              then go True inSingle False cs
              else go True inSingle True cs
          '"' ->
            if escaped
              then go True inSingle False cs
              else go False inSingle False cs
          _ -> go True inSingle False cs
    go inDouble True escaped (c : cs) =
      c
        : case c of
          '\\' ->
            if escaped
              then go inDouble True False cs
              else go inDouble True True cs
          '\'' ->
            if escaped
              then go inDouble True False cs
              else go inDouble False False cs
          _ -> go inDouble True False cs
    go False False _ (c : cs)
      | c == '"' = c : go True False False cs
      | c == '\'' = c : go False True False cs
      | isIdentStart c =
          let (ident, rest) = span isIdentChar (c : cs)
              identTxt = T.pack ident
           in T.unpack (M.findWithDefault identTxt identTxt subs) ++ go False False False rest
      | otherwise = c : go False False False cs

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
  let tokens = tokenize (T.unpack input)
   in case parseExpr tokens of
        (val, _) -> val

data Token = TOp Text | TNum Integer | TIdent Text | TOpenParen | TCloseParen deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isDigit c =
      let (num, rest) = span isDigit (c : cs)
       in TNum (read num) : tokenize rest
  | isIdentStart c =
      let (ident, rest) = span isIdentChar (c : cs)
       in TIdent (T.pack ident) : tokenize rest
  | c == '(' = TOpenParen : tokenize cs
  | c == ')' = TCloseParen : tokenize cs
  | otherwise =
      let (op, rest) = span isOpChar (c : cs)
       in if null op then tokenize (drop 1 cs) else TOp (T.pack op) : tokenize rest

isOpChar :: Char -> Bool
isOpChar c = c `elem` ("+-*/%&|!=<>!" :: String)

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
replaceDefined macros = T.pack . go . T.unpack
  where
    go [] = []
    go cs@(c : rest_cs)
      | "defined" `isPrefixOf` cs && not (isIdentChar (safeHead (drop 7 cs))) =
          let rest = dropWhile isSpace (drop 7 cs)
           in case rest of
                '(' : rest1 ->
                  let name = takeWhile isIdentChar (dropWhile isSpace rest1)
                      rest2 = dropWhile isSpace (drop (length name) (dropWhile isSpace rest1))
                   in case rest2 of
                        ')' : rest3 -> (if M.member (T.pack name) macros then " 1 " else " 0 ") ++ go rest3
                        _ -> " 0 " ++ go rest2
                _ ->
                  let name = takeWhile isIdentChar rest
                   in if null name
                        then " 0 " ++ go rest
                        else (if M.member (T.pack name) macros then " 1 " else " 0 ") ++ go (drop (length name) rest)
      | otherwise = c : go rest_cs
    safeHead [] = ' '
    safeHead (x : _) = x

replaceRemainingWithZero :: Text -> Text
replaceRemainingWithZero = T.pack . go . T.unpack
  where
    go [] = []
    go (c : cs)
      | isIdentStart c =
          let (_, rest) = span isIdentChar (c : cs)
           in " 0 " ++ go rest
      | otherwise = c : go cs
