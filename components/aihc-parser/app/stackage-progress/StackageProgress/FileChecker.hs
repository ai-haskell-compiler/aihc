{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | File-level validation checks for Haskell source files.
module StackageProgress.FileChecker
  ( -- * File checking
    FileResult (..),
    PackageFileSummary (..),
    checkFile,
    emptyFileSummary,
    checkAndAccumulateFile,
    foldFilesForPackage,
    firstFailureMessage,

    -- * Check predicates
    needsFullPackageScan,
    needsParsedModule,
    shouldStopAfterFailure,
  )
where

import Aihc.Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import Aihc.Parser (ParseResult (..))
import qualified Aihc.Parser
import Aihc.Parser.Ast
import AstStripping (stripExpr)
import CppSupport (preprocessForParserIfEnabled)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GhcOracle
import HackageSupport
  ( FileInfo (..),
    diagToText,
    prefixCppErrors,
    readTextFileLenient,
    resolveIncludeBestEffort,
  )
import HseExtensions (fromExtensionNames)
import qualified Language.Haskell.Exts as HSE
import ParserValidation (ValidationError (..), ValidationErrorKind (..), validateParserDetailedWithExtensionNames)
import StackageProgress.CLI (Check (..))
import StackageProgress.Summary (forceString)

-- | Result of checking a single file.
data FileResult = FileResult
  { fileOursOk :: Bool,
    fileHseOk :: Bool,
    fileGhcOk :: Bool,
    fileError :: Maybe String,
    fileGhcError :: Maybe String
  }

-- | Accumulated summary for a package's files.
data PackageFileSummary = PackageFileSummary
  { packageFileOursOk :: !Bool,
    packageFileHseOk :: !Bool,
    packageFileGhcOk :: !Bool,
    packageFileFirstFailure :: Maybe String,
    packageFileGhcError :: Maybe String
  }

-- | Empty file summary (all checks pass).
emptyFileSummary :: PackageFileSummary
emptyFileSummary =
  PackageFileSummary
    { packageFileOursOk = True,
      packageFileHseOk = True,
      packageFileGhcOk = True,
      packageFileFirstFailure = Nothing,
      packageFileGhcError = Nothing
    }

-- | Check a file and accumulate results.
checkAndAccumulateFile :: [Check] -> FilePath -> PackageFileSummary -> FileInfo -> IO PackageFileSummary
checkAndAccumulateFile checks packageRoot summary info = do
  result <- checkFile checks packageRoot info
  let !oursOk = packageFileOursOk summary && fileOursOk result
      !hseOk = packageFileHseOk summary && fileHseOk result
      !ghcOk = packageFileGhcOk summary && fileGhcOk result
      firstFailure =
        case packageFileFirstFailure summary of
          Just err -> Just err
          Nothing ->
            case fileError result of
              Just err -> Just (forceString err)
              Nothing ->
                if fileOursOk result
                  then Nothing
                  else Just "ours failed"
      ghcError =
        case packageFileGhcError summary of
          Just err -> Just err
          Nothing -> fmap forceString (fileGhcError result)
  pure
    PackageFileSummary
      { packageFileOursOk = oursOk,
        packageFileHseOk = hseOk,
        packageFileGhcOk = ghcOk,
        packageFileFirstFailure = firstFailure,
        packageFileGhcError = ghcError
      }

-- | Get the first failure message from a summary.
firstFailureMessage :: PackageFileSummary -> String
firstFailureMessage summary =
  fromMaybe "unknown failure" (packageFileFirstFailure summary)

-- | Fold over files, checking each and accumulating results.
foldFilesForPackage :: [Check] -> FilePath -> PackageFileSummary -> [FileInfo] -> IO PackageFileSummary
foldFilesForPackage _ _ summary [] = pure summary
foldFilesForPackage checks packageRoot summary (info : rest)
  | shouldStopAfterFailure checks summary = pure summary
  | otherwise = do
      summary' <- checkAndAccumulateFile checks packageRoot summary info
      foldFilesForPackage checks packageRoot summary' rest

-- | Whether to stop checking after a failure.
shouldStopAfterFailure :: [Check] -> PackageFileSummary -> Bool
shouldStopAfterFailure checks summary =
  not (packageFileOursOk summary) && not (needsFullPackageScan checks)

-- | Whether we need to check all files in the package.
needsFullPackageScan :: [Check] -> Bool
needsFullPackageScan checks =
  CheckHse `elem` checks || CheckGhc `elem` checks

-- | Check a single file.
checkFile :: [Check] -> FilePath -> FileInfo -> IO FileResult
checkFile checks packageRoot info = do
  let file = fileInfoPath info
      parserExts = GhcOracle.extensionNamesToParserExtensions (fileInfoExtensions info)
      parserConfig = Aihc.Parser.defaultConfig {Aihc.Parser.parserExtensions = parserExts}
  source <- readTextFileLenient file
  preprocessed <- preprocessForParserIfEnabled (fileInfoExtensions info) (fileInfoCppOptions info) file (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      cppErrors = [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error]
      cppErrorMsg =
        if null cppErrors
          then Nothing
          else Just (T.intercalate "\n" cppErrors)
      oursResult = Aihc.Parser.parseModule parserConfig source'

  oursStatus <- case oursResult of
    ParseErr err ->
      if CheckParse `elem` checks || needsParsedModule checks
        then pure (Left (T.unpack (prefixCppErrors cppErrorMsg ("parse failed in " <> T.pack file <> ":\n" <> T.pack (Aihc.Parser.errorBundlePretty err)))))
        else pure (Right ())
    ParseOk parsed -> do
      roundtripRes <-
        if CheckRoundtripGhc `elem` checks
          then pure (checkRoundtrip (fileInfoExtensions info) (fileInfoLanguage info) file cppErrorMsg source')
          else pure (Right ())
      case roundtripRes of
        Left err -> pure (Left err)
        Right () ->
          if CheckSourceSpan `elem` checks
            then pure (checkSourceSpans file source' parsed)
            else pure (Right ())

  hseOk <-
    if CheckHse `elem` checks
      then pure $ checkHse (fileInfoExtensions info) (fileInfoLanguage info) source'
      else pure True

  ghcOkResult <-
    if CheckGhc `elem` checks
      then pure $ GhcOracle.oracleDetailedParsesModuleWithNamesAt file (fileInfoExtensions info) (fileInfoLanguage info) source'
      else pure (Right ())
  let ghcOk = case ghcOkResult of Right () -> True; Left _ -> False
      ghcErrMsg = case ghcOkResult of Left err -> Just (T.unpack err); Right () -> Nothing

  pure
    FileResult
      { fileOursOk = case oursStatus of Right () -> True; Left _ -> False,
        fileHseOk = hseOk,
        fileGhcOk = ghcOk,
        fileError = case oursStatus of Left err -> Just err; Right () -> Nothing,
        fileGhcError = ghcErrMsg
      }

-- | Check if parsing with HSE succeeds.
checkHse :: [String] -> Maybe String -> Text -> Bool
checkHse extNames _langName source =
  let mode = hseParseMode {HSE.extensions = fromExtensionNames extNames}
   in case HSE.parseFileContentsWithMode mode (T.unpack source) of
        HSE.ParseOk _ -> True
        HSE.ParseFailed _ _ -> False

hseParseMode :: HSE.ParseMode
hseParseMode =
  HSE.defaultParseMode
    { HSE.parseFilename = "<stackage-progress>",
      HSE.extensions = []
    }

-- | Whether we need a parsed module for the checks.
needsParsedModule :: [Check] -> Bool
needsParsedModule checks =
  CheckRoundtripGhc `elem` checks || CheckSourceSpan `elem` checks

-- | Check roundtrip via GHC oracle.
checkRoundtrip :: [String] -> Maybe String -> FilePath -> Maybe Text -> Text -> Either String ()
checkRoundtrip extNames langName file cppErrorMsg source' =
  case validateParserDetailedWithExtensionNames extNames langName source' of
    Nothing -> Right ()
    Just err ->
      case validationErrorKind err of
        ValidationParseError ->
          Left (T.unpack (prefixCppErrors cppErrorMsg ("parse failed in " <> T.pack file <> ": " <> T.pack (validationErrorMessage err))))
        ValidationRoundtripError ->
          Left (T.unpack (prefixCppErrors cppErrorMsg ("roundtrip mismatch in " <> T.pack file <> ": " <> T.pack (validationErrorMessage err))))

-- | Check source spans are accurate.
checkSourceSpans :: FilePath -> Text -> Module -> Either String ()
checkSourceSpans file source modu =
  let exprs = [expr | expr <- collectModuleExprs modu, hasRealSourceSpan (exprSpan expr)]
   in case firstLeft (map (validateExprSpan source) exprs) of
        Nothing -> Right ()
        Just err -> Left (file ++ ": " ++ err)

validateExprSpan :: Text -> Expr -> Either String ()
validateExprSpan source expr = do
  snippet <- extractSpanText source (exprSpan expr)
  case Aihc.Parser.parseExpr Aihc.Parser.defaultConfig snippet of
    ParseErr err -> Left ("source-span parse failed for span " ++ show (exprSpan expr) ++ ":\n" ++ Aihc.Parser.errorBundlePretty err)
    ParseOk reparsed ->
      if stripExpr reparsed == stripExpr expr
        then Right ()
        else Left ("source-span mismatch at " ++ show (exprSpan expr))

firstLeft :: [Either a b] -> Maybe a
firstLeft [] = Nothing
firstLeft (x : xs) =
  case x of
    Left err -> Just err
    Right _ -> firstLeft xs

hasRealSourceSpan :: SourceSpan -> Bool
hasRealSourceSpan span' =
  case span' of
    SourceSpan {} -> True
    NoSourceSpan -> False

extractSpanText :: Text -> SourceSpan -> Either String Text
extractSpanText input span' =
  case span' of
    NoSourceSpan -> Left "missing source span"
    SourceSpan sLine sCol eLine eCol
      | sLine <= 0 || sCol <= 0 || eLine <= 0 || eCol <= 0 -> Left "invalid non-positive span coordinates"
      | (eLine, eCol) < (sLine, sCol) -> Left "invalid reversed span"
      | otherwise ->
          let ls = T.splitOn "\n" input
           in if sLine > length ls || eLine > length ls
                then Left "span exceeds input line count"
                else
                  let lineAt n = ls !! (n - 1)
                      startLine = lineAt sLine
                      endLine = lineAt eLine
                   in if sLine == eLine
                        then
                          let startIx = sCol - 1
                              len = eCol - sCol
                           in if startIx < 0 || len < 0 || startIx + len > T.length startLine
                                then Left "single-line span exceeds line bounds"
                                else Right (T.take len (T.drop startIx startLine))
                        else
                          let startIx = sCol - 1
                              endIx = eCol - 1
                              firstPart = T.drop startIx startLine
                              middleParts = [lineAt n | n <- [sLine + 1 .. eLine - 1]]
                              lastPart = T.take endIx endLine
                           in if startIx < 0 || endIx < 0 || startIx > T.length startLine || endIx > T.length endLine
                                then Left "multi-line span exceeds line bounds"
                                else Right (T.intercalate "\n" (firstPart : middleParts <> [lastPart]))

exprSpan :: Expr -> SourceSpan
exprSpan expr =
  case expr of
    EVar span' _ -> span'
    EInt span' _ _ -> span'
    EIntBase span' _ _ -> span'
    EFloat span' _ _ -> span'
    EChar span' _ _ -> span'
    EString span' _ _ -> span'
    EQuasiQuote span' _ _ -> span'
    EIf span' _ _ _ -> span'
    ELambdaPats span' _ _ -> span'
    ELambdaCase span' _ -> span'
    EInfix span' _ _ _ -> span'
    ENegate span' _ -> span'
    ESectionL span' _ _ -> span'
    ESectionR span' _ _ -> span'
    ELetDecls span' _ _ -> span'
    ECase span' _ _ -> span'
    EDo span' _ -> span'
    EListComp span' _ _ -> span'
    EListCompParallel span' _ _ -> span'
    EArithSeq span' _ -> span'
    ERecordCon span' _ _ -> span'
    ERecordUpd span' _ _ -> span'
    ETypeSig span' _ _ -> span'
    EParen span' _ -> span'
    EWhereDecls span' _ _ -> span'
    EList span' _ -> span'
    ETuple span' _ -> span'
    ETupleSection span' _ -> span'
    ETupleCon span' _ -> span'
    ETypeApp span' _ _ -> span'
    EApp span' _ _ -> span'

-- | Collect all expressions from a module.
collectModuleExprs :: Module -> [Expr]
collectModuleExprs modu = concatMap collectDeclExprs (moduleDecls modu)

collectDeclExprs :: Decl -> [Expr]
collectDeclExprs decl =
  case decl of
    DeclValue _ valueDecl -> collectValueDeclExprs valueDecl
    DeclClass _ classDecl -> concatMap collectClassDeclItemExprs (classDeclItems classDecl)
    DeclInstance _ instDecl -> concatMap collectInstanceDeclItemExprs (instanceDeclItems instDecl)
    _ -> []

collectClassDeclItemExprs :: ClassDeclItem -> [Expr]
collectClassDeclItemExprs item =
  case item of
    ClassItemDefault _ valueDecl -> collectValueDeclExprs valueDecl
    _ -> []

collectInstanceDeclItemExprs :: InstanceDeclItem -> [Expr]
collectInstanceDeclItemExprs item =
  case item of
    InstanceItemBind _ valueDecl -> collectValueDeclExprs valueDecl
    _ -> []

collectValueDeclExprs :: ValueDecl -> [Expr]
collectValueDeclExprs valueDecl =
  case valueDecl of
    FunctionBind _ _ matches -> concatMap collectMatchExprs matches
    PatternBind _ pat rhs -> collectPatternExprs pat <> collectRhsExprs rhs

collectMatchExprs :: Match -> [Expr]
collectMatchExprs match =
  concatMap collectPatternExprs (matchPats match)
    <> collectRhsExprs (matchRhs match)

collectRhsExprs :: Rhs -> [Expr]
collectRhsExprs rhs =
  case rhs of
    UnguardedRhs _ expr -> collectExprTree expr
    GuardedRhss _ guarded -> concatMap collectGuardedRhsExprs guarded

collectGuardedRhsExprs :: GuardedRhs -> [Expr]
collectGuardedRhsExprs guarded =
  concatMap collectGuardQualifierExprs (guardedRhsGuards guarded)
    <> collectExprTree (guardedRhsBody guarded)

collectGuardQualifierExprs :: GuardQualifier -> [Expr]
collectGuardQualifierExprs qualifier =
  case qualifier of
    GuardExpr _ expr -> collectExprTree expr
    GuardPat _ pat expr -> collectPatternExprs pat <> collectExprTree expr
    GuardLet _ decls -> concatMap collectDeclExprs decls

collectPatternExprs :: Pattern -> [Expr]
collectPatternExprs pat =
  case pat of
    PView _ viewExpr inner -> collectExprTree viewExpr <> collectPatternExprs inner
    PCon _ _ pats -> concatMap collectPatternExprs pats
    PInfix _ left _ right -> collectPatternExprs left <> collectPatternExprs right
    PAs _ _ inner -> collectPatternExprs inner
    PStrict _ inner -> collectPatternExprs inner
    PIrrefutable _ inner -> collectPatternExprs inner
    PParen _ inner -> collectPatternExprs inner
    PRecord _ _ fields -> concatMap (collectPatternExprs . snd) fields
    PTuple _ pats -> concatMap collectPatternExprs pats
    PList _ pats -> concatMap collectPatternExprs pats
    _ -> []

collectExprTree :: Expr -> [Expr]
collectExprTree expr =
  expr
    : case expr of
      EIf _ c t e -> collectExprTree c <> collectExprTree t <> collectExprTree e
      ELambdaPats _ pats body -> concatMap collectPatternExprs pats <> collectExprTree body
      ELambdaCase _ alts -> concatMap collectCaseAltExprs alts
      EInfix _ l _ r -> collectExprTree l <> collectExprTree r
      ENegate _ e -> collectExprTree e
      ESectionL _ e _ -> collectExprTree e
      ESectionR _ _ e -> collectExprTree e
      ELetDecls _ decls body -> concatMap collectDeclExprs decls <> collectExprTree body
      ECase _ scrutinee alts -> collectExprTree scrutinee <> concatMap collectCaseAltExprs alts
      EDo _ stmts -> concatMap collectDoStmtExprs stmts
      EListComp _ body stmts -> collectExprTree body <> concatMap collectCompStmtExprs stmts
      EListCompParallel _ body stmtGroups ->
        collectExprTree body <> concatMap (concatMap collectCompStmtExprs) stmtGroups
      EArithSeq _ seqExpr -> collectArithSeqExprs seqExpr
      ERecordCon _ _ fields -> concatMap (collectExprTree . snd) fields
      ERecordUpd _ base fields -> collectExprTree base <> concatMap (collectExprTree . snd) fields
      ETypeSig _ e _ -> collectExprTree e
      EParen _ e -> collectExprTree e
      EWhereDecls _ e decls -> collectExprTree e <> concatMap collectDeclExprs decls
      EList _ es -> concatMap collectExprTree es
      ETuple _ es -> concatMap collectExprTree es
      ETypeApp _ e _ -> collectExprTree e
      EApp _ f x -> collectExprTree f <> collectExprTree x
      _ -> []

collectCaseAltExprs :: CaseAlt -> [Expr]
collectCaseAltExprs alt =
  collectPatternExprs (caseAltPattern alt)
    <> collectRhsExprs (caseAltRhs alt)

collectDoStmtExprs :: DoStmt -> [Expr]
collectDoStmtExprs stmt =
  case stmt of
    DoBind _ pat expr -> collectPatternExprs pat <> collectExprTree expr
    DoLet _ binds -> concatMap (collectExprTree . snd) binds
    DoLetDecls _ decls -> concatMap collectDeclExprs decls
    DoExpr _ expr -> collectExprTree expr

collectCompStmtExprs :: CompStmt -> [Expr]
collectCompStmtExprs stmt =
  case stmt of
    CompGen _ pat expr -> collectPatternExprs pat <> collectExprTree expr
    CompGuard _ expr -> collectExprTree expr
    CompLet _ binds -> concatMap (collectExprTree . snd) binds
    CompLetDecls _ decls -> concatMap collectDeclExprs decls

collectArithSeqExprs :: ArithSeq -> [Expr]
collectArithSeqExprs seqExpr =
  case seqExpr of
    ArithSeqFrom _ a -> collectExprTree a
    ArithSeqFromThen _ a b -> collectExprTree a <> collectExprTree b
    ArithSeqFromTo _ a b -> collectExprTree a <> collectExprTree b
    ArithSeqFromThenTo _ a b c -> collectExprTree a <> collectExprTree b <> collectExprTree c
