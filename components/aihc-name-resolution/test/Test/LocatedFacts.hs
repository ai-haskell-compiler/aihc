{-# LANGUAGE OverloadedStrings #-}

module Test.LocatedFacts
  ( BinderSite (..),
    DiagnosticFact (..),
    LocatedFacts (..),
    UseSite (..),
    normalizeLocatedFacts,
    oracleLocatedFacts,
    oursLocatedFacts,
    renderLocatedFactsDiff,
  )
where

import Control.Monad (foldM)
import Data.Char (isAlphaNum, isLower)
import Data.Foldable (toList)
import Data.List (find, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension)
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( ParseResult (..),
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SourceText (IntegralLit (..))
import GHC.Types.SrcLoc (SrcSpan (RealSrcSpan), mkRealSrcLoc, srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (showSDocUnsafe)
import Parser (defaultConfig)
import qualified Parser
import Parser.Types (ParseResult (..))
import Resolver (resolveModule)
import Resolver.Ast
import Resolver.Types

data BinderSite = BinderSite
  { bsKey :: !Text,
    bsName :: !Text,
    bsClass :: !NameClass,
    bsSpan :: !Span
  }
  deriving (Eq, Ord, Show)

data UseSite = UseSite
  { usName :: !Text,
    usClass :: !NameClass,
    usSpan :: !Span,
    usResolvesTo :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show)

data DiagnosticFact = DiagnosticFact
  { dfCode :: !DiagnosticCode,
    dfSpan :: !Span
  }
  deriving (Eq, Ord, Show)

data LocatedFacts = LocatedFacts
  { lfModuleName :: !(Maybe Text),
    lfBinders :: ![BinderSite],
    lfUses :: ![UseSite],
    lfDiagnostics :: ![DiagnosticFact]
  }
  deriving (Eq, Show)

data OracleDecl = OracleDecl
  { oracleDeclName :: !Text,
    oracleDeclExpr :: !(HsExpr GhcPs),
    oracleDeclSpan :: !Span
  }

normalizeLocatedFacts :: LocatedFacts -> LocatedFacts
normalizeLocatedFacts lf =
  lf
    { lfBinders = sort (lfBinders lf),
      lfUses = sort (lfUses lf),
      lfDiagnostics = sort (lfDiagnostics lf)
    }

renderLocatedFactsDiff :: LocatedFacts -> LocatedFacts -> String
renderLocatedFactsDiff ours oracleFacts =
  unlines
    [ "binders(ours): " <> show (lfBinders ours),
      "binders(oracle): " <> show (lfBinders oracleFacts),
      "uses(ours): " <> show (lfUses ours),
      "uses(oracle): " <> show (lfUses oracleFacts),
      "diagnostics(ours): " <> show (lfDiagnostics ours),
      "diagnostics(oracle): " <> show (lfDiagnostics oracleFacts)
    ]

oursLocatedFacts :: ResolveConfig -> Text -> Either Text LocatedFacts
oursLocatedFacts cfg source =
  case Parser.parseModule defaultConfig source of
    ParseErr err -> Left (T.pack (show err))
    ParseOk modu ->
      let rr = resolveModule cfg modu
          resolvedMod = resolved rr
          decls = resolvedDecls resolvedMod
          binderSpans = locateTopLevelBinders source (map resolvedDeclName decls)
          binderSites = mkBinderSites decls binderSpans
          idToKey = firstBindingKeyById binderSites decls
          uses = concat (zipWith (collectDeclUses source idToKey) decls binderSpans)
          diagnosticsWithSpans = attachDiagnosticSpans (diagnostics rr) binderSites uses
       in Right
            ( normalizeLocatedFacts
                LocatedFacts
                  { lfModuleName = resolvedModuleName resolvedMod,
                    lfBinders = binderSites,
                    lfUses = uses,
                    lfDiagnostics = diagnosticsWithSpans
                  }
            )

oracleLocatedFacts :: ResolveConfig -> Text -> Either Text LocatedFacts
oracleLocatedFacts cfg source = do
  modu <- parseWithGhc source
  decls <- extractDecls modu
  let moduleName = fmap (T.pack . moduleNameString . unLoc) (hsmodName modu)
  buildOracleFacts cfg moduleName decls

buildOracleFacts :: ResolveConfig -> Maybe Text -> [OracleDecl] -> Either Text LocatedFacts
buildOracleFacts cfg moduleName decls = do
  let binderSites = mkOracleBinderSites decls
      firstDeclKey = firstOracleBindingKey decls
      duplicateSpans = duplicateDeclSpans decls
  (useSites, unresolvedSpans) <- foldM (resolveDeclExpr cfg firstDeclKey) ([], []) decls
  let diagFacts =
        map (\sp -> DiagnosticFact {dfCode = EDuplicateBinding, dfSpan = sp}) duplicateSpans
          <> map (\sp -> DiagnosticFact {dfCode = EUnboundVariable, dfSpan = sp}) unresolvedSpans
  pure
    ( normalizeLocatedFacts
        LocatedFacts
          { lfModuleName = moduleName,
            lfBinders = binderSites,
            lfUses = useSites,
            lfDiagnostics = diagFacts
          }
    )
  where
    resolveDeclExpr cfg' keyMap (accUses, accUnbound) decl = do
      (uses, unboundSpans) <- resolveExpr cfg' keyMap (oracleDeclExpr decl)
      pure (accUses <> uses, accUnbound <> unboundSpans)

resolveExpr ::
  ResolveConfig ->
  M.Map Text Text ->
  HsExpr GhcPs ->
  Either Text ([UseSite], [Span])
resolveExpr cfg declKeyMap expr =
  case expr of
    HsPar _ inner -> resolveExpr cfg declKeyMap (unLoc inner)
    HsApp _ f x -> do
      (fUses, fUnbound) <- resolveExpr cfg declKeyMap (unLoc f)
      (xUses, xUnbound) <- resolveExpr cfg declKeyMap (unLoc x)
      pure (fUses <> xUses, fUnbound <> xUnbound)
    HsVar _ locatedName ->
      let name = T.pack (occNameString (rdrNameOcc (unLoc locatedName)))
          span' = ghcSpanToSpan (getLocA locatedName)
       in case M.lookup name declKeyMap of
            Just key ->
              pure
                ( [ UseSite
                      { usName = name,
                        usClass = TopLevelBinder,
                        usSpan = span',
                        usResolvesTo = Just key
                      }
                  ],
                  []
                )
            Nothing ->
              if isPreludeName cfg name
                then
                  pure
                    ( [ UseSite
                          { usName = name,
                            usClass = PreludeBinder,
                            usSpan = span',
                            usResolvesTo = Just ("external:" <> name)
                          }
                      ],
                      []
                    )
                else
                  pure
                    ( [ UseSite
                          { usName = name,
                            usClass = Unresolved,
                            usSpan = span',
                            usResolvesTo = Nothing
                          }
                      ],
                      [span']
                    )
    HsOverLit _ lit ->
      case ol_val lit of
        HsIntegral IL {} -> pure ([], [])
        _ -> Left "unsupported literal"
    _ -> Left "unsupported expression"

extractDecls :: HsModule GhcPs -> Either Text [OracleDecl]
extractDecls modu = traverse toOracleDecl (hsmodDecls modu)

toOracleDecl :: LHsDecl GhcPs -> Either Text OracleDecl
toOracleDecl locatedDecl =
  case unLoc locatedDecl of
    ValD _ bind ->
      case bind of
        FunBind {fun_id = locatedName, fun_matches = MG {mg_alts = locatedMatches}} -> do
          let name = T.pack (occNameString (rdrNameOcc (unLoc locatedName)))
              span' = ghcSpanToSpan (getLocA locatedName)
          match <-
            case unLoc locatedMatches of
              [singleMatch] -> Right (unLoc singleMatch)
              _ -> Left "unsupported multiple matches"
          expr <-
            case m_grhss match of
              GRHSs _ grhss _ ->
                case toList grhss of
                  [grhs] ->
                    case unLoc grhs of
                      GRHS _ [] body -> Right (unLoc body)
                      _ -> Left "unsupported guarded rhs"
                  _ -> Left "unsupported function rhs"
          pure (OracleDecl {oracleDeclName = name, oracleDeclExpr = expr, oracleDeclSpan = span'})
        _ -> Left "unsupported value binding"
    _ -> Left "unsupported declaration kind"

parseWithGhc :: Text -> Either Text (HsModule GhcPs)
parseWithGhc input =
  let opts = mkParserOpts (EnumSet.empty :: EnumSet.EnumSet Extension) emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<oracle>") 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)

mkOracleBinderSites :: [OracleDecl] -> [BinderSite]
mkOracleBinderSites =
  map
    ( \decl ->
        BinderSite
          { bsKey = binderKey (oracleDeclName decl) (oracleDeclSpan decl),
            bsName = oracleDeclName decl,
            bsClass = TopLevelBinder,
            bsSpan = oracleDeclSpan decl
          }
    )

firstOracleBindingKey :: [OracleDecl] -> M.Map Text Text
firstOracleBindingKey = foldl step M.empty
  where
    step acc decl =
      M.insertWith (\_ existing -> existing) (oracleDeclName decl) (binderKey (oracleDeclName decl) (oracleDeclSpan decl)) acc

duplicateDeclSpans :: [OracleDecl] -> [Span]
duplicateDeclSpans = snd . foldl step (S.empty, [])
  where
    step (seen, spans) decl =
      if S.member (oracleDeclName decl) seen
        then (seen, spans <> [oracleDeclSpan decl])
        else (S.insert (oracleDeclName decl) seen, spans)

locateTopLevelBinders :: Text -> [Text] -> [Span]
locateTopLevelBinders source = go 1
  where
    ls = zip [1 ..] (T.lines source)
    go _ [] = []
    go fromLine (name : rest) =
      case findBinderLine fromLine name ls of
        Nothing -> NoSpan : go fromLine rest
        Just (ln, col) ->
          let sp =
                Span
                  { startLine = ln,
                    startCol = col,
                    endLine = ln,
                    endCol = col + T.length name
                  }
           in sp : go (ln + 1) rest

findBinderLine :: Int -> Text -> [(Int, Text)] -> Maybe (Int, Int)
findBinderLine fromLine name =
  foldl step Nothing
  where
    step acc (ln, raw)
      | ln < fromLine = acc
      | otherwise =
          case acc of
            Just _ -> acc
            Nothing -> matchLine ln raw

    matchLine ln raw =
      let trimmed = T.dropWhile (\c -> c == ' ' || c == '\t') raw
          leadingSpaces = T.length raw - T.length trimmed
          prefix = name <> " "
          binderAtStart =
            trimmed == name
              || prefix `T.isPrefixOf` trimmed
              || (name <> "=") `T.isPrefixOf` trimmed
              || (name <> " =") `T.isPrefixOf` trimmed
       in if binderAtStart && T.any (== '=') trimmed
            then Just (ln, leadingSpaces + 1)
            else Nothing

mkBinderSites :: [ResolvedDecl] -> [Span] -> [BinderSite]
mkBinderSites =
  zipWith
    ( \decl span' ->
        BinderSite
          { bsKey = binderKey (resolvedDeclName decl) span',
            bsName = resolvedDeclName decl,
            bsClass = TopLevelBinder,
            bsSpan = span'
          }
    )

firstBindingKeyById :: [BinderSite] -> [ResolvedDecl] -> M.Map NameId Text
firstBindingKeyById sites decls =
  foldl
    (\acc (site, decl) -> M.insertWith (\_ old -> old) (resolvedDeclId decl) (bsKey site) acc)
    M.empty
    (zip sites decls)

collectDeclUses :: Text -> M.Map NameId Text -> ResolvedDecl -> Span -> [UseSite]
collectDeclUses source idToKey decl declSpan =
  let tokenSpans = rhsTokenSpans source declSpan
      vars = collectResolvedVars (resolvedDeclExpr decl)
      initialUsed = S.empty :: S.Set Int
   in snd (foldl (step tokenSpans) (initialUsed, []) vars)
  where
    step tokens (usedIx, acc) name =
      let mIx =
            find
              (\(ix, (tokName, _)) -> tokName == rnText name && not (S.member ix usedIx))
              (zip [0 ..] tokens)
          span' = maybe NoSpan (\(_, (_, sp)) -> sp) mIx
          usedIx' = maybe usedIx (\(ix, _) -> S.insert ix usedIx) mIx
          resolvedTo =
            case rnClass name of
              TopLevelBinder -> rnId name >>= (`M.lookup` idToKey)
              PreludeBinder -> Just ("external:" <> rnText name)
              ImportedBinder -> Just ("external:" <> rnText name)
              LocalBinder -> Just ("external:" <> rnText name)
              Unresolved -> Nothing
          site =
            UseSite
              { usName = rnText name,
                usClass = rnClass name,
                usSpan = span',
                usResolvesTo = resolvedTo
              }
       in (usedIx', acc <> [site])

collectResolvedVars :: ResolvedExpr -> [ResolvedName]
collectResolvedVars expr =
  case expr of
    RInt _ -> []
    RApp f x -> collectResolvedVars f <> collectResolvedVars x
    RVar name -> [name]

rhsTokenSpans :: Text -> Span -> [(Text, Span)]
rhsTokenSpans source declSpan =
  case declSpan of
    NoSpan -> []
    Span {startLine = sLine, endLine = _} ->
      let ls = zip [1 ..] (T.lines source)
          endLine' =
            case nextTopLevelLineAfter source sLine of
              Nothing -> length ls
              Just ln -> ln - 1
          rows = [(ln, txt) | (ln, txt) <- ls, ln >= sLine && ln <= endLine']
       in concatMap (scanLine rows) rows
  where
    scanLine rows (ln, txt) =
      let stripped = stripLineComment txt
          firstDeclLine = maybe False (\(firstLn, _) -> firstLn == ln) (safeHead rows)
          (offset, segment) =
            if firstDeclLine
              then case T.breakOn "=" stripped of
                (lhs, rhs)
                  | T.null rhs -> (1, stripped)
                  | otherwise -> (T.length lhs + 2, T.drop 1 rhs)
              else (1, stripped)
          tokens = scanVarTokens segment
       in map
            ( \(nm, c0, c1) ->
                ( nm,
                  Span
                    { startLine = ln,
                      startCol = offset + c0 - 1,
                      endLine = ln,
                      endCol = offset + c1 - 1
                    }
                )
            )
            tokens

nextTopLevelLineAfter :: Text -> Int -> Maybe Int
nextTopLevelLineAfter source afterLine =
  let ls = zip [1 ..] (T.lines source)
   in fst <$> safeHead [row | row@(ln, raw) <- ls, ln > afterLine, isTopLevelDeclLine raw]

isTopLevelDeclLine :: Text -> Bool
isTopLevelDeclLine raw =
  let trimmed = T.dropWhile (\c -> c == ' ' || c == '\t') raw
      starts = ["module ", "import ", "--", "{-#", "type ", "data ", "newtype ", "class ", "instance ", "default ", "foreign "]
   in not (T.null trimmed)
        && T.head raw /= ' '
        && T.head raw /= '\t'
        && T.any (== '=') trimmed
        && not (any (`T.isPrefixOf` trimmed) starts)

stripLineComment :: Text -> Text
stripLineComment line = fst (T.breakOn "--" line)

scanVarTokens :: Text -> [(Text, Int, Int)]
scanVarTokens txt = go 1 txt []
  where
    go _ rest acc
      | T.null rest = reverse acc
    go col rest acc =
      case T.uncons rest of
        Nothing -> reverse acc
        Just (ch, xs)
          | isVarStart ch ->
              let (tok, tailTxt) = T.span isVarTail rest
                  len = T.length tok
               in go (col + len) tailTxt ((tok, col, col + len) : acc)
          | otherwise -> go (col + 1) xs acc

    isVarStart c = isLower c || c == '_'
    isVarTail c = isAlphaNum c || c == '_' || c == '\''

attachDiagnosticSpans :: [Diagnostic] -> [BinderSite] -> [UseSite] -> [DiagnosticFact]
attachDiagnosticSpans diags binders uses = out
  where
    (_, _, out) = foldl step (dupSpans, unboundSpans, []) diags
    dupSpans = duplicateBinderSpans binders
    unboundSpans = [usSpan u | u <- uses, usClass u == Unresolved]
    step (dupQ, unboundQ, acc) diag =
      case diagCode diag of
        EDuplicateBinding ->
          case dupQ of
            [] -> (dupQ, unboundQ, acc <> [DiagnosticFact {dfCode = EDuplicateBinding, dfSpan = NoSpan}])
            (sp : rest) -> (rest, unboundQ, acc <> [DiagnosticFact {dfCode = EDuplicateBinding, dfSpan = sp}])
        EUnboundVariable ->
          case unboundQ of
            [] -> (dupQ, unboundQ, acc <> [DiagnosticFact {dfCode = EUnboundVariable, dfSpan = NoSpan}])
            (sp : rest) -> (dupQ, rest, acc <> [DiagnosticFact {dfCode = EUnboundVariable, dfSpan = sp}])
        code ->
          (dupQ, unboundQ, acc <> [DiagnosticFact {dfCode = code, dfSpan = NoSpan}])

duplicateBinderSpans :: [BinderSite] -> [Span]
duplicateBinderSpans = snd . foldl step (S.empty, [])
  where
    step (seen, spans) site =
      if S.member (bsName site) seen
        then (seen, spans <> [bsSpan site])
        else (S.insert (bsName site) seen, spans)

binderKey :: Text -> Span -> Text
binderKey name span' = name <> "@" <> renderSpan span'

renderSpan :: Span -> Text
renderSpan NoSpan = "nospan"
renderSpan (Span sl sc el ec) =
  T.pack (show sl <> ":" <> show sc <> "-" <> show el <> ":" <> show ec)

ghcSpanToSpan :: SrcSpan -> Span
ghcSpanToSpan sp =
  case sp of
    RealSrcSpan real _ ->
      Span
        { startLine = srcSpanStartLine real,
          startCol = srcSpanStartCol real,
          endLine = srcSpanEndLine real,
          endCol = srcSpanEndCol real
        }
    _ -> NoSpan

isPreludeName :: ResolveConfig -> Text -> Bool
isPreludeName cfg name =
  case preludeMode cfg of
    ImplicitPreludeOff -> False
    ImplicitPreludeOn -> name `elem` preludeNames

preludeNames :: [Text]
preludeNames = ["return", ">>=", ">>", "fail", "fromInteger", "ifThenElse"]

safeHead :: [a] -> Maybe a
safeHead xs =
  case xs of
    [] -> Nothing
    (y : _) -> Just y
