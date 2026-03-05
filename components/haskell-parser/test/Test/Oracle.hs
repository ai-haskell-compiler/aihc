{-# LANGUAGE OverloadedStrings #-}

module Test.Oracle
  ( oracleCanonicalModule,
    oracleParsesModule,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (ForeignFunctionInterface))
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( ParseResult (..),
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.ForeignCall
  ( CCallConv (CCallConv, StdCallConv),
    CCallTarget (DynamicTarget, StaticTarget),
    CExportSpec (CExportStatic),
    Header,
    Safety (PlayInterruptible, PlayRisky, PlaySafe),
  )
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SourceText (IntegralLit (..))
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (showSDocUnsafe)
import Parser.Canonical

oracleParsesModule :: Text -> Bool
oracleParsesModule input =
  case parseWithGhc input of
    Left _ -> False
    Right _ -> True

oracleCanonicalModule :: Text -> Either Text CanonicalModule
oracleCanonicalModule input = do
  parsed <- parseWithGhc input
  first T.pack (toCanonicalModule parsed)

parseWithGhc :: Text -> Either Text (HsModule GhcPs)
parseWithGhc input =
  let exts = EnumSet.fromList [ForeignFunctionInterface] :: EnumSet.EnumSet Extension
      opts = mkParserOpts exts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<oracle>") 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)

toCanonicalModule :: HsModule GhcPs -> Either String CanonicalModule
toCanonicalModule modu = do
  declGroups <- traverse toCanonicalDecls (hsmodDecls modu)
  pure
    CanonicalModule
      { canonicalModuleName = fmap (T.pack . moduleNameString . unLoc) (hsmodName modu),
        canonicalDecls = concat declGroups
      }

toCanonicalDecls :: LHsDecl GhcPs -> Either String [CanonicalDecl]
toCanonicalDecls locatedDecl =
  case unLoc locatedDecl of
    ValD _ bind -> (: []) <$> toCanonicalValueDecl bind
    SigD _ sig -> toCanonicalSigDecls sig
    ForD _ foreignDecl -> (: []) <$> toCanonicalForeignDecl foreignDecl
    _ -> Left "unsupported declaration kind"

toCanonicalValueDecl :: HsBind GhcPs -> Either String CanonicalDecl
toCanonicalValueDecl bind =
  case bind of
    FunBind {fun_id = locatedName, fun_matches = MG {mg_alts = locatedMatches}} ->
      let name = occNameText (unLoc locatedName)
          matches = unLoc locatedMatches
          maybeExpr = do
            singleMatch <- case matches of
              [m] -> Just (unLoc m)
              _ -> Nothing
            toSimpleBody singleMatch
       in pure $
            case maybeExpr of
              Just expr ->
                CanonicalValueDecl
                  { canonicalDeclName = name,
                    canonicalDeclExpr = expr
                  }
              Nothing ->
                CanonicalFunctionDecl
                  { canonicalFunctionName = name
                  }
    _ -> Left "unsupported value binding"
  where
    toSimpleBody match =
      case m_grhss match of
        GRHSs _ grhss _ ->
          case toList grhss of
            [grhs] ->
              case unLoc grhs of
                GRHS _ [] body ->
                  case toCanonicalExpr (unLoc body) of
                    Right expr -> Just expr
                    Left _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        XGRHSs _ -> Nothing

toCanonicalSigDecls :: Sig GhcPs -> Either String [CanonicalDecl]
toCanonicalSigDecls sig =
  case sig of
    TypeSig _ locatedNames _ ->
      pure
        [ CanonicalTypeSigDecl
            { canonicalTypeSigName = occNameText (unLoc locatedName)
            }
        | locatedName <- locatedNames
        ]
    _ -> Left "unsupported signature declaration"

toCanonicalForeignDecl :: ForeignDecl GhcPs -> Either String CanonicalDecl
toCanonicalForeignDecl foreignDecl =
  case foreignDecl of
    ForeignImport {fd_name = locatedName, fd_fi = fi} -> do
      (callConv, safety, entity) <- toCanonicalForeignImport fi
      pure
        CanonicalForeignDecl
          { canonicalForeignDirection = CanonicalForeignImport,
            canonicalForeignCallConv = callConv,
            canonicalForeignSafety = safety,
            canonicalForeignEntity = entity,
            canonicalForeignName = occNameText (unLoc locatedName)
          }
    ForeignExport {fd_name = locatedName, fd_fe = fe} -> do
      (callConv, entity) <- toCanonicalForeignExport (occNameText (unLoc locatedName)) fe
      pure
        CanonicalForeignDecl
          { canonicalForeignDirection = CanonicalForeignExport,
            canonicalForeignCallConv = callConv,
            canonicalForeignSafety = Nothing,
            canonicalForeignEntity = entity,
            canonicalForeignName = occNameText (unLoc locatedName)
          }

toCanonicalForeignImport :: ForeignImport GhcPs -> Either String (CanonicalCallConv, Maybe CanonicalForeignSafety, Maybe Text)
toCanonicalForeignImport fi =
  case fi of
    CImport _ locatedConv locatedSafety header importSpec -> do
      callConv <- toCanonicalCallConv (unLoc locatedConv)
      safety <- toCanonicalSafety (unLoc locatedSafety)
      pure (callConv, safety, classifyImportEntity header importSpec)

toCanonicalForeignExport :: Text -> ForeignExport GhcPs -> Either String (CanonicalCallConv, Maybe Text)
toCanonicalForeignExport haskellName fe =
  case fe of
    CExport _ locatedSpec ->
      case unLoc locatedSpec of
        CExportStatic _ exportedName callConv -> do
          canonConv <- toCanonicalCallConv callConv
          let entity
                | T.pack (show exportedName) == haskellName = Nothing
                | otherwise = Just "named"
          pure (canonConv, entity)

toCanonicalCallConv :: CCallConv -> Either String CanonicalCallConv
toCanonicalCallConv callConv =
  case callConv of
    CCallConv -> Right CanonicalCCall
    StdCallConv -> Right CanonicalStdCall
    _ -> Left "unsupported calling convention"

toCanonicalSafety :: Safety -> Either String (Maybe CanonicalForeignSafety)
toCanonicalSafety safety =
  case safety of
    PlaySafe -> Right (Just CanonicalSafe)
    PlayRisky -> Right (Just CanonicalUnsafe)
    PlayInterruptible -> Right (Just CanonicalSafe)

classifyImportEntity :: Maybe Header -> CImportSpec -> Maybe Text
classifyImportEntity mHeader importSpec =
  case importSpec of
    CLabel _ -> Just "address"
    CWrapper -> Just "wrapper"
    CFunction DynamicTarget -> Just "dynamic"
    CFunction (StaticTarget _ _ _ isFunction)
      | not isFunction -> Just "address"
      | hasHeader mHeader -> Just "static"
      | otherwise -> Just "named"
  where
    hasHeader = isJust

occNameText :: RdrName -> Text
occNameText = T.pack . occNameString . rdrNameOcc

toCanonicalExpr :: HsExpr GhcPs -> Either String CanonicalExpr
toCanonicalExpr expr =
  case expr of
    HsVar _ locatedName ->
      let name = unLoc locatedName
       in Right (CVar (T.pack (occNameString (rdrNameOcc name))))
    HsPar _ inner -> toCanonicalExpr (unLoc inner)
    HsApp _ f x -> CApp <$> toCanonicalExpr (unLoc f) <*> toCanonicalExpr (unLoc x)
    HsOverLit _ lit ->
      case ol_val lit of
        HsIntegral (IL _ _ value) -> Right (CInt value)
        _ -> Left "unsupported literal"
    _ -> Left "unsupported expression"
