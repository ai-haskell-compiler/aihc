{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GhcOracle
  ( oracleParsesModuleWithExtensions,
    oracleParsesModuleWithExtensionsAt,
    oracleModuleAstFingerprintWithExtensions,
    oracleModuleAstFingerprintWithExtensionsAt,
    oracleParsesModuleWithNames,
    oracleParsesModuleWithNamesAt,
    oracleDetailedParsesModuleWithNames,
    oracleDetailedParsesModuleWithNamesAt,
    toGhcExtension,
    fromGhcExtension,
  )
where

import Control.Exception (catch, displayException, evaluate)
import Cpp (resultOutput)
import CppSupport (moduleHeaderExtensionSettings, preprocessForParserWithoutIncludes)
import Data.List (nub)
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Driver.DynFlags as DynFlags
import GHC.Driver.Session (impliedXFlags)
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsModule)
import qualified GHC.LanguageExtensions.Type as GHC
import GHC.Parser (parseModule)
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer
  ( ParseResult (..),
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SourceError (SourceError)
import GHC.Types.SrcLoc (GenLocated, mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import qualified Parser.Ast as Ast
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (foldl')

oracleParsesModuleWithExtensions :: [GHC.Extension] -> Text -> Bool
oracleParsesModuleWithExtensions = oracleParsesModuleWithExtensionsAt "oracle"

oracleParsesModuleWithExtensionsAt :: String -> [GHC.Extension] -> Text -> Bool
oracleParsesModuleWithExtensionsAt sourceTag exts input =
  case parseWithGhcWithExtensions sourceTag exts input of
    Left _ -> False
    Right _ -> True

oracleModuleAstFingerprintWithExtensions :: [GHC.Extension] -> Text -> Either Text Text
oracleModuleAstFingerprintWithExtensions = oracleModuleAstFingerprintWithExtensionsAt "oracle"

oracleModuleAstFingerprintWithExtensionsAt :: String -> [GHC.Extension] -> Text -> Either Text Text
oracleModuleAstFingerprintWithExtensionsAt sourceTag exts input = do
  (pragmas, parsed) <- parseWithGhcWithExtensions sourceTag exts input
  let pragmaFingerprint =
        if null pragmas
          then ""
          else "LANGUAGE " <> T.intercalate "," (map Ast.extensionSettingName pragmas) <> "\n"
  pure (pragmaFingerprint <> T.pack (showSDocUnsafe (ppr parsed)))

parseWithGhcWithExtensions :: String -> [GHC.Extension] -> Text -> Either Text ([Ast.ExtensionSetting], HsModule GhcPs)
parseWithGhcWithExtensions sourceTag extraExts input =
  let baseExts = nub extraExts
   in do
        languagePragmas <- extractLanguagePragmas sourceTag baseExts input
        let parseExts =
              applyImpliedExtensions
                (List.foldl' applyExtensionSetting (EnumSet.fromList baseExts :: EnumSet.EnumSet GHC.Extension) languagePragmas)
            inputForParse =
              if EnumSet.member GHC.Cpp parseExts
                then resultOutput (preprocessForParserWithoutIncludes sourceTag input)
                else input
            opts = mkParserOpts parseExts emptyDiagOpts False False False True
            buffer = stringToStringBuffer (T.unpack inputForParse)
            start = mkRealSrcLoc (mkFastString sourceTag) 1 1
        case catchPureExceptionText $ case unP parseModule (initParserState opts buffer start) of
          POk _ modu -> Right (languagePragmas, unLoc modu)
          PFailed st ->
            let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
             in Left (T.pack rendered) of
          Left err -> Left ("GHC parser exception: " <> err)
          Right result -> result

applyExtensionSetting :: EnumSet.EnumSet GHC.Extension -> Ast.ExtensionSetting -> EnumSet.EnumSet GHC.Extension
applyExtensionSetting exts setting =
  case setting of
    Ast.EnableExtension ext ->
      maybe exts (`EnumSet.insert` exts) (toGhcExtension ext)
    Ast.DisableExtension ext ->
      maybe exts (`EnumSet.delete` exts) (toGhcExtension ext)

extractLanguagePragmas :: String -> [GHC.Extension] -> Text -> Either Text [Ast.ExtensionSetting]
extractLanguagePragmas sourceTag baseExts input =
  let headerPragmas = moduleHeaderExtensionSettings input
      headerExts =
        applyImpliedExtensions
          (List.foldl' applyExtensionSetting (EnumSet.fromList baseExts :: EnumSet.EnumSet GHC.Extension) headerPragmas)
      inputForOptions =
        if EnumSet.member GHC.Cpp headerExts
          then resultOutput (preprocessForParserWithoutIncludes sourceTag input)
          else input
      buffer = stringToStringBuffer (T.unpack inputForOptions)
      baseOpts =
        mkParserOpts
          (EnumSet.fromList baseExts :: EnumSet.EnumSet GHC.Extension)
          emptyDiagOpts
          False
          False
          False
          True
   in case catchPureExceptionText
        ( let (_warns, rawOptions) = getOptions baseOpts supportedLanguagePragmas buffer sourceTag
              optionPragmas = mapMaybe optionToLanguagePragma rawOptions
              pragmas = nub (headerPragmas <> optionPragmas)
           in length pragmas `seq` pragmas
        ) of
        Left err -> Left ("GHC option parsing exception: " <> err)
        Right pragmas -> Right pragmas

optionToLanguagePragma :: GenLocated l String -> Maybe Ast.ExtensionSetting
optionToLanguagePragma locatedOpt =
  let opt = T.pack (unLoc locatedOpt)
   in case T.stripPrefix "-X" opt of
        Just pragmaName | not (T.null pragmaName) -> Ast.parseExtensionSettingName pragmaName
        _ -> Nothing

applyImpliedExtensions :: EnumSet.EnumSet GHC.Extension -> EnumSet.EnumSet GHC.Extension
applyImpliedExtensions start = go start
  where
    go exts =
      let next = List.foldl' apply exts impliedXFlags
       in if EnumSet.toList next == EnumSet.toList exts then exts else go next

    apply exts (enabled, implied)
      | EnumSet.member enabled exts =
          case implied of
            DynFlags.On ext -> EnumSet.insert ext exts
            DynFlags.Off ext -> EnumSet.delete ext exts
      | otherwise = exts

supportedLanguagePragmas :: [String]
supportedLanguagePragmas =
  [ "CPP",
    "Safe",
    "Trustworthy",
    "Unsafe",
    "Rank2Types",
    "PolymorphicComponents",
    "GeneralisedNewtypeDeriving",
    "NoGeneralisedNewtypeDeriving"
  ]
    <> concatMap (includeNegative . show) ([minBound .. maxBound] :: [GHC.Extension])
  where
    includeNegative extName =
      case extName of
        "Cpp" -> [extName, "NoCPP", "NoCpp"]
        _ -> [extName, "No" <> extName]

catchPureExceptionText :: a -> Either Text a
catchPureExceptionText value =
  unsafePerformIO $ do
    (Right <$> evaluate value)
      `catch` \(err :: SourceError) ->
        pure (Left (T.pack (displayException err)))
{-# NOINLINE catchPureExceptionText #-}

oracleParsesModuleWithNames :: [String] -> Maybe String -> Text -> Bool
oracleParsesModuleWithNames = oracleParsesModuleWithNamesAt "oracle"

oracleParsesModuleWithNamesAt :: String -> [String] -> Maybe String -> Text -> Bool
oracleParsesModuleWithNamesAt sourceTag extNames langName input =
  case oracleDetailedParsesModuleWithNamesAt sourceTag extNames langName input of
    Left _ -> False
    Right _ -> True

oracleDetailedParsesModuleWithNames :: [String] -> Maybe String -> Text -> Either Text ()
oracleDetailedParsesModuleWithNames = oracleDetailedParsesModuleWithNamesAt "oracle"

oracleDetailedParsesModuleWithNamesAt :: String -> [String] -> Maybe String -> Text -> Either Text ()
oracleDetailedParsesModuleWithNamesAt sourceTag extNames langName input =
  let extSettings = mapMaybe (Ast.parseExtensionSettingName . T.pack) extNames
      langExts = maybe [] languageExtensions langName
      allExts = EnumSet.toList (List.foldl' applyExtensionSetting (EnumSet.fromList langExts) extSettings)
   in case parseWithGhcWithExtensions sourceTag allExts input of
        Left err ->
          let extList = T.pack (show extNames)
              langInfo = maybe "" (\l -> " Language: " <> T.pack l) langName
           in Left (err <> "\n(Extensions: " <> extList <> langInfo <> ")")
        Right _ -> Right ()

toGhcExtension :: Ast.Extension -> Maybe GHC.Extension
toGhcExtension ext =
  lookup (toGhcExtensionName ext) [(show ghcExt, ghcExt) | ghcExt <- [minBound .. maxBound]]
  where
    toGhcExtensionName Ast.CPP = "Cpp"
    toGhcExtensionName Ast.GeneralizedNewtypeDeriving = "GeneralisedNewtypeDeriving"
    toGhcExtensionName Ast.SafeHaskell = "Safe"
    toGhcExtensionName Ast.Trustworthy = "Trustworthy"
    toGhcExtensionName Ast.UnsafeHaskell = "Unsafe"
    toGhcExtensionName Ast.Rank2Types = "RankNTypes"
    toGhcExtensionName Ast.PolymorphicComponents = "RankNTypes"
    toGhcExtensionName other = T.unpack (Ast.extensionName other)

fromGhcExtension :: GHC.Extension -> Maybe Ast.Extension
fromGhcExtension ghcExt = Ast.parseExtensionName (T.pack (show ghcExt))

languageExtensions :: String -> [GHC.Extension]
languageExtensions lang =
  case parseLanguage lang of
    Just ghcLanguage -> DynFlags.languageExtensions (Just ghcLanguage)
    Nothing -> []

parseLanguage :: String -> Maybe DynFlags.Language
parseLanguage lang =
  case lang of
    "Haskell98" -> Just DynFlags.Haskell98
    "Haskell2010" -> Just DynFlags.Haskell2010
    "GHC2021" -> Just DynFlags.GHC2021
    "GHC2024" -> Just DynFlags.GHC2024
    _ -> Nothing
