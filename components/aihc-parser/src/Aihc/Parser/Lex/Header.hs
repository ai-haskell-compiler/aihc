module Aihc.Parser.Lex.Header
  ( enabledExtensionsFromSettings,
    readModuleHeaderExtensionsFromTokens,
    separateEditionAndExtensions,
  )
where

import Aihc.Parser.Lex.Types
import Aihc.Parser.Syntax
import Data.List qualified as List

readModuleHeaderExtensionsFromTokens :: [LexToken] -> [ExtensionSetting]
readModuleHeaderExtensionsFromTokens = go
  where
    go toks =
      case toks of
        LexToken {lexTokenKind = TkPragmaLanguage settings} : rest -> settings <> go rest
        LexToken {lexTokenKind = TkPragmaWarning _} : rest -> go rest
        LexToken {lexTokenKind = TkPragmaDeprecated _} : rest -> go rest
        LexToken {lexTokenKind = TkError _} : _ -> []
        _ -> []

separateEditionAndExtensions :: [ExtensionSetting] -> ModuleHeaderPragmas
separateEditionAndExtensions settings =
  let (editions, extensions) = List.partition isEditionSetting settings
      lastEdition = case reverse editions of
        EnableExtension ext : _ -> extensionToEdition ext
        _ -> Nothing
   in ModuleHeaderPragmas
        { headerLanguageEdition = lastEdition,
          headerExtensionSettings = extensions
        }

isEditionSetting :: ExtensionSetting -> Bool
isEditionSetting setting =
  case setting of
    EnableExtension ext -> isEditionExtension ext
    DisableExtension ext -> isEditionExtension ext

isEditionExtension :: Extension -> Bool
isEditionExtension ext =
  ext `elem` [Haskell98, Haskell2010, GHC2021, GHC2024]

extensionToEdition :: Extension -> Maybe LanguageEdition
extensionToEdition ext =
  case ext of
    Haskell98 -> Just Haskell98Edition
    Haskell2010 -> Just Haskell2010Edition
    GHC2021 -> Just GHC2021Edition
    GHC2024 -> Just GHC2024Edition
    _ -> Nothing

enabledExtensionsFromSettings :: [ExtensionSetting] -> [Extension]
enabledExtensionsFromSettings = List.foldl' apply []
  where
    apply exts setting =
      case setting of
        EnableExtension ext
          | ext `elem` exts -> exts
          | otherwise -> exts <> [ext]
        DisableExtension ext -> filter (/= ext) exts
