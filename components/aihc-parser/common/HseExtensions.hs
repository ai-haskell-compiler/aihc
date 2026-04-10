module HseExtensions
  ( toHseExtension,
    fromHseExtension,
    toHseExtensions,
    toHseExtensionSetting,
    toHseExtensionSettings,
    fromExtensionNames,
    fromParserExtensions,
  )
where

import Aihc.Parser.Syntax qualified as Syntax
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text qualified as T
import Language.Haskell.Exts qualified as HSE
import Text.Read (readMaybe)

toHseExtension :: Syntax.Extension -> Maybe HSE.Extension
toHseExtension ext =
  HSE.EnableExtension <$> toHseKnownExtension ext

fromHseExtension :: HSE.Extension -> Maybe Syntax.Extension
fromHseExtension hseExt =
  case hseExt of
    HSE.EnableExtension known -> Syntax.parseExtensionName (T.pack (show known))
    HSE.DisableExtension known -> Syntax.parseExtensionName (T.pack (show known))
    HSE.UnknownExtension name -> Syntax.parseExtensionName (T.pack name)

toHseExtensions :: [Syntax.Extension] -> [HSE.Extension]
toHseExtensions = mapMaybe toHseExtension

toHseExtensionSetting :: Syntax.ExtensionSetting -> Maybe HSE.Extension
toHseExtensionSetting setting =
  case setting of
    Syntax.EnableExtension ext -> toHseExtension ext
    Syntax.DisableExtension ext -> HSE.DisableExtension <$> toHseKnownExtension ext

toHseExtensionSettings :: [Syntax.ExtensionSetting] -> [HSE.Extension]
toHseExtensionSettings = mapMaybe toHseExtensionSetting

fromExtensionNames :: [String] -> [HSE.Extension]
fromExtensionNames names =
  toHseExtensionSettings (mapMaybe (Syntax.parseExtensionSettingName . T.pack) names)

-- | Convert a list of parser extensions to HSE extensions.
-- This is the preferred way to convert extensions when using unified extension handling.
fromParserExtensions :: [Syntax.Extension] -> [HSE.Extension]
fromParserExtensions = toHseExtensions

toHseKnownExtension :: Syntax.Extension -> Maybe HSE.KnownExtension
toHseKnownExtension ext =
  listToMaybe [known | name <- hseKnownNameCandidates ext, Just known <- [readMaybe name]]

hseKnownNameCandidates :: Syntax.Extension -> [String]
hseKnownNameCandidates ext =
  case ext of
    Syntax.CPP -> ["CPP", "Cpp"]
    Syntax.GeneralizedNewtypeDeriving -> ["GeneralizedNewtypeDeriving", "GeneralisedNewtypeDeriving"]
    _ -> [T.unpack (Syntax.extensionName ext)]
