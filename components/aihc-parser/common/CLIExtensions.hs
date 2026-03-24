{-# LANGUAGE OverloadedStrings #-}

-- | Shared CLI option parsers for language extensions.
--
-- This module provides reusable optparse-applicative parsers for handling
-- GHC-style language extension flags (-X).
module CLIExtensions
  ( -- * Option parsers
    extensionOption,
    extensionsOption,
    parseExtension,
  )
where

import Aihc.Parser.Ast (Extension, ExtensionSetting (..), parseExtensionSettingName)
import qualified Data.Text as T
import Options.Applicative

-- | Parser for a single -X extension flag.
--
-- Usage: @many extensionOption@ to collect all -X flags.
extensionOption :: Parser Extension
extensionOption =
  option
    parseExtension
    ( short 'X'
        <> metavar "EXTENSION"
        <> help "Enable a language extension (e.g., -XNegativeLiterals)"
    )

-- | Parser for multiple extensions.
extensionsOption :: Parser [Extension]
extensionsOption = many extensionOption

-- | ReadM parser for extension names.
--
-- Accepts extension names like "NegativeLiterals" or "NoMagicHash".
-- Returns an error for unknown extensions or explicit disabling with -X.
parseExtension :: ReadM Extension
parseExtension = eitherReader $ \s ->
  case parseExtensionSettingName (T.pack s) of
    Just (EnableExtension ext) -> Right ext
    Just (DisableExtension _) -> Left ("Cannot disable extension with -X: " <> s)
    Nothing -> Left ("Unknown extension: " <> s)
