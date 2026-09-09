-- |
-- Module      : Aihc.Language.Extensions
-- Description : The language extensions the phases after parsing work from
--
-- Language pragmas are a source-level notion. The driver that reads a module
-- folds the language edition, the package's default extensions and the
-- module's own @LANGUAGE@ pragmas into one extension set and hands that set
-- to name resolution and to the type checker as data. No phase after the
-- parser reads 'moduleLanguagePragmas' itself.
--
-- The module lives in @aihc-resolve@ because that is the package every later
-- phase already depends on.
module Aihc.Language.Extensions
  ( modulePragmaExtensions,
  )
where

import Aihc.Parser.Syntax
  ( Extension (..),
    ExtensionSetting (..),
    Module (..),
    applyExtensionSetting,
    applyImpliedExtensions,
  )

-- | The extensions in force when nothing says otherwise.
defaultExtensions :: [Extension]
defaultExtensions = [ImplicitPrelude, MonoLocalBinds, MonomorphismRestriction]

-- | The extension set of a module that brings nothing but its own pragmas:
-- the defaults with the module's @LANGUAGE@ pragmas applied in source order,
-- where an enabled extension brings its implied extensions with it at once.
-- A later @NoMonoLocalBinds@ then turns off the @MonoLocalBinds@ that an
-- earlier @TypeFamilies@ implied, like in GHC.
--
-- A package build does not use this. It has a cabal file and a language
-- edition to fold in as well, and computes its extension set from all three.
modulePragmaExtensions :: Module -> [Extension]
modulePragmaExtensions = foldl step defaultExtensions . moduleLanguagePragmas
  where
    step extensions setting =
      case setting of
        EnableExtension _ -> applyImpliedExtensions (applyExtensionSetting setting extensions)
        DisableExtension _ -> applyExtensionSetting setting extensions
