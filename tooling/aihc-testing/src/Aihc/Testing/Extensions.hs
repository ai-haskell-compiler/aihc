-- |
-- Module      : Aihc.Testing.Extensions
-- Description : The language extensions a fixture module compiles under
--
-- The compiler proper never derives extensions from a module: whoever reads
-- the source folds the language edition, the package's default extensions
-- and the module's @LANGUAGE@ pragmas into one set, and name resolution and
-- the type checker take that set as data.
--
-- A fixture has no cabal file, so a test harness stands in for that reader:
-- it picks a language edition and applies the module's own pragmas to it.
module Aihc.Testing.Extensions
  ( fixtureExtensions,
  )
where

import Aihc.Parser.Syntax
  ( Extension,
    ExtensionSetting (..),
    LanguageEdition,
    Module (..),
    applyExtensionSetting,
    applyImpliedExtensions,
    languageEditionExtensions,
  )

-- | The extensions of a fixture module: one language edition with the
-- module's own pragmas applied to it.
--
-- The pragmas apply in source order, so a later pragma wins, and an enabled
-- extension brings its implied extensions with it at once. A later
-- @NoMonoLocalBinds@ then turns off the @MonoLocalBinds@ that an earlier
-- @TypeFamilies@ implied, like in GHC. 'Aihc.PackagePlan.Source' folds a
-- package's pragmas the same way, for the same reason: the parser's own
-- 'Aihc.Parser.Syntax.effectiveExtensions' folds the other way and applies
-- implied extensions once at the end,
-- <https://github.com/ai-haskell-compiler/aihc-parser/issues/29>.
fixtureExtensions :: LanguageEdition -> Module -> [Extension]
fixtureExtensions edition =
  foldl applyOne (languageEditionExtensions edition) . moduleLanguagePragmas
  where
    applyOne extensions setting =
      case setting of
        EnableExtension _ -> applyImpliedExtensions (applyExtensionSetting setting extensions)
        DisableExtension _ -> applyExtensionSetting setting extensions
