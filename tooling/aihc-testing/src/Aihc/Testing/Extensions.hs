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
    LanguageEdition,
    Module (..),
    effectiveExtensions,
  )

-- | The extensions of a fixture module: one language edition with the
-- module's own pragmas applied to it.
fixtureExtensions :: LanguageEdition -> Module -> [Extension]
fixtureExtensions edition = effectiveExtensions edition . moduleLanguagePragmas
