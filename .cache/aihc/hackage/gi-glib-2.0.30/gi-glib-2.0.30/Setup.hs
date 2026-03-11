{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))



main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "GLib"
        version = "2.0"
        pkgName = "gi-glib"
        pkgVersion = "2.0.30"
        overridesFile = Just "GLib.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = []
