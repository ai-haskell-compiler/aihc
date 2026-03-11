{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "JavaScriptCore"
        version = "6.0"
        pkgName = "gi-javascriptcore6"
        pkgVersion = "6.0.5"
        overridesFile = Nothing
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GObject" GObject.overrides]
