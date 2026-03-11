{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject
import qualified GI.Gio.Config as Gio


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "Soup"
        version = "3.0"
        pkgName = "gi-soup3"
        pkgVersion = "3.0.5"
        overridesFile = Just "Soup.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:Gio" Gio.overrides]
