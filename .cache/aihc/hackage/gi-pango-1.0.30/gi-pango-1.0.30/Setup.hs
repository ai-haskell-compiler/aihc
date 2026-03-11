{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject
import qualified GI.Gio.Config as Gio
import qualified GI.HarfBuzz.Config as HarfBuzz


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "Pango"
        version = "1.0"
        pkgName = "gi-pango"
        pkgVersion = "1.0.30"
        overridesFile = Just "Pango.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:Gio" Gio.overrides, TaggedOverride "inherited:HarfBuzz" HarfBuzz.overrides]
