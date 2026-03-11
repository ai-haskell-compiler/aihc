{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GLib.Config as GLib
import qualified GI.GModule.Config as GModule
import qualified GI.GObject.Config as GObject
import qualified GI.Gio.Config as Gio


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "GdkPixbuf"
        version = "2.0"
        pkgName = "gi-gdkpixbuf"
        pkgVersion = "2.0.32"
        overridesFile = Just "GdkPixbuf.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GModule" GModule.overrides, TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:Gio" Gio.overrides]
