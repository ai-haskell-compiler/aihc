{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject
import qualified GI.GdkPixbuf.Config as GdkPixbuf
import qualified GI.Gio.Config as Gio
import qualified GI.Pango.Config as Pango
import qualified GI.Cairo.Config as Cairo


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "Gdk"
        version = "3.0"
        pkgName = "gi-gdk3"
        pkgVersion = "3.0.30"
        overridesFile = Just "Gdk.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:GdkPixbuf" GdkPixbuf.overrides, TaggedOverride "inherited:Gio" Gio.overrides, TaggedOverride "inherited:Pango" Pango.overrides, TaggedOverride "inherited:Cairo" Cairo.overrides]
