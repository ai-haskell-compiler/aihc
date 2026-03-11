{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.Atk.Config as Atk
import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject
import qualified GI.Gdk.Config as Gdk
import qualified GI.GdkPixbuf.Config as GdkPixbuf
import qualified GI.Gio.Config as Gio
import qualified GI.Gtk.Config as Gtk
import qualified GI.Pango.Config as Pango
import qualified GI.Cairo.Config as Cairo


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "Vte"
        version = "2.91"
        pkgName = "gi-vte"
        pkgVersion = "2.91.35"
        overridesFile = Just "Vte.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:Atk" Atk.overrides, TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:Gdk" Gdk.overrides, TaggedOverride "inherited:GdkPixbuf" GdkPixbuf.overrides, TaggedOverride "inherited:Gio" Gio.overrides, TaggedOverride "inherited:Gtk" Gtk.overrides, TaggedOverride "inherited:Pango" Pango.overrides, TaggedOverride "inherited:Cairo" Cairo.overrides]
