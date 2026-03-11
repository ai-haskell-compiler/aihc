{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.Atk.Config as Atk
import qualified GI.Dbusmenu.Config as Dbusmenu
import qualified GI.GObject.Config as GObject
import qualified GI.Gdk.Config as Gdk
import qualified GI.GdkPixbuf.Config as GdkPixbuf
import qualified GI.Gtk.Config as Gtk


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "DbusmenuGtk3"
        version = "0.4"
        pkgName = "gi-dbusmenugtk3"
        pkgVersion = "0.4.16"
        overridesFile = Just "DbusmenuGtk3.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:Atk" Atk.overrides, TaggedOverride "inherited:Dbusmenu" Dbusmenu.overrides, TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:Gdk" Gdk.overrides, TaggedOverride "inherited:GdkPixbuf" GdkPixbuf.overrides, TaggedOverride "inherited:Gtk" Gtk.overrides]
