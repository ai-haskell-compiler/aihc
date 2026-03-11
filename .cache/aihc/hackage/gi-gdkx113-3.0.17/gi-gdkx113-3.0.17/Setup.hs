{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GObject.Config as GObject
import qualified GI.Gdk.Config as Gdk
import qualified GI.Gio.Config as Gio
import qualified GI.Cairo.Config as Cairo
import qualified GI.Xlib.Config as Xlib


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "GdkX11"
        version = "3.0"
        pkgName = "gi-gdkx113"
        pkgVersion = "3.0.17"
        overridesFile = Just "GdkX11.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:Gdk" Gdk.overrides, TaggedOverride "inherited:Gio" Gio.overrides, TaggedOverride "inherited:Cairo" Cairo.overrides, TaggedOverride "inherited:Xlib" Xlib.overrides]
