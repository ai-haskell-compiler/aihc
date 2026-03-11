{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject
import qualified GI.Gdk.Config as Gdk
import qualified GI.Graphene.Config as Graphene
import qualified GI.Pango.Config as Pango
import qualified GI.Cairo.Config as Cairo


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "Gsk"
        version = "4.0"
        pkgName = "gi-gsk"
        pkgVersion = "4.0.9"
        overridesFile = Just "Gsk.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:Gdk" Gdk.overrides, TaggedOverride "inherited:Graphene" Graphene.overrides, TaggedOverride "inherited:Pango" Pango.overrides, TaggedOverride "inherited:Cairo" Cairo.overrides]
