{-# LANGUAGE FlexibleInstances #-}

{- | Module for pretty-printing epub meta-information

   Defined here is the Formattable typeclass and instances for the
   top-level epub data structures. The intention is that consumers
   use the format function on any of these otherwise disparate types.
-}
module Codec.Epub.Format
   ( Formattable (..)
   )
   where

import Codec.Epub.Data.Guide
import Codec.Epub.Data.Manifest
import Codec.Epub.Data.Metadata
import Codec.Epub.Data.Package
import Codec.Epub.Data.Spine
import Codec.Epub.Format.Guide
import Codec.Epub.Format.Manifest
import Codec.Epub.Format.Metadata
import Codec.Epub.Format.Package
import Codec.Epub.Format.Spine


class Formattable a where
   format :: a -> String


instance Formattable [GuideRef] where
   format = formatGuide

instance Formattable Manifest where
   format = formatManifest

instance Formattable Metadata where
   format = formatMetadata

instance Formattable Package where
   format = formatPackage

instance Formattable Spine where
   format = formatSpine
