{-# LANGUAGE FlexibleContexts #-}

{- | Module for pretty-printing epub manifest info

   Used internally by Codec.Epub.Format
-}
module Codec.Epub.Format.Manifest
   ( formatManifest
   )
   where

import Control.Monad.Writer.Lazy ( MonadWriter, execWriter )
import Data.Foldable ( toList )
import Text.Printf ( printf )

import Codec.Epub.Format.Util
import Codec.Epub.Data.Manifest


tellManifestItem :: MonadWriter (Seq Char) m => ManifestItem -> m ()
tellManifestItem (ManifestItem mfId href mediaType) =
   tellSeq $ printf "   id: %s, href: %s, media-type: %s\n"
      mfId href mediaType


tellManifest :: MonadWriter (Seq Char) m => [ManifestItem] -> m ()
tellManifest mas = do
   tellSeq "manifest items:\n"
   mapM_ tellManifestItem mas


{- | Format an epub Manifest structure for pretty printing
-}
formatManifest :: Manifest -> String
formatManifest (Manifest mis) = toList . execWriter
   $ tellManifest mis
