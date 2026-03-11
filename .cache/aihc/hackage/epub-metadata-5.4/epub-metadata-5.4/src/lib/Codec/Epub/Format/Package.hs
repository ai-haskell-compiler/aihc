{-# LANGUAGE FlexibleContexts #-}

{- | Module for pretty-printing epub package info

   Used internally by Codec.Epub.Format
-}
module Codec.Epub.Format.Package
   ( formatPackage
   )
   where

import Control.Monad.Writer.Lazy ( MonadWriter, execWriter )
import Data.Foldable ( toList )

import Codec.Epub.Data.Package
import Codec.Epub.Format.Util


tellPackage :: MonadWriter (Seq Char) m => (String, String) -> m ()
tellPackage (version, uniqueId) = do
   tellSeq "package\n"
   tellSeq $ formatSubline "version" (Just version)
   tellSeq $ formatSubline "unique-identifier" (Just uniqueId)


{- | Format an epub Package structure for pretty printing
-}
formatPackage :: Package -> String
formatPackage (Package v u) = toList . execWriter
   $ tellPackage (v, u)
