{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.PathPieces () where

import Control.Error.Util (hush)
import Data.BCP47 (BCP47, fromText, toText)
import Web.PathPieces (PathPiece (..))

instance PathPiece BCP47 where
  fromPathPiece = hush . fromText
  toPathPiece = toText
