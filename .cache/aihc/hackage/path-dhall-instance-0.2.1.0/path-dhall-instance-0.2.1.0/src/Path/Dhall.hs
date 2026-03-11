{- |
   Module     : Path.Dhall
   License    : MIT
   Stability  : experimental

Dhall instance for `Path`.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances  #-}
module Path.Dhall where

import Dhall
import qualified Data.Text as T
import Path
import Data.Either.Validation

pathDecoder f opts =
      Decoder
            { extract = extractPath
            , expected = expectedPath
            }
      where
        filePathDecoder :: Decoder FilePath
        filePathDecoder = autoWith opts

        extractPath expression =
          case extract filePathDecoder expression of
              Success x -> case f x of
                Left exception   -> Dhall.extractError (T.pack $ show exception)
                Right path       -> Success path
              Failure e        -> Failure e

        expectedPath = expected filePathDecoder

instance FromDhall (Path Rel Dir) where
    autoWith options = pathDecoder parseRelDir options

instance FromDhall (Path Rel File) where
    autoWith options = pathDecoder parseRelFile options

instance FromDhall (Path Abs Dir) where
    autoWith options = pathDecoder parseAbsDir options

instance FromDhall (Path Abs File) where
    autoWith options = pathDecoder parseAbsFile options

instance ToDhall (Path Rel Dir)
instance ToDhall (Path Rel File)
instance ToDhall (Path Abs Dir)
instance ToDhall (Path Abs File)
