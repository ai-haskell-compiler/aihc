{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise4 where

import Data.Registry
import Protolude
import Test.Tutorial.Application
import Test.Tutorial.Exercise2
import Test.Tutorial.Exercise3

printApp :: IO ()
printApp =
  putStrLn $
    unDot $
      makeDot @App $
        specialize @(Rng IO) (fun silentLogger) $
          val (SecretReaderConfig "missing") <: registry
