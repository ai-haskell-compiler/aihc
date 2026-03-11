{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Data.GenValidity.UUID.Typed ()
import Data.UUID.Typed

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @(UUID Int)
    ]
