{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.Appendful.Collection
import Data.GenValidity.Appendful ()
import Data.GenValidity.Criterion

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @ClientId,
      genValidBench @(ClientStore ClientId Int Bool),
      genValidBench @(SyncRequest ClientId Int Bool),
      genValidBench @(SyncResponse ClientId Int Bool),
      genValidBench @(ServerStore Int Bool)
    ]
