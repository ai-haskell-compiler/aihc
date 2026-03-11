{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Data.GenValidity.Mergeful ()
import Data.Mergeful.Collection
import Data.Mergeful.Item
import Data.Mergeful.Value

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup "Utils" [genValidBench @ServerTime, genValidBench @(Timed Bool)],
      bgroup
        "Value"
        [ genValidBench @ChangedFlag,
          genValidBench @(ValueMergeResult Bool),
          genValidBench @(ClientValue Bool),
          genValidBench @(ValueSyncRequest Bool),
          genValidBench @(ValueSyncResponse Bool),
          genValidBench @(ServerValue Bool)
        ],
      bgroup
        "Item"
        [ genValidBench @(ItemMergeResult Bool),
          genValidBench @(ClientItem Bool),
          genValidBench @(ItemSyncRequest Bool),
          genValidBench @(ItemSyncResponse Bool),
          genValidBench @(ServerItem Bool)
        ],
      bgroup
        "Collection"
        [ genValidBench @ClientId,
          genValidBench @(ClientStore ClientId Int Bool),
          genValidBench @(SyncRequest ClientId Int Bool),
          genValidBench @(ClientAddition Int),
          genValidBench @(SyncResponse ClientId Int Bool),
          genValidBench @(ServerStore Int Bool)
        ]
    ]
