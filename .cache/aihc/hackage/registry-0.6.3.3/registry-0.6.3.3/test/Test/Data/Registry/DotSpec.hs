{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.DotSpec where

import Data.Registry
import Data.Text as T
import Protolude
import Test.Data.Registry.Make.SpecializationSpec
import Test.Tasty.Extensions

test_dot =
  prop "a dot graph can be generated from a registry" $ do
    let dot = makeDot @App appRegistry

    annotate "the graph does not contain redundant edges"
    annotate "the graph does not contain redundant edges"
    unDot dot
      === T.unlines
        [ "strict digraph {",
          "  node [shape=record]",
          "\"Test.Data.Registry.Make.SpecializationSpec.App\" -> \"Test.Data.Registry.Make.SpecializationSpec.Sql-1\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.App\" -> \"Test.Data.Registry.Make.SpecializationSpec.TwitterClient\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.App\" -> \"Test.Data.Registry.Make.SpecializationSpec.Supervisor-1\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.App\" -> \"Test.Data.Registry.Make.SpecializationSpec.StatsStore\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.StatsStore\" -> \"Test.Data.Registry.Make.SpecializationSpec.TwitterClient\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.StatsStore\" -> \"Test.Data.Registry.Make.SpecializationSpec.Sql-2\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.StatsStore\" -> \"Test.Data.Registry.Make.SpecializationSpec.Supervisor-1\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.Supervisor-1\" -> \"Test.Data.Registry.Make.SpecializationSpec.SupervisorConfig-1\\nSupervisorConfig default\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.Sql-2\" -> \"Test.Data.Registry.Make.SpecializationSpec.Supervisor-2\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.Supervisor-2\" -> \"Test.Data.Registry.Make.SpecializationSpec.SupervisorConfig-2\\nSupervisorConfig for sql under the stats store\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.TwitterClient\" -> \"Test.Data.Registry.Make.SpecializationSpec.Supervisor-3\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.Supervisor-3\" -> \"Test.Data.Registry.Make.SpecializationSpec.SupervisorConfig-3\\nSupervisorConfig for the twitter client\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.Supervisor-1\" -> \"Test.Data.Registry.Make.SpecializationSpec.SupervisorConfig-1\\nSupervisorConfig default\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.TwitterClient\" -> \"Test.Data.Registry.Make.SpecializationSpec.Supervisor-3\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.Supervisor-3\" -> \"Test.Data.Registry.Make.SpecializationSpec.SupervisorConfig-3\\nSupervisorConfig for the twitter client\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.Sql-1\" -> \"Test.Data.Registry.Make.SpecializationSpec.Supervisor-4\";",
          "\"Test.Data.Registry.Make.SpecializationSpec.Supervisor-4\" -> \"Test.Data.Registry.Make.SpecializationSpec.SupervisorConfig-4\\nSupervisorConfig for sql in general\";",
          "}"
        ]
