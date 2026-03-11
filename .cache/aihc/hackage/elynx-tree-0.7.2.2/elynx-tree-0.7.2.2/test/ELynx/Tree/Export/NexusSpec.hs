{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.Export.NexusSpec
-- Description :  Test export of trees in Nexus files
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 18:08:14 2020.
module ELynx.Tree.Export.NexusSpec
  ( spec,
  )
where

import ELynx.Tools.InputOutput
import ELynx.Tree
import Test.Hspec

tree :: Tree Phylo Name
tree =
  Node
    (Phylo Nothing Nothing)
    ""
    [ Node
        (Phylo Nothing Nothing)
        ""
        [ Node (Phylo Nothing Nothing) "A" [],
          Node (Phylo Nothing Nothing) "B" []
        ],
      Node (Phylo Nothing Nothing) "C" []
    ]

spec :: Spec
spec = describe "toNexusTrees" $
  it "exports a nexus file with a TREES block" $
    do
      let ts = parseByteStringWith (nexusTrees Standard) (toNexusTrees [("tree1", tree)])
      head ts `shouldBe` ("tree1", tree)
