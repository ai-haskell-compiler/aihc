{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.Import.NexusSpec
-- Description :  Test import of trees in Nexus files
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 18:08:14 2020.
module ELynx.Tree.Import.NexusSpec
  ( spec,
  )
where

import ELynx.Tools.InputOutput
import ELynx.Tree
import Test.Hspec

file :: FilePath
file = "data/SimpleTree.nex"

noPL :: Phylo
noPL = Phylo Nothing Nothing

res :: Tree Phylo Name
res =
  Node
    noPL
    ""
    [ Node
        noPL
        ""
        [ Node noPL "A" [],
          Node noPL "B" []
        ],
      Node noPL "C" []
    ]

spec :: Spec
spec = describe "trees" $
  it "parses a nexus file with a TREES block" $
    do
      ts <- parseFileWith (nexusTrees Standard) file
      head ts `shouldBe` ("tree1", res)
