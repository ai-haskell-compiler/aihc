{-# language OverloadedStrings #-}

module Main (main) where

import Dot

main :: IO ()
main = do
  putStrLn $ "dumping example dotgraph to " ++ target
  encodeToFile target example
 
target :: FilePath
target = "example/example.dot"

example :: DotGraph
example = DotGraph Strict Directed (Just "foobar")
  [ StatementNode $ NodeStatement "a1"
    [ Attribute "color" "blue"
    , Attribute "shape" "box"
    ]
  , StatementNode $ NodeStatement "a2" []
  , StatementEdge $ EdgeStatement (ListTwo "a1" "a2" ["a3"])
      [ Attribute "color" "red"
      ]
  ]
