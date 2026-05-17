{-# LANGUAGE OverloadedStrings #-}

module Test.GoldenUpdate
  ( goldenUpdateTests,
  )
where

import Aihc.Dev.Golden.Update (replaceYamlValueAt)
import Data.Aeson (Value (..), object, (.=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

goldenUpdateTests :: TestTree
goldenUpdateTests =
  testGroup
    "golden-update"
    [ testCase "replaces nested YAML values without dropping siblings" $ do
        let input =
              object
                [ "formatters"
                    .= object
                      [ "aihc-fmt"
                          .= object
                            [ "status" .= String "pass",
                              "output" .= String "old"
                            ],
                        "ormolu"
                          .= object
                            [ "status" .= String "pass",
                              "output" .= String "kept"
                            ]
                      ]
                ]
            expected =
              object
                [ "formatters"
                    .= object
                      [ "aihc-fmt"
                          .= object
                            [ "status" .= String "pass",
                              "output" .= String "new"
                            ],
                        "ormolu"
                          .= object
                            [ "status" .= String "pass",
                              "output" .= String "kept"
                            ]
                      ]
                ]
        assertEqual
          "updated object"
          (Right expected)
          (replaceYamlValueAt ["formatters", "aihc-fmt", "output"] (String "new") input),
      testCase "rejects missing YAML paths" $ do
        let input = object ["output" .= String "old"]
        assertBool
          "missing path is rejected"
          ( case replaceYamlValueAt ["formatters", "aihc-fmt", "output"] (String "new") input of
              Left _ -> True
              Right _ -> False
          )
    ]
