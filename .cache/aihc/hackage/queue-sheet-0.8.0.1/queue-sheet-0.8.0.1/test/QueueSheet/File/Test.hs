------------------------------------------------------------------------------
-- |
-- Module      : QueueSheet.File.Test
-- Description : queues file tests
-- Copyright   : Copyright (c) 2020-2025 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module QueueSheet.File.Test (tests) where

-- https://hackage.haskell.org/package/base
import Control.Monad (unless)
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (queue-sheet)
import QueueSheet.File (loadYaml')
import QueueSheet.Types
  ( Date(Date), Item(Item, itemName, itemTags, itemUrl), Name(Name)
  , Queue
      ( Queue, queueDate, queueItems, queueName, queueSection, queueTags
      , queueUrl
      )
  , QueueSheet(QueueSheet, qsQueues, qsSections)
  , Section(Section), Tag(Tag), Url(Url), defaultSection
  )

------------------------------------------------------------------------------

defaultItem :: Item
defaultItem = Item
    { itemName = Name ""
    , itemUrl  = Nothing
    , itemTags = []
    }

defaultQueue :: Queue
defaultQueue = Queue
    { queueName    = Name ""
    , queueUrl     = Nothing
    , queueSection = defaultSection
    , queueTags    = []
    , queueDate    = Nothing
    , queueItems   = Nothing
    }

defaultQueueSheet :: QueueSheet
defaultQueueSheet = QueueSheet
    { qsSections = [defaultSection]
    , qsQueues   = []
    }

------------------------------------------------------------------------------

loadYaml
  :: [(FilePath, Either String ByteString)]
  -> Either String QueueSheet
loadYaml files
    = runIdentity
    . loadYaml' loadFile
    . fromMaybe "/tmp/unknown.yaml"
    . listToMaybe
    . map fst
    $ take 1 files
  where
    loadFile :: FilePath -> Identity (Either String ByteString)
    loadFile path = pure .
      fromMaybe (Left ("file not found: " ++ path)) $ lookup path files

validFile
  :: FilePath
  -> [ByteString]
  -> (FilePath, Either String ByteString)
validFile path lines' = (path, Right (BS8.unlines $ "---" : "" : lines'))

invalidFile
  :: FilePath
  -> (FilePath, Either String ByteString)
invalidFile path = (path, Left ("error loading file: " ++ path))

------------------------------------------------------------------------------
-- $Queue

testQueueName :: TestTree
testQueueName = testGroup "name"
    [ testCase "string" $ do
        let expected = defaultQueueSheet
              { qsQueues = [defaultQueue{ queueName = Name "test" }]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              ]
          ]
    , testCase "number" $ do
        let expected = defaultQueueSheet
              { qsQueues = [defaultQueue{ queueName = Name "42" }]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: 42"
              ]
          ]
    , testCase "bool" $ do
        let expected = defaultQueueSheet
              { qsQueues = [defaultQueue{ queueName = Name "true" }]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: true"
              ]
          ]
    , testCase "null" $ do
        let expected = defaultQueueSheet
              { qsQueues = [defaultQueue{ queueName = Name "null" }]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: null"
              ]
          ]
    , testCase "empty" $ do
        let message = intercalate "\n"
              [ "error loading /tmp/test.yaml: Aeson exception:"
              , "Error in $[0].name: empty string"
              ]
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: \"\""
              ]
          ]
    , testCase "array" $ do
        let message = intercalate "\n"
              [ "error loading /tmp/test.yaml: Aeson exception:"
              , "Error in $[0].name: unexpected array"
              ]
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name:"
              , "  - invalid"
              ]
          ]
    , testCase "object" $ do
        let message = intercalate "\n"
              [ "error loading /tmp/test.yaml: Aeson exception:"
              , "Error in $[0].name: unexpected object"
              ]
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name:"
              , "    invalid: true"
              ]
          ]
    ]

testQueueUrl :: TestTree
testQueueUrl = testGroup "url"
    [ testCase "string" $ do
        let expected = defaultQueueSheet
              { qsQueues =
                  [ defaultQueue
                      { queueName = Name "test"
                      , queueUrl  = Just $ Url "https://www.example.com:8080/"
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  url: https://www.example.com:8080/"
              ]
          ]
    , testCase "empty" $ do
        let message = intercalate "\n"
              [ "error loading /tmp/test.yaml: Aeson exception:"
              , "Error in $[0].url: empty string"
              ]
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  url: \"\""
              ]
          ]
    ]

testQueueSection :: TestTree
testQueueSection = testGroup "section"
    [ testCase "known" $ do
        let generalSection = Section "general"
            expected = QueueSheet
              { qsSections = [defaultSection, generalSection]
              , qsQueues =
                  [ defaultQueue
                      { queueName = Name "test"
                      , queueSection = generalSection
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "sections:"
              , "  - general"
              , ""
              , "queues:"
              , "  - name: test"
              , "    section: general"
              ]
          ]
    , testCase "multiple" $ do
        let generalSection = Section "general"
            expected = QueueSheet
              { qsSections = [defaultSection, generalSection]
              , qsQueues =
                  [ defaultQueue
                      { queueName = Name "one"
                      , queueSection = defaultSection
                      }
                  , defaultQueue
                      { queueName = Name "two"
                      , queueSection = generalSection
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "sections:"
              , "  - general"
              , ""
              , "queues:"
              , "  - name: one"
              , "  - name: two"
              , "    section: general"
              ]
          ]
    , testCase "none" $ do
        let expected = QueueSheet
              { qsSections = [defaultSection]
              , qsQueues =
                  [ defaultQueue
                      { queueName = Name "one"
                      , queueSection = defaultSection
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "sections:"
              , ""
              , "queues:"
              , "  - name: one"
              ]
          ]
    , testCase "unknown" $ do
        let message = unwords
              [ "error loading /tmp/test.yaml:"
              , "queue test has unknown section general"
              ]
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  section: general"
              ]
          ]
    , testCase "empty" $ do
        let message = intercalate "\n"
              [ "error loading /tmp/test.yaml: Aeson exception:"
              , "Error in $[0].section: empty string"
              ]
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  section: \"\""
              ]
          ]
    ]

testQueueTag :: TestTree
testQueueTag = testGroup "tag"
    [ testGroup "array"
        [ testCase "valid" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName = Name "test"
                          , queueTags = [Tag "partial"]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  tags:"
                  , "    - partial"
                  ]
              ]
        , testCase "multiple" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName = Name "test"
                          , queueTags = [Tag "complete", Tag "partial"]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  tags:"
                  , "    - complete"
                  , "    - partial"
                  ]
              ]
        , testCase "empty" $ do
            let message = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: empty string"
                  ]
            Left message @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  tags:"
                  , "    - \"\""
                  ]
              ]
        , testCase "invalid" $ do
            let message = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: invalid tag: (invalid)"
                  ]
            Left message @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  tags:"
                  , "    - (invalid)"
                  ]
              ]
        , testCase "number" $ do
            let messageOld = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: expected Tag, encountered Number"
                  ]
                message = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: parsing Tag failed, " ++
                    "expected String, but encountered Number"
                  ]
                eeq = loadYaml
                  [ validFile "/tmp/test.yaml"
                      [ "- name: test"
                      , "  tags:"
                      , "    - 13"
                      ]
                  ]
            unless (Left messageOld == eeq) $ Left message @=? eeq
        ]
    , testGroup "ssv"
        [ testCase "single" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName = Name "test"
                          , queueTags = [Tag "partial"]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  tags: partial"
                  ]
              ]
        , testCase "multiple" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName = Name "test"
                          , queueTags = [Tag "complete", Tag "partial"]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  tags: complete partial"
                  ]
              ]
        , testCase "folded" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName = Name "test"
                          , queueTags = [Tag "complete", Tag "partial"]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  tags: >"
                  , "    complete"
                  , "    partial"
                  ]
              ]
        , testCase "empty" $ do
            let message = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: empty string"
                  ]
            Left message @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  tags: \"\""
                  ]
              ]
        , testCase "invalid" $ do
            let message = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: invalid tag: (invalid)"
                  ]
            Left message @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  tags: (invalid)"
                  ]
              ]
        , testCase "number" $ do
            let messageOld = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: expected Tag, encountered Number"
                  ]
                message = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: parsing Tag failed, " ++
                    "expected String, but encountered Number"
                  ]
                eeq = loadYaml
                  [ validFile "/tmp/test.yaml"
                      [ "- name: test"
                      , "  tags: 13"
                      ]
                  ]
            unless (Left messageOld == eeq) $ Left message @=? eeq
        ]
    ]

testQueueDate :: TestTree
testQueueDate = testGroup "date"
    [ testCase "string" $ do
        let expected = defaultQueueSheet
              { qsQueues =
                  [ defaultQueue
                      { queueName = Name "test"
                      , queueDate = Just $ Date "2020-08-14"
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  date: 2020-08-14"
              ]
          ]
    , testCase "empty" $ do
        let message = intercalate "\n"
              [ "error loading /tmp/test.yaml: Aeson exception:"
              , "Error in $[0].date: empty string"
              ]
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  date: \"\""
              ]
          ]
    ]

testQueuePrev :: TestTree
testQueuePrev = testGroup "prev"
    [ testCase "string" $ do
        let expected = defaultQueueSheet
              { qsQueues =
                  [ defaultQueue
                      { queueName  = Name "test"
                      , queueItems = Just . Left $ defaultItem
                          { itemName = Name "premiere"
                          }
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  prev: premiere"
              ]
          ]
    , testCase "number" $ do
        let expected = defaultQueueSheet
              { qsQueues =
                  [ defaultQueue
                      { queueName  = Name "test"
                      , queueItems = Just . Left $ defaultItem
                          { itemName = Name "42"
                          }
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  prev: 42"
              ]
          ]
    , testCase "object" $ do
        let expected = defaultQueueSheet
              { qsQueues =
                  [ defaultQueue
                      { queueName  = Name "test"
                      , queueItems = Just . Left $ defaultItem
                          { itemName = Name "premiere"
                          }
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  prev:"
              , "    name: premiere"
              ]
          ]
    , testCase "url" $ do
        let expected = defaultQueueSheet
              { qsQueues =
                  [ defaultQueue
                      { queueName  = Name "test"
                      , queueItems = Just . Left $ defaultItem
                          { itemName = Name "42"
                          , itemUrl  = Just $ Url "https://www.example.com/42"
                          }
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  prev:"
              , "    name: 42"
              , "    url: https://www.example.com/42"
              ]
          ]
    , testGroup "tags"
        [ testCase "array" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just . Left $ defaultItem
                              { itemName = Name "42"
                              , itemTags = [Tag "one", Tag "two"]
                              }
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  prev:"
                  , "    name: 42"
                  , "    tags:"
                  , "      - one"
                  , "      - two"
                  ]
              ]
        , testCase "ssv" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just . Left $ defaultItem
                              { itemName = Name "42"
                              , itemTags = [Tag "one", Tag "two"]
                              }
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  prev:"
                  , "    name: 42"
                  , "    tags: one two"
                  ]
              ]
        ]
    , testCase "empty" $ do
        let message = intercalate "\n"
              [ "error loading /tmp/test.yaml: Aeson exception:"
              , "Error in $[0].prev.name: empty string"
              ]
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: test"
              , "  prev:"
              , "    name: \"\""
              ]
          ]
    ]

testQueueNext :: TestTree
testQueueNext = testGroup "next"
    [ testGroup "array"
        [ testCase "string" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "one"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next:"
                  , "    - one"
                  ]
              ]
        , testCase "number" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "42"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next:"
                  , "    - 42"
                  ]
              ]
        , testCase "object" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "one"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next:"
                  , "    - name: one"
                  ]
              ]
        , testCase "url" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "one"
                                  , itemUrl  = Just $
                                      Url "https://www.example.com/one"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next:"
                  , "    - name: one"
                  , "      url: https://www.example.com/one"
                  ]
              ]
        , testGroup "tags"
            [ testCase "array" $ do
                let expected = defaultQueueSheet
                      { qsQueues =
                          [ defaultQueue
                              { queueName  = Name "test"
                              , queueItems = Just $ Right
                                  [ defaultItem
                                      { itemName = Name "one"
                                      , itemTags = [Tag "one", Tag "two"]
                                      }
                                  ]
                              }
                          ]
                      }
                Right expected @=? loadYaml
                  [ validFile "/tmp/test.yaml"
                      [ "- name: test"
                      , "  next:"
                      , "    - name: one"
                      , "      tags:"
                      , "        - one"
                      , "        - two"
                      ]
                  ]
            , testCase "ssv" $ do
                let expected = defaultQueueSheet
                      { qsQueues =
                          [ defaultQueue
                              { queueName  = Name "test"
                              , queueItems = Just $ Right
                                  [ defaultItem
                                      { itemName = Name "one"
                                      , itemTags = [Tag "one", Tag "two"]
                                      }
                                  ]
                              }
                          ]
                      }
                Right expected @=? loadYaml
                  [ validFile "/tmp/test.yaml"
                      [ "- name: test"
                      , "  next:"
                      , "    - name: one"
                      , "      tags: one two"
                      ]
                  ]
            ]
        , testCase "multiple" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "one"
                                  }
                              , defaultItem
                                  { itemName = Name "42"
                                  , itemUrl  = Just $
                                      Url "https://www.example.com/42"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next:"
                  , "    - one"
                  , "    - name: 42"
                  , "      url: https://www.example.com/42"
                  ]
              ]
        , testCase "none" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Nothing
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next:"
                  ]
              ]
        , testCase "prev" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "42"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  prev: 41"
                  , "  next:"
                  , "    - 42"
                  ]
              ]
        , testCase "empty" $ do
            let message = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: empty string"
                  ]
            Left message @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next:"
                  , "    - \"\""
                  ]
              ]
        ]
    , testGroup "ssv"
        [ testCase "single" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "one"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next: one"
                  ]
              ]
        , testCase "number" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "42"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next: 42"
                  ]
              ]
        , testCase "multiple" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "11"
                                  }
                              , defaultItem
                                  { itemName = Name "42"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next: 11 42"
                  ]
              ]
        , testCase "folded" $ do
            let expected = defaultQueueSheet
                  { qsQueues =
                      [ defaultQueue
                          { queueName  = Name "test"
                          , queueItems = Just $ Right
                              [ defaultItem
                                  { itemName = Name "11"
                                  }
                              , defaultItem
                                  { itemName = Name "42"
                                  }
                              ]
                          }
                      ]
                  }
            Right expected @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next: >"
                  , "    11"
                  , "    42"
                  ]
              ]
        , testCase "empty" $ do
            let message = intercalate "\n"
                  [ "error loading /tmp/test.yaml: Aeson exception:"
                  , "Error in $[0]: empty string"
                  ]
            Left message @=? loadYaml
              [ validFile "/tmp/test.yaml"
                  [ "- name: test"
                  , "  next: \"\""
                  ]
              ]
        ]
    ]

------------------------------------------------------------------------------
-- $Import

testImportSection :: TestTree
testImportSection = testGroup "section"
    [ testCase "existing" $ do
        let generalSection = Section "general"
            extraSection = Section "extra"
            expected = defaultQueueSheet
              { qsSections = [defaultSection, generalSection, extraSection]
              , qsQueues =
                  [ defaultQueue
                      { queueName    = Name "one"
                      , queueSection = generalSection
                      }
                  , defaultQueue
                      { queueName    = Name "two"
                      , queueSection = extraSection
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "sections:"
              , "  - general"
              , "  - extra"
              , ""
              , "queues:"
              , "  - name: one"
              , "    section: general"
              , "  - import: extra.yaml"
              , "    section: extra"
              ]
          , validFile "/tmp/extra.yaml"
              [ "sections:"
              , "  - general"
              , ""
              , "queues:"
              , "  - name: two"
              , "    section: general"
              ]
          ]
    , testCase "unknown" $ do
        let message =
              "error loading /tmp/test.yaml: " ++
              "import extra.yaml has unknown section extra"
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: one"
              , "- import: extra.yaml"
              , "  section: extra"
              ]
          , validFile "/tmp/extra.yaml"
              [ "- name: two"
              ]
          ]
    ]

testImportInherit :: TestTree
testImportInherit = testGroup "inherit"
    [ testCase "default" $ do
        let expected = defaultQueueSheet
              { qsQueues =
                  [ defaultQueue
                      { queueName = Name "one"
                      }
                  , defaultQueue
                      { queueName = Name "two"
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: one"
              , "- import: extra.yaml"
              ]
          , validFile "/tmp/extra.yaml"
              [ "- name: two"
              ]
          ]
    , testCase "existing" $ do
        let generalSection = Section "general"
            expected = defaultQueueSheet
              { qsSections = [defaultSection, generalSection]
              , qsQueues =
                  [ defaultQueue
                      { queueName    = Name "one"
                      , queueSection = generalSection
                      }
                  , defaultQueue
                      { queueName    = Name "two"
                      , queueSection = generalSection
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "sections:"
              , "  - general"
              , ""
              , "queues:"
              , "  - name: one"
              , "    section: general"
              , "  - import: extra.yaml"
              ]
          , validFile "/tmp/extra.yaml"
              [ "sections:"
              , "  - general"
              , ""
              , "queues:"
              , "  - name: two"
              , "    section: general"
              ]
          ]
    , testCase "unknown" $ do
        let message =
              "error loading /tmp/test.yaml: " ++
              "queue two imported from extra.yaml has unknown section general"
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: one"
              , "- import: extra.yaml"
              ]
          , validFile "/tmp/extra.yaml"
              [ "sections:"
              , "  - general"
              , ""
              , "queues:"
              , "  - name: two"
              , "    section: general"
              ]
          ]
    ]

testImportPath :: TestTree
testImportPath = testGroup "path"
    [ testCase "absolute" $ do
        let expected = defaultQueueSheet
              { qsQueues =
                  [ defaultQueue
                      { queueName = Name "one"
                      }
                  , defaultQueue
                      { queueName = Name "two"
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: one"
              , "- import: /home/test/extra.yaml"
              ]
          , validFile "/home/test/extra.yaml"
              [ "- name: two"
              ]
          ]
    , testCase "relative" $ do
        let expected = defaultQueueSheet
              { qsQueues =
                  [ defaultQueue
                      { queueName = Name "one"
                      }
                  , defaultQueue
                      { queueName = Name "two"
                      }
                  ]
              }
        Right expected @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- name: one"
              , "- import: test/extra.yaml"
              ]
          , validFile "/tmp/test/extra.yaml"
              [ "- name: two"
              ]
          ]
    , testCase "error" $ do
        let message = "error loading file: /tmp/extra.yaml"
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- import: extra.yaml"
              ]
          , invalidFile "/tmp/extra.yaml"
          ]
    , testCase "same" $ do
        let message =
              "error loading /tmp/test.yaml: " ++
              "cyclic import: /tmp/test.yaml"
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- import: test.yaml"
              ]
          ]
    , testCase "cyclic" $ do
        let message =
              "error loading /tmp/two.yaml: " ++
              "cyclic import: /tmp/one.yaml"
        Left message @=? loadYaml
          [ validFile "/tmp/test.yaml"
              [ "- import: one.yaml"
              ]
          , validFile "/tmp/one.yaml"
              [ "- import: two.yaml"
              ]
          , validFile "/tmp/two.yaml"
              [ "- import: one.yaml"
              ]
          ]
    ]

------------------------------------------------------------------------------
-- $File

testFileLoad :: TestTree
testFileLoad = testGroup "load"
    [ testCase "error" $ do
        let message = "error loading file: /tmp/test.yaml"
        Left message @=? loadYaml
          [ invalidFile "/tmp/test.yaml"
          ]
    ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "QueueSheet.File"
    [ testGroup "Queue"
        [ testQueueName
        , testQueueUrl
        , testQueueSection
        , testQueueTag
        , testQueueDate
        , testQueuePrev
        , testQueueNext
        ]
    , testGroup "Import"
        [ testImportSection
        , testImportInherit
        , testImportPath
        ]
    , testGroup "File"
        [ testFileLoad
        ]
    ]
