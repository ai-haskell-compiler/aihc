module Text.RSS.Export.Tests
  ( rssExportTests
  ) where

import Prelude.Compat

import Data.Text (pack)
import Data.XML.Types as XML
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)
import Text.RSS.Equals ()
import Text.RSS.Export
import Text.RSS.Syntax
import Text.RSS.Utils

rssExportTests :: Test
rssExportTests =
  testGroup
    "Text.RSS.Export"
    [ mutuallyExclusive $
      testGroup
        "RSS export"
        [ testCreateXMLImage
        , testCreateXMLCloud
        , testCreateXMLTextInput
        , testCreateEmptyXMLSkipHours
        , testCreateXMLSkipHours
        , testCreateEmptyXMLSkipDays
        , testCreateXMLSkipDays
        , testCreateXMLAttr
        , testCreateXMLLeaf
        ]
    ]

testCreateXMLImage :: Test
testCreateXMLImage = testCase "should create image as xml" testImage
  where
    testImage :: Assertion
    testImage = do
      let other =
            XML.Element
              { elementName = createQName "other"
              , elementAttributes = [] :: [Attr]
              , elementNodes = [createContent "image other"]
              }
      let image =
            RSSImage
              { rssImageURL = "image url"
              , rssImageTitle = "image title"
              , rssImageLink = "image link"
              , rssImageWidth = Just 100
              , rssImageHeight = Just 200
              , rssImageDesc = Just "image desc"
              , rssImageOther = [other]
              }
      let expected =
            XML.Element
              { elementName = createQName "image"
              , elementAttributes = [] :: [Attr]
              , elementNodes =
                  [ NodeElement
                      XML.Element
                        { elementName = createQName "url"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "image url"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "title"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "image title"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "link"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "image link"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "width"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "100"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "height"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "200"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "description"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "image desc"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "other"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "image other"]
                        }
                  ]
              }
      assertEqual "image" expected (xmlImage image)

testCreateXMLCloud :: Test
testCreateXMLCloud = testCase "should create cloud as xml" testCloud
  where
    testCloud :: Assertion
    testCloud = do
      let attr = mkNAttr (createQName "attr") "text for attr"
      let cloud =
            RSSCloud
              { rssCloudDomain = Just "domain cloud"
              , rssCloudPort = Just "port cloud"
              , rssCloudPath = Just "path cloud"
              , rssCloudRegisterProcedure = Just "register cloud"
              , rssCloudProtocol = Just "protocol cloud"
              , rssCloudAttrs = [attr]
              }
      let expected =
            XML.Element
              { elementName = createQName "cloud"
              , elementAttributes =
                  [ mkNAttr (createQName "domain") "domain cloud"
                  , mkNAttr (createQName "port") "port cloud"
                  , mkNAttr (createQName "path") "path cloud"
                  , mkNAttr (createQName "registerProcedure") "register cloud"
                  , mkNAttr (createQName "protocol") "protocol cloud"
                  , attr
                  ] :: [Attr]
              , elementNodes = [createContent ""]
              }
      assertEqual "cloud" expected (xmlCloud cloud)

testCreateXMLTextInput :: Test
testCreateXMLTextInput = testCase "should create text input as xml" textInput
  where
    textInput :: Assertion
    textInput = do
      let attr = mkNAttr (createQName "attr") "text for attr"
      let other =
            XML.Element
              { elementName = createQName "leaf"
              , elementAttributes = [] :: [Attr]
              , elementNodes = [createContent "text for leaf"]
              }
      let input =
            RSSTextInput
              { rssTextInputTitle = "title"
              , rssTextInputDesc = "desc"
              , rssTextInputName = "name"
              , rssTextInputLink = "http://url.com"
              , rssTextInputAttrs = [attr]
              , rssTextInputOther = [other]
              }
      let expected =
            XML.Element
              { elementName = createQName "textInput"
              , elementAttributes = [attr] :: [Attr]
              , elementNodes =
                  [ NodeElement
                      XML.Element
                        { elementName = createQName "title"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "title"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "description"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "desc"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "name"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "name"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "link"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "http://url.com"]
                        }
                  , NodeElement
                      XML.Element
                        { elementName = createQName "leaf"
                        , elementAttributes = [] :: [Attr]
                        , elementNodes = [createContent "text for leaf"]
                        }
                  ]
              }
      assertEqual "text input" expected (xmlTextInput input)

testCreateEmptyXMLSkipHours :: Test
testCreateEmptyXMLSkipHours =
  testCase "should create an empty list of skip hours as xml" emptySkipHours
  where
    emptySkipHours :: Assertion
    emptySkipHours = do
      let hoursToSkip = []
      let expected =
            XML.Element
              { elementName = createQName "skipHours"
              , elementAttributes = [] :: [Attr]
              , elementNodes = []
              }
      assertEqual "empty skip hours" expected (xmlSkipHours hoursToSkip)

testCreateXMLSkipHours :: Test
testCreateXMLSkipHours = testCase "should create skip hours as xml" skipHours
  where
    skipHours :: Assertion
    skipHours = do
      let hoursToSkip = [1, 2, 3]
      let expected =
            XML.Element
              { elementName = createQName "skipHours"
              , elementAttributes = [] :: [Attr]
              , elementNodes = [hourElem 0, hourElem 1, hourElem 2]
              }
            where
              hourElem ind =
                NodeElement
                  XML.Element
                    { elementName = createQName "hour"
                    , elementAttributes = [] :: [Attr]
                    , elementNodes = [createContent $ pack $ show $ hoursToSkip !! ind]
                    }
      assertEqual "skip hours" expected (xmlSkipHours hoursToSkip)

testCreateEmptyXMLSkipDays :: Test
testCreateEmptyXMLSkipDays =
  testCase "should create an empty list of skip days as xml" emptySkipDays
  where
    emptySkipDays :: Assertion
    emptySkipDays = do
      let daysToSkip = []
      let expected =
            XML.Element
              { elementName = createQName "skipDays"
              , elementAttributes = [] :: [Attr]
              , elementNodes = []
              }
      assertEqual "empty skip days" expected (xmlSkipDays daysToSkip)

testCreateXMLSkipDays :: Test
testCreateXMLSkipDays = testCase "should create skip days as xml" skipDays
  where
    skipDays :: Assertion
    skipDays = do
      let daysToSkip = ["first day", "second day", "third day"]
      let expected =
            XML.Element
              { elementName = createQName "skipDays"
              , elementAttributes = [] :: [Attr]
              , elementNodes = [dayElem 0, dayElem 1, dayElem 2]
              }
            where
              dayElem ind =
                NodeElement
                  XML.Element
                    { elementName = createQName "day"
                    , elementAttributes = [] :: [Attr]
                    , elementNodes = [createContent $ daysToSkip !! ind]
                    }
      assertEqual "skip days" expected (xmlSkipDays daysToSkip)

testCreateXMLAttr :: Test
testCreateXMLAttr = testCase "should create attr as xml" createXMLAttr
  where
    createXMLAttr :: Assertion
    createXMLAttr = do
      let tg = "attr"
      let txt = "example of attr value"
      let expected = mkNAttr (createQName tg) txt
      assertEqual "create a leaf" expected (xmlAttr tg txt)

testCreateXMLLeaf :: Test
testCreateXMLLeaf = testCase "should create leaf as xml" createXMLLeaf
  where
    createXMLLeaf :: Assertion
    createXMLLeaf = do
      let tg = "leaf"
      let txt = "example of leaf text"
      let expected =
            XML.Element
              { elementName = createQName tg
              , elementAttributes = [] :: [Attr]
              , elementNodes = [createContent txt]
              }
      assertEqual "create a leaf" expected (xmlLeaf tg txt)
