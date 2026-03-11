module Text.RSS.Import.Tests
  ( rssImportTests
  ) where

import Prelude.Compat

import Data.XML.Types as XML
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)
import Text.RSS.Equals ()
import Text.RSS.Import
import Text.RSS.Syntax
import Text.RSS.Utils

rssImportTests :: Test
rssImportTests =
  testGroup
    "Text.RSS.Import"
    [ mutuallyExclusive $
      testGroup "RSS import" [testElementToCloudIsNotCreated, testElementToCloud]
    ]

testElementToCloudIsNotCreated :: Test
testElementToCloudIsNotCreated = testCase "should not create rss cloud" notCreateRSSCloud
  where
    notCreateRSSCloud :: Assertion
    notCreateRSSCloud = do
      let notXmlCloudElement =
            XML.Element
              {elementName = createQName "notCloud", elementAttributes = [], elementNodes = []}
      let expected = Nothing
      assertEqual "not create rss cloud" expected (elementToCloud notXmlCloudElement)

testElementToCloud :: Test
testElementToCloud = testCase "should create rss cloud" createRSSCloud
  where
    createRSSCloud :: Assertion
    createRSSCloud = do
      let attr = mkNAttr (createQName "attr") "text for attr"
      let xmlCloudElement =
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
      let expected =
            Just
              RSSCloud
                { rssCloudDomain = Just "domain cloud"
                , rssCloudPort = Just "port cloud"
                , rssCloudPath = Just "path cloud"
                , rssCloudRegisterProcedure = Just "register cloud"
                , rssCloudProtocol = Just "protocol cloud"
                , rssCloudAttrs = [attr]
                }
      assertEqual "create rss cloud" expected (elementToCloud xmlCloudElement)
