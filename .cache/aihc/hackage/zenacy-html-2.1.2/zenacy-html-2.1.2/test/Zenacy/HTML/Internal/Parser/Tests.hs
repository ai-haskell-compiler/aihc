{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Zenacy.HTML.Internal.Parser.Tests
  ( testParser
  ) where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Char
import Zenacy.HTML.Internal.DOM
import Zenacy.HTML.Internal.Lexer
import Zenacy.HTML.Internal.Parser
import Zenacy.HTML.Internal.Token
import Zenacy.HTML.Internal.Types
import Control.Monad.ST
import Data.Default
  ( Default(..)
  )
import Data.Either.Extra
  ( fromRight
  )
import Data.IntMap
  ( IntMap
  )
import qualified Data.IntMap as IntMap
  ( fromList
  )
import Data.Sequence
  ( Seq
  )
import qualified Data.Sequence as Seq
  ( fromList
  )
import GHC.Exts
  ( IsList(..)
  )
import Test.Framework
  ( Test
  , testGroup
  )
import Test.Framework.Providers.HUnit
  ( testCase
  )
import Test.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  )
import Text.RawString.QQ
import Text.Show.Pretty
  ( ppShow
  , pPrint
  )

testParser :: Test
testParser = testGroup "Zenacy.HTML.Internal.Parser"
  [ testBasic
  , testComment
  , testTemplate
  , testType
  , testCode
  ]

testBasic :: Test
testBasic = testCase "parser basic" $ do
  assertEqual "TEST 1" domData $ parserResultDOM $ getResult htmlData

testComment :: Test
testComment = testCase "parser comment" $ do
  assertEqual "TEST 1" domComm $ parserResultDOM $ getResult htmlComm

testTemplate :: Test
testTemplate = testCase "parser template" $ do
  assertEqual "TEST 1" domTemp $ parserResultDOM $ getResult htmlTemp

testType :: Test
testType = testCase "parser type" $ do
  assertEqual "TEST 1" domType $ parserResultDOM $ getResult htmlType

testCode :: Test
testCode = testCase "parser code" $ do
  assertEqual "TEST 1" domCode $ parserResultDOM $ getResult htmlCode

-- | This test case is used to make other test cases by rendering
-- the results of the dom.
testRender :: Test
testRender = testCase "parser render" $ do
  let r = getResult htmlCode
  let d = parserResultDOM r
  pPrint d
  assertEqual "TEST 1" def $ ppShow d

getResult :: BS -> ParserResult
getResult s = fromRight def $ parseDocument def { parserOptionInput = s }

htmlData = [r|<html><body>aaa<a href="https://example.com">bbb</a>ccc</body></html>|]

htmlComm = [r|<!--abcxyz--><!-- abcxyz -->|]

htmlTemp = [r|<!DOCTYPE html><template><div>a</div></template>|]

htmlType = [r|<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">|]

htmlCode = [r|<!DOCTYPE html>&#x2212;&#x0394;&#160;|]

domData =
  DOM
    { domNodes =
        fromList
          [ ( 1
            , DOMDocument
                { domDocumentID = 0
                , domDocumentParent = 0
                , domDocumentName = ""
                , domDocumentChildren = fromList [ 2 ]
                , domDocumentQuirksMode = DOMQuirksMode
                }
            )
          , ( 2
            , DOMElement
                { domElementID = 2
                , domElementParent = 1
                , domElementName = "html"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList [ 3 , 4 ]
                }
            )
          , ( 3
            , DOMElement
                { domElementID = 3
                , domElementParent = 2
                , domElementName = "head"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList []
                }
            )
          , ( 4
            , DOMElement
                { domElementID = 4
                , domElementParent = 2
                , domElementName = "body"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList [ 5 , 6 , 8 ]
                }
            )
          , ( 5
            , DOMText
                { domTextID = 5 , domTextParent = 4 , domTextData = "aaa" }
            )
          , ( 6
            , DOMElement
                { domElementID = 6
                , domElementParent = 4
                , domElementName = "a"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes =
                    fromList
                      [ DOMAttr
                          { domAttrName = "href"
                          , domAttrVal = "https://example.com"
                          , domAttrNamespace = HTMLAttrNamespaceNone
                          }
                      ]
                , domElementChildren = fromList [ 7 ]
                }
            )
          , ( 7
            , DOMText
                { domTextID = 7 , domTextParent = 6 , domTextData = "bbb" }
            )
          , ( 8
            , DOMText
                { domTextID = 8 , domTextParent = 4 , domTextData = "ccc" }
            )
          ]
    , domNextID = 9
    }

domComm =
  DOM
    { domNodes =
        fromList
          [ ( 1
            , DOMDocument
                { domDocumentID = 0
                , domDocumentParent = 0
                , domDocumentName = ""
                , domDocumentChildren = fromList [ 2 , 3 , 4 ]
                , domDocumentQuirksMode = DOMQuirksMode
                }
            )
          , ( 2
            , DOMComment
                { domCommentID = 2
                , domCommentParent = 1
                , domCommentData = "abcxyz"
                }
            )
          , ( 3
            , DOMComment
                { domCommentID = 3
                , domCommentParent = 1
                , domCommentData = " abcxyz "
                }
            )
          , ( 4
            , DOMElement
                { domElementID = 4
                , domElementParent = 1
                , domElementName = "html"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList [ 5 , 6 ]
                }
            )
          , ( 5
            , DOMElement
                { domElementID = 5
                , domElementParent = 4
                , domElementName = "head"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList []
                }
            )
          , ( 6
            , DOMElement
                { domElementID = 6
                , domElementParent = 4
                , domElementName = "body"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList []
                }
            )
          ]
    , domNextID = 7
    }

domTemp =
  DOM
    { domNodes =
        fromList
          [ ( 1
            , DOMDocument
                { domDocumentID = 0
                , domDocumentParent = 0
                , domDocumentName = ""
                , domDocumentChildren = fromList [ 2 , 3 ]
                , domDocumentQuirksMode = DOMQuirksNone
                }
            )
          , ( 2
            , DOMDoctype
                { domDoctypeID = 2
                , domDoctypeParent = 1
                , domDoctypeName = "html"
                , domDoctypePublicID = Nothing
                , domDoctypeSystemID = Nothing
                }
            )
          , ( 3
            , DOMElement
                { domElementID = 3
                , domElementParent = 1
                , domElementName = "html"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList [ 4 , 9 ]
                }
            )
          , ( 4
            , DOMElement
                { domElementID = 4
                , domElementParent = 3
                , domElementName = "head"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList [ 6 ]
                }
            )
          , ( 5
            , DOMFragment
                { domFragmentID = 5
                , domFragmentParent = 6
                , domFragmentName = ""
                , domFragmentChildren = fromList [ 7 ]
                }
            )
          , ( 6
            , DOMTemplate
                { domTemplateID = 6
                , domTemplateParent = 4
                , domTemplateNamespace = HTMLNamespaceHTML
                , domTemplateAttributes = fromList []
                , domTemplateContents = 5
                }
            )
          , ( 7
            , DOMElement
                { domElementID = 7
                , domElementParent = 5
                , domElementName = "div"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList [ 8 ]
                }
            )
          , ( 8
            , DOMText { domTextID = 8 , domTextParent = 7 , domTextData = "a" }
            )
          , ( 9
            , DOMElement
                { domElementID = 9
                , domElementParent = 3
                , domElementName = "body"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList []
                }
            )
          ]
    , domNextID = 10
    }

domType =
  DOM
    { domNodes =
        fromList
          [ ( 1
            , DOMDocument
                { domDocumentID = 0
                , domDocumentParent = 0
                , domDocumentName = ""
                , domDocumentChildren = fromList [ 2 , 3 ]
                , domDocumentQuirksMode = DOMQuirksNone
                }
            )
          , ( 2
            , DOMDoctype
                { domDoctypeID = 2
                , domDoctypeParent = 1
                , domDoctypeName = "html"
                , domDoctypePublicID = Just "-//W3C//DTD HTML 4.01//EN"
                , domDoctypeSystemID = Just "http://www.w3.org/TR/html4/strict.dtd"
                }
            )
          , ( 3
            , DOMElement
                { domElementID = 3
                , domElementParent = 1
                , domElementName = "html"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList [ 4 , 5 ]
                }
            )
          , ( 4
            , DOMElement
                { domElementID = 4
                , domElementParent = 3
                , domElementName = "head"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList []
                }
            )
          , ( 5
            , DOMElement
                { domElementID = 5
                , domElementParent = 3
                , domElementName = "body"
                , domElementNamespace = HTMLNamespaceHTML
                , domElementAttributes = fromList []
                , domElementChildren = fromList []
                }
            )
          ]
    , domNextID = 6
    }

domCode =
  DOM
  { domNodes =
      fromList
        [ ( 1
          , DOMDocument
              { domDocumentID = 0
              , domDocumentParent = 0
              , domDocumentName = ""
              , domDocumentChildren = fromList [ 2 , 3 ]
              , domDocumentQuirksMode = DOMQuirksNone
              }
          )
        , ( 2
          , DOMDoctype
              { domDoctypeID = 2
              , domDoctypeParent = 1
              , domDoctypeName = "html"
              , domDoctypePublicID = Nothing
              , domDoctypeSystemID = Nothing
              }
          )
        , ( 3
          , DOMElement
              { domElementID = 3
              , domElementParent = 1
              , domElementName = "html"
              , domElementNamespace = HTMLNamespaceHTML
              , domElementAttributes = fromList []
              , domElementChildren = fromList [ 4 , 5 ]
              }
          )
        , ( 4
          , DOMElement
              { domElementID = 4
              , domElementParent = 3
              , domElementName = "head"
              , domElementNamespace = HTMLNamespaceHTML
              , domElementAttributes = fromList []
              , domElementChildren = fromList []
              }
          )
        , ( 5
          , DOMElement
              { domElementID = 5
              , domElementParent = 3
              , domElementName = "body"
              , domElementNamespace = HTMLNamespaceHTML
              , domElementAttributes = fromList []
              , domElementChildren = fromList [ 6 ]
              }
          )
        , ( 6
          , DOMText
              { domTextID = 6
              , domTextParent = 5
              , domTextData = "\226\136\146\206\148\194\160"
              }
          )
        ]
  , domNextID = 7
  }
