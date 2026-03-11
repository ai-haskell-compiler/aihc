{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.ByteString.Lazy as LazyByteString
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Text.XML as Xc
import qualified XmlParser as Xp
import Prelude hiding (assert)

main :: IO ()
main =
  defaultMain
    $ testGroup "All tests"
    $ [ testProperty "ByName/many" $ do
          bContents <- fmap (fromString . show) <$> listOf (chooseInt (0, 99))

          xml <-
            let childByName name =
                  Xc.Element name [] . pure . Xc.NodeContent
                root =
                  Xc.Element "a" [] . fmap (Xc.NodeElement)
             in fmap (documentByteString . elementDocument . root . join)
                  $ forM bContents
                  $ \bContent -> do
                    prefix <-
                      oneof
                        [ pure [],
                          listOf (childByName "c" <$> arbitrary)
                        ]
                    return (prefix <> [childByName "b" bContent])

          let parser =
                Xp.childrenByName $ many $ Xp.byName Nothing "b" $ Xp.children $ Xp.contentNode $ Xp.textContent
              result = Xp.parseByteString parser xml

          return (result === Right bContents),
        testCase "Namespaces" $ do
          let parser = Xp.childrenByName $ Xp.byName (Just "B") "c" $ Xp.children $ Xp.contentNode $ Xp.textContent
              result = Xp.parseByteString parser "<a xmlns:b=\"B\"><b:c>d</b:c></a>"
          assertEqual "" (Right "d") result,
        testCase "Error" $ do
          let parser = Xp.childrenByName $ Xp.byName Nothing "c" $ Xp.children $ Xp.contentNode $ Xp.textContent
              result = Xp.parseByteString parser "<a><b>c</b><d/></a>"
          assertEqual "" (Left "/: None of following child element names found: [c]. Names available: [b, d]") result,
        testCase "QName content" $ do
          let input = "<root xmlns:abc='abc-uri'>abc:d</root>"
              parser = Xp.children $ Xp.contentNode $ Xp.qNameContent
           in assertEqual "" (Right (Just "abc-uri", "d")) (Xp.parseByteString parser input),
        testGroup
          "Regressions"
          [ testCase "Empty string content parsing"
              $ let input = "<Value xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xsi:type=\"xsd:string\"></Value>" :: ByteString
                    parser = Xp.children $ Xp.contentNode $ Xp.textContent
                 in assertEqual
                      ""
                      (Right "")
                      (Xp.parseByteString parser input)
          ]
      ]

documentByteString :: Xc.Document -> ByteString
documentByteString = LazyByteString.toStrict . Xc.renderLBS Xc.def

elementDocument :: Xc.Element -> Xc.Document
elementDocument x =
  Xc.Document (Xc.Prologue [] Nothing []) x []
