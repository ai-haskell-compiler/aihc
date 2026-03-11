{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Text.XML.DublinCore.Conduit.Parse
import Text.XML.DublinCore.Conduit.Render

import Conduit
import Data.Monoid
import Data.Text
import Data.Time.Clock
import Data.XML.Types
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ properties
      ]

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ roundtripTest "contributor" renderElementContributor elementContributor
    , roundtripTest "coverage" renderElementCoverage elementCoverage
    , roundtripTest "creator" renderElementCreator elementCreator
    , roundtripTest' "date" renderElementDate elementDate genTime
    , roundtripTest "description" renderElementDescription elementDescription
    , roundtripTest "format" renderElementFormat elementFormat
    , roundtripTest "identifier" renderElementIdentifier elementIdentifier
    , roundtripTest "language" renderElementLanguage elementLanguage
    , roundtripTest "publisher" renderElementPublisher elementPublisher
    , roundtripTest "relation" renderElementRelation elementRelation
    , roundtripTest "rights" renderElementRights elementRights
    , roundtripTest "source" renderElementSource elementSource
    , roundtripTest "subject" renderElementSubject elementSubject
    , roundtripTest "title" renderElementTitle elementTitle
    , roundtripTest "type" renderElementType elementType
    ]

roundtripTest
  :: Eq a
  => Show a
  => Arbitrary a
  => TestName
  -> (a -> ConduitT () Event (Either e) ())
  -> ConduitT Event Void (Either e) (Maybe a)
  -> TestTree
roundtripTest name render parse = testProperty ("parse . render = id (" <> name <> ")") $ \t -> either (const False) (Just t ==) (runConduit $ render t .| parse)

roundtripTest'
  :: Eq a
  => Show a
  => TestName
  -> (a -> ConduitT () Event (Either e) ())
  -> ConduitT Event Void (Either e) (Maybe a)
  -> Gen a
  -> TestTree
roundtripTest' name render parse gen = testProperty ("parse . render = id (" <> name <> ")") $ do
  a <- gen
  return $ either (const False) (Just a ==) $ runConduit (render a .| parse)

-- | Generate 'UTCTime' with rounded seconds.
genTime :: Gen UTCTime
genTime = do
  (UTCTime d s) <- arbitrary
  return $ UTCTime d $ fromIntegral (round s :: Int)
