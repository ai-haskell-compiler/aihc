{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- {{{ Imports
import           Text.OPML.Conduit.Parse
import           Text.OPML.Conduit.Render
import           Text.OPML.Lens

import           Arbitrary                    ()
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Combinators     as Conduit (sourceFile)
import           Data.Default
import           Data.String
import           Data.Text.Encoding
import           Data.Tree
import           Data.Version
import           Lens.Micro
import           Paths_opml_conduit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.XML.Stream.Parse        as XML
import           URI.ByteString
-- }}}

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ unitTests
  , properties
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ categoriesCase
  , directoryCase
  , placesCase
  , scriptCase
  , statesCase
  , subscriptionsCase
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ inverseHeadProperty
  -- , inverseProperty
  ]


categoriesCase :: TestTree
categoriesCase = testCase "Parse categories list" $ do
  dataFile <- fromString <$> getDataFileName "data/category.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile .| XML.parseBytes def .| force "Invalid OPML" parseOpml

  (result ^. opmlVersionL) @?= Version [2,0] []
  (result ^. opmlHeadL . opmlTitleL) @?= "Illustrating the category attribute"
  show (result ^. opmlHeadL . opmlCreatedL) @?= "Just 2005-10-31 19:23:00 UTC"
  length (result ^.. opmlOutlinesL . traverse . traverse . opmlOutlineContentL) @?= 1
  map (map length .levels) (result ^. opmlOutlinesL) @?= [[1]]


directoryCase :: TestTree
directoryCase = testCase "Parse directory tree" $ do
  dataFile <- fromString <$> getDataFileName "data/directory.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile .| XML.parseBytes def .| force "Invalid OPML" parseOpml

  (result ^. opmlVersionL) @?= Version [2,0] []
  (result ^. opmlHeadL . opmlTitleL) @?= "scriptingNewsDirectory.opml"
  show (result ^. opmlHeadL . opmlCreatedL) @?= "Just 2005-10-13 15:34:07 UTC"
  show (result ^. opmlHeadL . modifiedL) @?= "Just 2005-10-25 21:33:57 UTC"
  (result ^. opmlHeadL . ownerNameL) @?= "Dave Winer"
  (result ^. opmlHeadL . ownerEmailL) @?= "dwiner@yahoo.com"
  (result ^. opmlHeadL . expansionStateL) @?= []
  (result ^. opmlHeadL . vertScrollStateL) @?= Just 1
  (result ^. opmlHeadL . windowBottomL) @?= Just 386
  (result ^. opmlHeadL . windowLeftL) @?= Just 466
  (result ^. opmlHeadL . windowRightL) @?= Just 964
  (result ^. opmlHeadL . windowTopL) @?= Just 105
  length (result ^.. opmlOutlinesL . traverse . traverse . opmlOutlineUriL) @?= 8
  map (map length .levels) (result ^. opmlOutlinesL) @?= [[1], [1], [1], [1], [1], [1], [1], [1]]


placesCase :: TestTree
placesCase = testCase "Parse places list" $ do
  dataFile <- fromString <$> getDataFileName "data/placesLived.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile .| XML.parseBytes def .| force "Invalid OPML" parseOpml

  (result ^. opmlVersionL) @?= Version [2,0] []
  (result ^. opmlHeadL . opmlTitleL) @?= "placesLived.opml"
  show (result ^. opmlHeadL . opmlCreatedL) @?= "Just 2006-02-27 12:09:48 UTC"
  show (result ^. opmlHeadL . modifiedL) @?= "Just 2006-02-27 12:11:44 UTC"
  (result ^. opmlHeadL . ownerNameL) @?= "Dave Winer"
  fmap (decodeUtf8 . serializeURIRef') (result ^. opmlHeadL . ownerIdL) @?= Just "http://www.opml.org/profiles/sendMail?usernum=1"
  (result ^. opmlHeadL . expansionStateL) @?= [1,2,5,10,13,15]
  (result ^. opmlHeadL . vertScrollStateL) @?= Just 1
  (result ^. opmlHeadL . windowBottomL) @?= Just 665
  (result ^. opmlHeadL . windowLeftL) @?= Just 329
  (result ^. opmlHeadL . windowRightL) @?= Just 547
  (result ^. opmlHeadL . windowTopL) @?= Just 242
  length (result ^.. opmlOutlinesL . traverse . traverse . opmlOutlineContentL) @?= 18
  length (result ^.. opmlOutlinesL . traverse . traverse . opmlOutlineUriL) @?= 1
  map (map length .levels) (result ^. opmlOutlinesL) @?= [[1,6,12]]


scriptCase :: TestTree
scriptCase = testCase "Parse script" $ do
  dataFile <- fromString <$> getDataFileName "data/simpleScript.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile .| XML.parseBytes def .| force "Invalid OPML" parseOpml

  (result ^. opmlVersionL) @?= Version [2,0] []
  (result ^. opmlHeadL . opmlTitleL) @?= "workspace.userlandsamples.doSomeUpstreaming"
  show (result ^. opmlHeadL . opmlCreatedL) @?= "Just 2002-02-11 22:48:02 UTC"
  show (result ^. opmlHeadL . modifiedL) @?= "Just 2005-10-30 03:30:17 UTC"
  (result ^. opmlHeadL . ownerNameL) @?= "Dave Winer"
  (result ^. opmlHeadL . ownerEmailL) @?= "dwiner@yahoo.com"
  (result ^. opmlHeadL . expansionStateL) @?= [1, 2, 4]
  (result ^. opmlHeadL . vertScrollStateL) @?= Just 1
  (result ^. opmlHeadL . windowBottomL) @?= Just 314
  (result ^. opmlHeadL . windowLeftL) @?= Just 41
  (result ^. opmlHeadL . windowRightL) @?= Just 475
  (result ^. opmlHeadL . windowTopL) @?= Just 74
  length (result ^.. opmlOutlinesL . traverse . traverse . opmlOutlineContentL) @?= 11
  map (map length .levels) (result ^. opmlOutlinesL) @?= [[1,2,2], [1,2], [1], [1,1]]


statesCase :: TestTree
statesCase = testCase "Parse states list" $ do
  dataFile <- fromString <$> getDataFileName "data/states.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile .| XML.parseBytes def .| force "Invalid OPML" parseOpml

  (result ^. opmlVersionL) @?= Version [2,0] []
  (result ^. opmlHeadL . opmlTitleL) @?= "states.opml"
  show (result ^. opmlHeadL . opmlCreatedL) @?= "Just 2005-03-15 16:35:45 UTC"
  show (result ^. opmlHeadL . modifiedL) @?= "Just 2005-07-14 23:41:05 UTC"
  (result ^. opmlHeadL . ownerNameL) @?= "Dave Winer"
  (result ^. opmlHeadL . ownerEmailL) @?= "dave@scripting.com"
  (result ^. opmlHeadL . expansionStateL) @?= [1, 6, 13, 16, 18, 20]
  (result ^. opmlHeadL . vertScrollStateL) @?= Just 1
  (result ^. opmlHeadL . windowBottomL) @?= Just 558
  (result ^. opmlHeadL . windowLeftL) @?= Just 106
  (result ^. opmlHeadL . windowRightL) @?= Just 479
  (result ^. opmlHeadL . windowTopL) @?= Just 106
  length (result ^.. opmlOutlinesL . traverse . traverse . opmlOutlineContentL) @?= 63
  map (map length .levels) (result ^. opmlOutlinesL) @?= [[1, 8, 50, 4]]


subscriptionsCase :: TestTree
subscriptionsCase = testCase "Parse subscriptions list" $ do
  dataFile <- fromString <$> getDataFileName "data/subscriptionList.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile .| XML.parseBytes def .| force "Invalid OPML" parseOpml

  (result ^. opmlVersionL) @?= Version [2,0] []
  (result ^. opmlHeadL . opmlTitleL) @?= "mySubscriptions.opml"
  show (result ^. opmlHeadL . opmlCreatedL) @?= "Just 2005-06-18 12:11:52 UTC"
  show (result ^. opmlHeadL . modifiedL) @?= "Just 2005-08-02 21:42:48 UTC"
  (result ^. opmlHeadL . ownerNameL) @?= "Dave Winer"
  (result ^. opmlHeadL . ownerEmailL) @?= "dave@scripting.com"
  (result ^. opmlHeadL . vertScrollStateL) @?= Just 1
  (result ^. opmlHeadL . windowBottomL) @?= Just 562
  (result ^. opmlHeadL . windowLeftL) @?= Just 304
  (result ^. opmlHeadL . windowRightL) @?= Just 842
  (result ^. opmlHeadL . windowTopL) @?= Just 61
  length (result ^.. opmlOutlinesL . traverse . traverse . opmlOutlineSubscriptionL) @?= 13

inverseHeadProperty :: TestTree
inverseHeadProperty = testProperty "parse . render = id (on OpmlHead)" $ \opmlHead -> either (const False) (opmlHead ==) (runConduit $ renderOpmlHead opmlHead .| force "Invalid <head>" parseOpmlHead)

-- inverseProperty :: TestTree
-- inverseProperty = testProperty "parse . render = id" $ \opml -> either (const False) (opml ==) (runIdentity . runCatchT . runConduit $ renderOpml opml .| force "Invalid OPML" parseOpml)
