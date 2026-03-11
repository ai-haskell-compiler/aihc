{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}

-- module Tests where

import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.XML.Pickle
import Data.XML.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import qualified Data.Text as Text

-- import Data.Monoid
-- import Test.QuickCheck
-- import Test.Framework
-- import Test.Framework.Providers.QuickCheck2

main =
  defaultMain $ testGroup "all"
    [ testProperty "prop_showXpPrim" prop_showXpPrim
    , testProperty "prop_xpUnit" prop_xpUnit
    , testProperty "prop_xpAttribute" prop_xpAttribute
    , testProperty "prop_xpElemVerbatimUnpickle" prop_xpElemVerbatimUnpickle
    , testProperty "prop_xpElemVerbatimPickle" prop_xpElemVerbatimPickle
    , testProperty "prop_xpElemTextPickle" prop_xpElemTextPickle
    , testProperty "prop_xpElemBlankUnpickleBlanks"
        prop_xpElemBlankUnpickleBlanks
    , testProperty "prop_xpElemBlankUnpickleNonBlanks"
        prop_xpElemBlankUnpickleNonBlanks
    , testProperty "prop_xpElemBlankPickle" prop_xpElemBlankPickle
    , testProperty "prop_xpElemExistsPickleTrue" prop_xpElemExistsPickleTrue
    , testProperty "prop_xpElemExistsPickleFalse" prop_xpElemExistsPickleFalse
    , testProperty "prop_xpElemExistsUnpickleBlanks"
        prop_xpElemExistsUnpickleBlanks
    , testProperty "prop_xpElemExistsUnpickleNonBlanks"
        prop_xpElemExistsUnpickleNonBlanks
    , testProperty "prop_xpBool" prop_xpBool
    , testProperty "prop_flattenContent" prop_flattenContent
    , testCase "xpBoolUnpickleTrue" $ assertBool "" xpBoolUnpickleTrue
    , testCase "xpBoolUnpickleFalse" $ assertBool "" xpBoolUnpickleFalse
    , testCase "xpBoolUnpickle1" $ assertBool "" xpBoolUnpickle1
    , testCase "xpBoolUnpickle0" $ assertBool "" xpBoolUnpickle0
    , testCase "xpBoolPickleTrue" $ assertBool "" xpBoolPickleTrue
    , testCase "xpBoolPickleFalse" $ assertBool "" xpBoolPickleFalse 
    ]

-- Generates an arbitrary XML name. Namespaces (not part of XML 1.0) are not
-- generated.
instance Arbitrary Name where
  arbitrary = do
    localName <- xmlString
    prefix <- oneof [return $ Nothing, xmlString >>= return . Just]
    return Name { nameLocalName = localName
                , nameNamespace = Nothing
                , namePrefix = prefix }
    where
      xmlString =
        -- We want to include a suitable set of non-ASCII characters here.
        let range = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_" in do
          char <- elements range
          string <- listOf $ elements $ "-." ++ range
          return . Text.pack $ char:string

-- We're using the printable ASCII characters. The commented list includes the
-- printable characters of the ISO 8859-1 extension.
instance Arbitrary Content where
  arbitrary = do
    string <- listOf $ elements ['\0032'..'\0126'] -- ['\0160'..'\0255']
    return $ ContentText $ Text.pack string

-- Generator for XML elements.
instance Arbitrary Element where
  arbitrary = do
    name <- arbitrary
    attributes <- arbitrary
    nodes <- arbitrary
    return Element { elementName = name
                   , elementAttributes = attributes
                   , elementNodes = nodes }

-- Generator for nodes. We resize child elements to avoid infinite structures.
instance Arbitrary Node where
  arbitrary = oneof [ sized (\n -> resize (n `div` 2) $ do
                                element <- arbitrary
                                return $ NodeElement element)
                    , arbitrary >>= \content -> return $ NodeContent content
                    , arbitrary >>= \comment -> return $ NodeComment comment
                    ]

-- Our `Text' generator is simply a wrapper around the `String' generator.
instance Arbitrary Text where
  arbitrary = liftM Text.pack arbitrary

-- Generator for an XML element without attributes and with child nodes that
-- are considered empty (see `emptyNodeGen').
emptyElementGen :: Gen Element
emptyElementGen = do
    name <- arbitrary
    nodes <- listOf emptyNodeGen
    return Element { elementName = name
                   , elementAttributes = []
                   , elementNodes = nodes }

-- Generator for child nodes that are considered empty: "" strings and comments.
emptyNodeGen :: Gen Node
emptyNodeGen = oneof [ return $ NodeContent (ContentText "")
                     , arbitrary >>= \comment -> return $ NodeComment comment ]

-- Generator for XML elements with only text.
elementWithTextGen :: Gen Element
elementWithTextGen = do
    name <- arbitrary
    nodes <- listOf nodeWithTextGen
    return Element { elementName = name
                   , elementAttributes = []
                   , elementNodes = nodes }

-- Generator for XML nodes with only text content and comments.
nodeWithTextGen :: Gen Node
nodeWithTextGen = do
    content0 <- arbitrary
    content1 <- arbitrary
    oneof [return $ NodeContent content0, return $ NodeComment content1]

-- Verify that the pickled show value is the same as the show value itself.
prop_showXpPrim :: Integer -> Bool
prop_showXpPrim a = Text.unpack (pickle xpPrim a) == show a

-- Verify that `xpUnit' never fails.
prop_xpUnit :: [Integer] -> Bool
prop_xpUnit as = (case unpickle xpUnit as of
                   Right () -> True
                   _ -> False) && null (pickle xpUnit ())

-- Given a list of attributes, every attribute should be present upon unpickle.
prop_xpAttribute :: [Attribute] -> Bool
prop_xpAttribute attributes =
  let result = map (\(n, c) -> let t = contentToText c
                               in unpickle (xpAttribute_ n t) attributes)
               attributes
  in length result == length (rights (result))

-- Provided a list of Nodes containing at least one Element, we verify that
-- xpElemVerbatim always finds the first element.
prop_xpElemVerbatimUnpickle :: [Node] -> Property
prop_xpElemVerbatimUnpickle ns =
  let ne = find (\n -> case n of NodeElement _ -> True; _ -> False) ns
  in isJust ne ==> let NodeElement e = fromJust ne
                   in case unpickle xpElemVerbatim ns of
                     Right e_ -> e == e_
                     Left _ -> False

-- Verify that xpElemVerbatim pickling an element returns only that element.
prop_xpElemVerbatimPickle :: Element -> Bool
prop_xpElemVerbatimPickle e = pickle xpElemVerbatim e == [NodeElement e]

-- Verify that xpElemText pickling text content returns that content properly.
prop_xpElemTextPickle :: Name -> Text -> Bool
prop_xpElemTextPickle n t =
  let nodes = pickle (xpElemText n) t
  in nodes == [ NodeElement
                  Element { elementName = n
                          , elementAttributes = []
                          , elementNodes = [NodeContent (ContentText t)]
                          } ]

-- Pickling of blanks should work.
prop_xpElemBlankUnpickleBlanks :: Property
prop_xpElemBlankUnpickleBlanks =
  forAll (listOf $ oneof [emptyNodeGen, arbitrary]) $ \nodes ->
  let blanks = filter (isEmpty . unwrapElement) (filterNodeElements nodes)
  in all (\e -> case unpickle (xpElemBlank (elementName e)) nodes of
             Right () -> True
             r -> False) (filterMapNodeElements blanks)

-- Pickling of non-blanks should not work.
prop_xpElemBlankUnpickleNonBlanks :: [Node] -> Bool
prop_xpElemBlankUnpickleNonBlanks nodes =
  let nonBlanks = filter (not . isEmpty . unwrapElement)
                    (filterNodeElements nodes)
  in all (\e -> case unpickle (xpElemBlank (elementName e)) nodes of
             Right () -> False
             _ -> True) (filterMapNodeElements nonBlanks)

-- Return true if element has no attributes, and has no non-comment nodes.
isEmpty :: Element -> Bool
isEmpty Element { elementAttributes = [], elementNodes = [] } = True
isEmpty Element { elementAttributes = [], elementNodes = ns }
  | all (\n -> case n of
            NodeComment _ -> True
            NodeContent (ContentText "") -> True
            NodeContent (ContentEntity _) ->
              error "Content entities not supported!"
            _ -> False) ns = True
isEmpty _ = False

-- Verify that `xpElemText' pickling text content returns that content
-- properly.
prop_xpElemBlankPickle :: Name -> Bool
prop_xpElemBlankPickle n =
  let nodes = pickle (xpElemBlank n) ()
  in nodes == [ NodeElement
                 Element { elementName = n
                         , elementAttributes = []
                         , elementNodes = []
                         } ]

-- Verify that `xpElemExists' always generates an empty `NodeElement' with the
-- correct name when the provided parameter is `True'.
prop_xpElemExistsPickleTrue :: Name -> Bool
prop_xpElemExistsPickleTrue n =
  pickle (xpElemExists n) True == [NodeElement Element { elementName = n
                                                       , elementAttributes = []
                                                       , elementNodes = [] }]

-- Verify that xpElemExists always generates an empty list when the provided
-- parameter is `False'.
prop_xpElemExistsPickleFalse :: Name -> Bool
prop_xpElemExistsPickleFalse n = pickle (xpElemExists n) False == []

-- Pickling of empty elements should work.
-- Verifies that all generated elements are actually found, that no error is
-- generated if the element is empty, and that an error is generated if the
-- element is not empty.
prop_xpElemExistsUnpickleBlanks :: Property
prop_xpElemExistsUnpickleBlanks =
  forAll (listOf $ oneof [emptyNodeGen, arbitrary]) $ \nodes ->
  let blanks = filter (isEmpty . unwrapElement) (filterNodeElements nodes)
  in all (\e -> case unpickle (xpElemExists (elementName e)) nodes of
             Right True -> True
             _ -> False) (filterMapNodeElements blanks)

-- Pickling of non-empty elements should not work.
prop_xpElemExistsUnpickleNonBlanks :: [Node] -> Bool
prop_xpElemExistsUnpickleNonBlanks nodes =
  let nonBlanks = filter (not . isEmpty . unwrapElement)
                    (filterNodeElements nodes)
  in all (\e -> case unpickle (xpElemExists (elementName e)) nodes of
             Right _ -> False
             _ -> True) (filterMapNodeElements nonBlanks)

-- Remove all Node values that are not NodeElement values, and convert the
-- remaining values to Element values.
filterMapNodeElements :: [Node] -> [Element]
filterMapNodeElements = map unwrapElement . filterNodeElements

-- Remove node children that are not elements.
filterNodeElements :: [Node] -> [Node]
filterNodeElements = filter (\n -> case n of NodeElement _ -> True; _ -> False)

-- Access inner `Element' values, and fail if something else is encountered.
unwrapElement :: Node -> Element
unwrapElement (NodeElement e) = e
unwrapElement _ = error "unwrapElement: Non-element Node value encountered."

-- xpBool should unpickle to `True' for "true".
xpBoolUnpickleTrue :: Bool
xpBoolUnpickleTrue = case unpickle xpBool "true" of
  Right True -> True
  _ -> False

-- xpBool should unpickle to `False' for "false".
xpBoolUnpickleFalse :: Bool
xpBoolUnpickleFalse = case unpickle xpBool "false" of
  Right False -> True
  _ -> False

-- xpBool should unpickle to `True' for "1".
xpBoolUnpickle1 :: Bool
xpBoolUnpickle1 = case unpickle xpBool "1" of
  Right True -> True
  _ -> False

-- xpBool should unpickle to `False' for "0".
xpBoolUnpickle0 :: Bool
xpBoolUnpickle0 = case unpickle xpBool "0" of
  Right False -> True
  _ -> False

-- xpBool should pickle to "true" for `True'.
xpBoolPickleTrue :: Bool
xpBoolPickleTrue = pickle xpBool True == "true"

-- `xpBool' should pickle to "false" for `False'.
xpBoolPickleFalse :: Bool
xpBoolPickleFalse = pickle xpBool False == "false"

-- `xpBool' should fail for any string that is not "true", "1", "false" or "0".
prop_xpBool :: Text -> Property
prop_xpBool t = and [ t /= "true"
                    , t /= "1"
                    , t /= "false"
                    , t /= "0" ] ==> case unpickle xpBool t of
  Right _ -> False
  Left _ -> True

-- Verify that arbitrarily splitting up `ContentText' values does not change
-- the resulting list of nodes.
prop_flattenContent :: [(Node, Content)] -> Property
prop_flattenContent ncs =
  hasContent (map fst ncs) ==>
    let combined = map (\(n, c) -> case (n, c) of
                           (NodeContent (ContentText t), ContentText t')->
                             NodeContent (ContentText (Text.concat [t, t']))
                           (NodeContent (ContentEntity _), _) ->
                             error "Content entities not supported!"
                           _ -> n) ncs
        split = concat $
                  map (\(n, c) -> case (n, c) of
                          (NodeContent (ContentText t), ContentText t') ->
                            [ NodeContent (ContentText t)
                            , NodeContent (ContentText t') ]
                          (NodeContent _, _) ->
                            error "Content entities not supported!"
                          _ -> [n]) ncs
    in (flattenContent combined) == (flattenContent split)
  where
    hasContent :: [Node] -> Bool
    hasContent [] = False
    hasContent ((NodeContent _):_) = True
    hasContent (_:ns) = hasContent ns

-- Get the `Text' value based on a list of the `Content' values.
contentToText :: [Content] -> Text
contentToText = Text.concat . map contentToText_

-- Unwrap `ContentText' values. Fail on `ContentEntity' values, as they are
-- not supported.
contentToText_ :: Content -> Text
contentToText_ (ContentText t) = t
contentToText_ (ContentEntity _) = error "Content entities not supported!"

-- escape :: Text -> Text
-- escape = Text.replace "&" "&amp;" . Text.replace "'" "&apos;" .
--            Text.replace ">" "&gt;" . Text.replace "<" "&lt;" .
--              Text.replace "\"" "&quot;"

-- Given a list of nodes which has elements containing only character data and
-- comments (yet has at least a CDATA length of 1), and verifies that the
-- content of those elements can be accessed properly.
-- prop_xpElemTextUnpickle :: [Element] -> Property
-- prop_xpElemTextUnpickle elems = undefined

-- main = flip defaultMainWithOpts (mempty {ropt_color_mode = Just ColorNever}) $
--   [ testGroup "xpUnit" [ testProperty "xpUnit1" testxpUnit1
--                        , testProperty "xpUnit2" testxpUnit2
--                        ]
--   , testGroup "xpId"        [ testProperty "xpId" testxpId]
--   , testGroup "combinators" [ testProperty "xpIsolate" testxpIsolate
--                             , testProperty "xpIsolatePair" testxpIsolatePair
--                             , testProperty "pair leniency" testPairLenient
--                             ]
--   ]

-- testxpUnit1 xs  = pickle xpUnit () == ([] :: [Int])
--     where types = xs :: Int

-- testxpUnit2 xs  = unpickle xpUnit xs == Right ()
--     where types = xs :: [Int]

-- testxpId xs = null xs .||. (unpickle xpId xs == Right xs)
--     where types = xs :: [Int]

-- testxpIsolate xs = null xs .||.
--                      (unpickle (xpIsolate xpId) xs == Right (take 1 xs))
--       where types = xs :: [Int]

-- testxpIsolatePair xs = null xs .||.
--                      (unpickle (xpPair (xpIsolate xpId)
--                                        (xpId)
--                                ) xs == Right (take 1 xs, drop 1 xs))
--       where types = xs :: [Int]

-- testPairLenient xs = unpickle (xpId <++> xpUnit) xs == Right (xs, ())
--       where types = xs :: [Int]

-- el x = Element "foo" [("bar",(return . ContentText  $ "blub" `Text.append` x))] []
-- el' xs = Element "parent" [] $ flip map xs (\x -> NodeElement $ el (Text.pack $ show x))

-- doc = el' [1..5]

-- testDoc = xpRoot $ xpElem "parent" xpUnit (xpAll $ xpElem "foo" xpId xpUnit)
