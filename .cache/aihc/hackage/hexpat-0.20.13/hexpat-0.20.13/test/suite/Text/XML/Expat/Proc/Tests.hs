{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module Text.XML.Expat.Proc.Tests (tests) where

import           Data.Maybe
import           Test.Framework (Test)
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Text.XML.Expat.Tests
import           Text.XML.Expat.Proc
import           Text.XML.Expat.Tree


tests :: [Test]
tests = [ testProperty "onlyElems"       prop_onlyElems
        , testProperty "onlyText"        prop_onlyText
        , testProperty "findChildren"    prop_findChildren
        , testProperty "findChildren2"   prop_findChildren2
        , testProperty "findChildren3"   prop_findChildren3
        , testProperty "filterChildren"  prop_filterChildren
        , testProperty "findChild"       prop_findChild
        , testProperty "filterElements1" prop_filterElements1
        , testProperty "filterElements2" prop_filterElements2
        , testProperty "filterElements3" prop_filterElements3
        , testProperty "filterElements4" prop_filterElements4
        , testProperty "others"          prop_others
        ]



prop_onlyElems :: [TNode] -> Bool
prop_onlyElems nodes = all isElement els
  where
    els = onlyElems nodes


prop_onlyText :: [TNode] -> Bool
prop_onlyText nodes = all isAText txts
  where
    txts     = onlyText nodes
    isAText b = elem b testTextSet



prop_findChildren :: TNode -> Bool
prop_findChildren node = all p ch
  where
    ch = findChildren "banana" node

    p (Text _)         = False
    p (Element nm _ _) = nm == "banana"


prop_findChildren2 :: Bool
prop_findChildren2 = ch == [child1]
  where
    child1 :: TNode
    child1 = Element "banana" [] []

    child2 :: TNode
    child2 = Element "rhubarb" [] []

    node :: TNode
    node   = Element "root" [] [child1, child2]
    ch     = findChildren "banana" node


prop_findChildren3 :: Bool
prop_findChildren3 = null ch
  where
    child :: TNode
    child = Text "foo"

    !ch = findChildren "banana" child


prop_filterChildren :: TNode -> Bool
prop_filterChildren node = all p ch
  where
    ch = filterChildrenName (=="banana") node

    p (Text _)         = False
    p (Element nm _ _) = nm == "banana"


prop_findChild :: TNode -> Property
prop_findChild node' = isElement node' ==> r == (Just child)
  where
    child :: TNode
    child = Element "tag" [] []

    node = node' { eChildren = child:(eChildren node') }

    r = findChild "tag" node


-- test positive case
prop_filterElements1 :: TNode -> Bool
prop_filterElements1 n@(Text _)         = filterElements isText n == [n]
prop_filterElements1 n@(Element nm _ _) = filterElements f n == [n]
  where
    f = isNamed nm

-- test that all results obey the predicate
prop_filterElements2 :: TNode -> Bool
prop_filterElements2 n = p1 && p2
  where
    l1 = filterElements isText n
    l2 = filterElements isElement n
    p1 = all isText l1
    p2 = all isElement l2

-- test that we grab all elements
prop_filterElements3 :: TNode -> Bool
prop_filterElements3 n = p1 && p2
  where
    l1 = filterElements isText n
    l2 = filterElements isElement n
    p1 = all isText l1
    p2 = all isElement l2


-- test that all children match & that we don't recurse into matching children
prop_filterElements4 :: Property
prop_filterElements4 = forAll gen $ \node ->
                         let ch = getChildren node
                         in ch == f node && ch == g node
  where
    gen = do
        let node = Element "banana" [] [] :: TNode
        let node'= Element "banana" [] [node] :: TNode
        n       <- choose(0,5)
        let l    = node':(replicate n node)
        return $ Element "root" [] l

    f = filterElements (isNamed "banana")
    g = filterElementsName (=="banana")


-- other functions are all trivial, this property just gives us code coverage
prop_others :: Bool
prop_others = and [p1, p2, p3, p4]
  where
    child1 :: TNode
    child1 = Element "banana" [] []

    child2 :: TNode
    child2 = Element "rhubarb" [] []

    node :: TNode
    node   = Element "root" [] [child1, child2]

    fc1 = filterChild (isNamed "rhubarb") node
    fc2 = filterChildName (=="rhubarb") node
    fc3 = findElement "banana" node
    fc4 = filterElement (isNamed "root") node
    fc5 = filterChild (isNamed "root") node
    fc6 = filterElementName (=="root") node

    p1 = all isJust [fc1, fc2, fc3, fc4, fc6]
    p2 = all (not . isJust) [fc5]
    p3 = findElements "banana" node == [child1]
    p4 = filterElementsName (=="root") foo == []

    foo :: TNode
    foo = Text "foo"
