{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings #-}

module Text.XML.Expat.Cursor.Tests (tests) where

import           Control.Monad (replicateM)
import           Data.Maybe
import           Test.Framework (Test)
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Text.XML.Expat.Tests
import           Text.XML.Expat.Cursor
import           Text.XML.Expat.Tree


tests :: [Test]
tests = [ testProperty "invertible"        prop_invertible
        , testProperty "invertible2"       prop_invertible2
        , testProperty "fromTag"           prop_fromTag
        , testProperty "fromForest"        prop_fromForest
        , testProperty "firstChild"        prop_firstChild
        , testProperty "firstChild2"       prop_firstChild2
        , testProperty "downUp"            prop_downUp
        , testProperty "leftRight"         prop_leftRight
        , testProperty "root"              prop_root
        , testProperty "lastChild"         prop_lastChild
        , testProperty "findLeft"          prop_findLeft 
        , testProperty "findRight"         prop_findRight
        , testProperty "findChild"         prop_findChild
        , testProperty "findChild2"        prop_findChild2
        , testProperty "nextDF1"           prop_nextDF1
        , testProperty "nextDF2"           prop_nextDF2
        , testProperty "nextDF3"           prop_nextDF3
        , testProperty "findRec"           prop_findRec
        , testProperty "getNodeIndex"      prop_getNodeIndex
        , testProperty "emptyChild"        prop_emptyChild
        , testProperty "negativeChild"     prop_negativeChild
        , testProperty "isChild"           prop_isChild
        , testProperty "modifyContent"     prop_modifyContent
        , testProperty "modifyContentList" prop_modifyContentList
        , testProperty "insertChildren"    prop_insertChildren
        , testProperty "insertLeftRight"   prop_insertLeftRight
        , testProperty "removeLeftRight"   prop_removeLeftRight
        , testProperty "insertGo"          prop_insertGo
        , testProperty "removeGo"          prop_removeGo
       ]


------------------------------------------------------------------------------
satisfy :: (Arbitrary a) =>
           (a -> Bool)          -- ^ predicate that generated values must
                                -- satisfy
        -> Gen a                -- ^ generator
        -> Gen a
satisfy f g = do
    x <- arbitrary
    if f x then return x else satisfy f g


currentsEq :: TCursor -> TCursor -> Bool
currentsEq a b = current a == current b


someNodes :: Gen [TNode]
someNodes = do
    n     <- choose (3::Int, 8::Int)
    replicateM n arbitrary

const' :: a -> b -> a
const' x y = y `seq` x


allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame xs = and $ map (uncurry (==)) (xs `zip` tail xs)


------------------------------------------------------------------------------

prop_invertible :: TNode -> Bool
prop_invertible n = p
  where
    p = toTree (fromTree n) == n

prop_invertible2 :: [TNode] -> Property
prop_invertible2 n = not (null n) ==> p
  where
    p = toForest (fromJust $ fromForest n :: TCursor) == n


-- this is stupid because the function is so trivial, but I lust after the
-- green bar
prop_fromTag :: TNode -> Property
prop_fromTag n = isElement n ==> fromTag (getTag n) (eChildren n) == n


prop_fromForest :: Bool
prop_fromForest = isNothing (fromForest [] :: Maybe TCursor)

prop_firstChild :: TNode -> Property
prop_firstChild node = isElement node ==> p1 && p2
  where
    child1 :: TNode
    child1 = Element "gryphon" [] []

    node' = node { eChildren= child1:(eChildren node) }

    mbfc = do
        c <- firstChild $ fromTree node'
        return $ current c

    p1 = isJust mbfc
    p2 = maybe False (== child1) mbfc


prop_firstChild2 :: Bool
prop_firstChild2 = (isNothing $ firstChild c) && (isNothing $ firstChild c2)
  where
    node :: TNode
    node = Element "root" [] []

    txt :: TNode
    txt = Text ""

    c = fromTree node
    c2 = fromTree txt


prop_downUp :: TNode -> Property
prop_downUp node = isElement node && (not $ null $ eChildren node) ==> p
  where
    p = p1 && p2

    cursor = fromTree node

    p1 = isNothing $ parent cursor

    m = do
        cur' <- firstChild cursor
        pa   <- parent cur'

        return $ current pa

    p2 = maybe False (== node) m


prop_leftRight :: Property
prop_leftRight = forAll gen p
  where
    gen :: Gen (TNode,TNode,TNode)
    gen = do
        ch1  <- arbitrary
        ch2  <- arbitrary
        chN1 <- arbitrary
        chN  <- arbitrary
        n    <- satisfy isElement (arbitrary :: Gen TNode)

        let n' = n {eChildren = ([ch1,ch2] ++ (eChildren n) ++ [chN1,chN])}

        return (n', ch1, chN)

    p :: (TNode, TNode, TNode) -> Bool
    p (node, ch1, chN) = p1 && p2
      where
        cursor = fromTree node

        m1 = do
            curFirst <- firstChild cursor
            curLast  <- lastChild cursor

            let f = current curFirst
            let l = current curLast

            return $ (f == ch1) && (l == chN)

        p1 = fromMaybe False m1


        m2 = do
            curFirst <- firstChild cursor
            curLast  <- lastChild cursor
            l        <- left curLast >>= left
            r        <- right l >>= right

            a        <- right curFirst >>= right
            b        <- left a >>= left

            let bad1 =  left curFirst
            let bad2 =  right curLast

            let lch = current curLast
            let x   = current r

            let fch = current curFirst
            let y   = current b

            return (x == lch && y == fch && isNothing bad1 && isNothing bad2)

        p2 = fromMaybe False m2


prop_root :: Property
prop_root = forAll gen f
  where
    gen :: Gen (TNode,TNode,TNode)
    gen = do
        ch1' <- satisfy isElement arbitrary
        ch2  <- arbitrary
        let ch1 = ch1' { eChildren = ch2:(eChildren ch1') }
        n    <- satisfy isElement (arbitrary :: Gen TNode)

        let n' = n {eChildren = ch1:(eChildren n)}

        return (n',ch1,ch2)


    f (n,ch1,ch2) = do
        fromMaybe False m
      where
        m = do c1 <- firstChild $ fromTree n
               c2 <- firstChild c1
               let r = root c2

               return $ and [ current r == n
                            , current c1 == ch1
                            , current c2 == ch2 ]

prop_lastChild :: TNode -> Property
prop_lastChild n' = isElement n' ==> p
  where
    n  = n' { eChildren=[] }
    c  = fromTree n
    n2 = n { eChildren=[n'] }
    c2 = fromTree n2
    p  = p1 && p2
    p1 = isNothing $ lastChild c
    mc = lastChild c2 >>= parent
    p2 = maybe False (\x -> toTree x == n2) mc




prop_findLeft :: Property
prop_findLeft = forAll gen f
  where
    gen :: Gen (TCursor,TCursor)
    gen = do
        nodes <- someNodes

        let ch = Element "halibut" [] []

        let node = Element "root" [] (ch:nodes)

        i <- choose (1,length nodes)

        let cn = fromTree node

        let c1 = fromMaybe (error "impossible") (getChild i cn)
        let c2 = fromMaybe (error "impossible") (firstChild cn)

        return (c1, c2)


    f :: (TCursor,TCursor) -> Bool
    f (c,c') = maybe False (currentsEq c') mbC
      where
        mbC = findLeft (\x -> isNamed "halibut" $ current x) c


prop_findRight :: Property
prop_findRight = forAll gen f
  where
    gen :: Gen (TCursor,TCursor)
    gen = do
        n     <- choose (3::Int, 8::Int)
        i     <- choose (0,n-1)
        nodes <- replicateM n arbitrary

        let ch = Element "halibut" [] []

        let node = Element "root" [] (nodes ++ [ch])

        let cn = fromTree node

        let c1 = fromMaybe (error "impossible") (getChild i cn)
        let c2 = fromMaybe (error "impossible") (lastChild cn)

        return (c1, c2)


    f :: (TCursor,TCursor) -> Bool
    f (c,c') = maybe False (currentsEq c') mbC
      where
        mbC = findRight (\x -> isNamed "halibut" $ current x) c


prop_findChild :: Property
prop_findChild = forAll gen f
  where
    gen :: Gen (TCursor,TCursor)
    gen = do
        nodes <- someNodes

        let ch = Element "halibut" [] []
        let n = length nodes

        let node = Element "root" [] (nodes ++ [ch] ++ nodes ++ [ch])

        let cn = fromTree node
        let c1 = fromMaybe (error "impossible") (getChild n cn)

        return (cn, c1)


    f :: (TCursor,TCursor) -> Bool
    f (c,c') = maybe False (currentsEq c') mbC
      where
        mbC = findChild (\x -> isNamed "halibut" $ current x) c


prop_findChild2 :: Property
prop_findChild2 = forAll gen f
  where
    gen :: Gen (TCursor,TCursor)
    gen = do
        nodes <- someNodes

        let ch = Element "halibut" [] []

        let node = Element "root" [] (ch:nodes)

        let cn = fromTree node
        let c1 = fromMaybe (error "impossible") (firstChild cn)

        return (cn, c1)


    f :: (TCursor,TCursor) -> Bool
    f (c,c') = maybe False (currentsEq c') mbC
      where
        mbC = findChild (\x -> isNamed "halibut" $ current x) c


prop_nextDF1 :: Property
prop_nextDF1 = forAll gen $ uncurry currentsEq
  where
    gen :: Gen (TCursor, TCursor)
    gen = do
        nodes <- someNodes
        let ch = Element "halibut" [] []
        let node = Element "root" [] (ch:nodes)

        let cn = fromJust $ fromForest [node]
        let c1 = fromJust (firstChild cn)
        let c2 = fromJust (nextDF cn)

        return (c1, c2)

prop_nextDF2 :: Property
prop_nextDF2 = forAll gen $ uncurry currentsEq
  where
    gen :: Gen (TCursor, TCursor)
    gen = do
        nodes <- someNodes
        let ch = Element "halibut" [] []
        let node = Element "root" [] (ch:nodes)

        let cn = fromJust $ fromForest [node]
        let cc = fromJust (firstChild cn)
        let c1 = fromJust (nextDF cc)
        let c2 = fromJust (right cc)

        return (c1, c2)

prop_nextDF3 :: Property
prop_nextDF3 = forAll gen $ uncurry currentsEq
  where
    gen :: Gen (TCursor, TCursor)
    gen = do
        nodes <- someNodes
        let ch = Element "halibut" [] []
        let ch2 = Element "pike" [] []
        let node1 = Element "subtree1" [] [ch]
        let node2 = Element "subtree2" [] (ch2:nodes)
        let node = Element "root" [] [node1, node2]

        let cn = fromJust $ fromForest [node] :: TCursor
        let cc = fromJust (firstChild cn >>= firstChild)
        let c1 = fromJust $ nextDF cc
        let c2 = fromJust (firstChild cn >>= right)

        return (c1, c2)


prop_findRec :: Property
prop_findRec = forAll gen $ uncurry currentsEq
  where
    gen :: Gen (TCursor, TCursor)
    gen = do
        nodes <- someNodes
        let ch = Element "halibut" [] []
        let ch2 = Element "pike" [] []
        let node1 = Element "subtree1" [] [ch]
        let node2 = Element "subtree2" [] (ch2:nodes)
        let node = Element "root" [] [node1, node2]

        let cn = fromTree node
        let c1 = fromJust (firstChild cn >>= right >>= firstChild)
        let c2 = fromJust $ findRec (isNamed "pike" . current) cn

        return (c1, c2)


prop_emptyChild :: Bool
prop_emptyChild = isNothing m
  where
    tree :: TNode
    tree = Element "root" [] []
    m    = getChild 0 $ fromTree tree


prop_negativeChild :: Property
prop_negativeChild = forAll gen f
  where
    gen = satisfy isElement (arbitrary :: Gen TNode)

    f node = isNothing $ getChild (-1) (fromTree node)


prop_getNodeIndex :: Property
prop_getNodeIndex = forAll gen $ uncurry (==)
  where
    gen :: Gen (Int, Int)
    gen = do
        nodes <- replicateM 10 arbitrary
        i     <- choose (0,9)

        let node = (Element "root" [] nodes)::TNode

        let cn = fromTree node
        let c1 = fromJust $ getChild i cn
        let j  = getNodeIndex c1

        return (i,j)


prop_isChild :: TNode -> Bool
prop_isChild n = isChild c && hasChildren r && isFirst r && isLast c
  where
    node = Element "root" [] [n]
    r    = fromTree node
    c    = fromJust $ firstChild r


prop_modifyContent :: Property
prop_modifyContent = forAll gen allSame
  where
    gen :: Gen [TNode]
    gen = do
        nodes <- replicateM 10 arbitrary

        let n1 = Element "apple" [] []
        let n2 = Element "banana" [] []

        let tree1 = Element "root" [] (n1:nodes)
        let tree2 = Element "root" [] (n2:nodes)

        let c = fromJust . firstChild . fromTree $ tree1
        let c1 = modifyContent (const' n2) c
        c2 <- modifyContentM (const' $ return n2) c

        let tree3 = toTree c1
        let tree4 = toTree c2
        return [tree2, tree3, tree4]


prop_modifyContentList :: Property
prop_modifyContentList = forAll gen $ uncurry (==)
  where
    gen :: Gen (TNode,TNode)
    gen = do
        nodes <- replicateM 10 arbitrary

        let n = Element "apple" [] []

        let tree1 = Element "root" [] [n]
        let tree2 = Element "root" [] nodes

        let c = fromJust . firstChild . fromTree $ tree1
        let c1 = fromJust $ modifyContentList (const' nodes) c

        let treeResult = toTree c1
        return (tree2, treeResult)


prop_insertChildren :: [TNode] -> Bool
prop_insertChildren ns = isNothing m1 && tree2 == tree3
  where
    tree = Element "root" [] ns
    n1 = Element "alpha" [] []
    n2 = Element "omega" [] []
    n3 = Element "beta" [] []
    n4 = Element "gamma" [] []

    tree2 = Element "root" [] $ concat [[n1,n2], ns, [n3,n4]]

    txt :: TNode
    txt = Text "foo"

    m1 = insertFirstChild n1 $ fromTree txt

    top = fromTree tree

    tree3 = fromJust $ do
                c1 <- insertFirstChild n2 top
                c2 <- insertLastChild n3 c1
                c3 <- insertManyFirstChild [n1] c2
                c4 <- insertManyLastChild [n4] c3

                return $ toTree c4


prop_insertLeftRight :: (TNode,TNode) -> Bool
prop_insertLeftRight (n,n') = f == [n1, n', n, n2]
  where
    n1 = Element "alpha" [] []
    n2 = Element "omega" [] []
    c = insertRight n2 $ insertManyLeft [n'] $ insertLeft n1 $ fromTree n

    f = toForest c


prop_removeLeftRight :: [TNode] -> Property
prop_removeLeftRight ns = not (null ns) ==> p1 && p2 && p3
  where
    n1      = Element "alpha" [] []
    n2      = Element "omega" [] []

    tree1   = Element "root" [] (n1:(ns ++ [n2]))
    tree2   = Element "root" [] ns

    c1      = fromJust $ firstChild (fromTree tree1)
    m1      = removeLeft c1

    c2      = fromJust $ right c1
    (x1,c3) = fromJust $ removeLeft c2

    c4      = fromJust (parent c3 >>= lastChild)
    m4      = removeRight c4

    c5      = fromJust $ left c4
    (x2,c6) = fromJust $ removeRight c5

    tree3   = toTree c6

    p1      = tree2 == tree3
    p2      = isNothing m1 && isNothing m4
    p3      = n1 == x1 && n2 == x2


prop_insertGo :: [TNode] -> Property
prop_insertGo ns = not (null ns) ==> p1
  where
    n1      = Element "alpha" [] []
    n2      = Element "omega" [] []

    tree1   = Element "root" [] ns
    tree2   = Element "root" [] $ [n1,n2] ++ ns

    c1      = fromJust $ firstChild (fromTree tree1)
    c2      = insertGoLeft n1 c1
    c3      = insertGoRight n2 c2

    tree3   = toTree c3

    p1      = tree2 == tree3


prop_removeGo :: [TNode] -> Property
prop_removeGo ns = not (null ns) ==> p1 && p2 && p3
  where
    n1      = Element "alpha" [] []
    n2      = Element "omega" [] []

    tree1   = Element "root" [] $ [n1,n2] ++ ns
    tree2   = Element "root" [] ns

    top     = fromTree tree1

    c1      = fromJust $ firstChild top
    c2      = fromJust $ lastChild top
    m1      = removeGoLeft c1
    m2      = removeGoRight c2
    m3      = removeGoUp top

    c3      = fromJust $ right c1
    c4      = fromJust $ removeGoLeft c3

    n3      = current c4

    c5      = fromJust $ removeGoRight c4
    c6      = fromJust $ removeGoUp c4

    m4      = left c6
    m5      = right c6

    tree3   = toTree c5
    tree4   = toTree c6

    p1      = and $ map isNothing [m1,m2,m3,m4,m5]
    p2      = tree2 == tree3 && tree2 == tree4
    p3      = n1 == n3
