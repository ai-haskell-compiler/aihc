{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Main(main) where

import           Data.Bifunctor
import           Data.Bimatchable
import           Data.Functor.Classes
import           Data.Matchable

import qualified Data.Map             as Map

import           GHC.Generics         (Generic1)
import           GHC.Generics.Generically (Generically1(..))

import           Test.Hspec

main :: IO ()
main = hspec $
  do context "Matchable []" $ do
       runIO $ do
         p $ "list1 = " ++ show list1
         p $ "list2 = " ++ show list2
         p $ "list3 = " ++ show list3
         p   "list4 = repeat 0"
       specify "zipMatch list1 list2 = Just [(0,3), (1,4), (2,5)]" $
         zipMatch list1 list2 `shouldBe` Just [(0,3), (1,4), (2,5)]
       specify "zipMatch list1 list3 = Nothing" $
         zipMatch list1 list3 `shouldBe` Nothing
       specify "zipMatch list1 list4 = Nothing" $
         zipMatch list1 list4 `shouldBe` Nothing
     context "Matchable (Either String)" $ do
       runIO $ do
         p $ "either1 = " ++ show either1
         p $ "either2 = " ++ show either2
         p $ "either3 = " ++ show either3
         p $ "either4 = " ++ show either4
       specify "zipMatch either1 either2 = Just (Right (1,2))" $
         zipMatch either1 either2 `shouldBe` Just (Right (1,2))
       specify "zipMatch either1 either3 = Nothing" $
         zipMatch either1 either3 `shouldBe` Nothing
       specify "zipMatch either3 either3 = Just (Left \"foo\")" $
         zipMatch either3 either3 `shouldBe` Just (Left "foo")
       specify "zipMatch either3 either4 = Nothing" $
         zipMatch either3 either4 `shouldBe` Nothing
     context "Bimatchable Either" $ do
       specify "bizipMatch either1 either2 = Just (Right (1,2))" $
         bizipMatch either1 either2 `shouldBe` Just (Right (1,2))
       specify "bizipMatch either1 either3 = Nothing" $
         bizipMatch either1 either3 `shouldBe` Nothing
       specify "bizipMatch either3 either4 = Just (Left (\"foo\", \"bar\"))" $
         bizipMatch either3 either4 `shouldBe` Just (Left ("foo", "bar"))
     context "Matchable ((,) Char)" $ do
       runIO $ do
         p $ "pair1 = " ++ show pair1
         p $ "pair2 = " ++ show pair2
         p $ "pair3 = " ++ show pair3
         p $ "pair4 = " ++ show pair4
       specify "zipMatch pair1 pair2 = Just ('a', (1,2))" $
         zipMatch pair1 pair2 `shouldBe` Just ('a', (1,2))
       specify "zipMatch pair3 pair4 = Just ('b', (3,4))" $
         zipMatch pair3 pair4 `shouldBe` Just ('b', (3,4))
       specify "zipMatch pair1 pair3 = Nothing" $
         zipMatch pair1 pair3 `shouldBe` Nothing
     context "Bimatchable (,)" $ do
       specify "bizipMatch pair1 pair3 = Just (('a','b'),(1,3))" $
         bizipMatch pair1 pair3 `shouldBe` Just (('a','b'),(1,3))
     context "Matchable (Map Char)" $ do
       runIO $ do
         p $ "map1 = " ++ show map1
         p $ "map2 = " ++ show map2
         p $ "map3 = " ++ show map3
       specify "zipMatch map1 map2 = Just (Map.fromList [('a', (1,2)), ('b', (3,4))])" $
         zipMatch map1 map2 `shouldBe` Just (Map.fromList [('a', (1,2)), ('b', (3,4))])
       specify "zipMatch map1 map3 = Nothing" $
         zipMatch map1 map3 `shouldBe` Nothing
     context "Matchable (MyTree Int)" $ do
       runIO $ do
         p $ "myTree1 = " ++ show myTree1
         p $ "myTree2 = " ++ show myTree2
         p $ "myTree3 = " ++ show myTree3
         p $ "myTree4 = " ++ show myTree4
       specify "zipMatch myTree1 myTree2 = Just _" $
         zipMatch myTree1 myTree2 `shouldBe` Just (Node 0 ("foo",200) Empty (Node 1 ("bar", 300) Empty Empty))
       specify "zipMatch myTree1 myTree3 = Nothing" $
         zipMatch myTree1 myTree3 `shouldBe` Nothing
       specify "zipMatch myTree1 myTree4 = Nothing" $
         zipMatch myTree1 myTree4 `shouldBe` Nothing
     context "Bimatchable MyTree" $ do
       specify "bizipMatch myTree1 myTree3 = Just _" $
         bizipMatch myTree1 myTree3 `shouldBe` Just (Node (0,3) ("foo",True) Empty (Node (1,4) ("bar", False) Empty Empty))
       specify "bizipMatch myTree1 myTree4 = Nothing" $
         bizipMatch myTree1 myTree4 `shouldBe` Nothing
  where p = putStrLn

list1, list2, list3, list4 :: [Int]
list1 = [0,1,2]
list2 = [3,4,5]
list3 = [0,1]
list4 = repeat 0

either1, either2, either3, either4 :: Either String Int
either1 = Right 1
either2 = Right 2
either3 = Left "foo"
either4 = Left "bar"

pair1, pair2, pair3, pair4 :: (Char, Int)
pair1 = ('a', 1)
pair2 = ('a', 2)
pair3 = ('b', 3)
pair4 = ('b', 4)

map1, map2, map3 :: Map.Map Char Int
map1 = Map.fromList [pair1, pair3]
map2 = Map.fromList [pair2, pair4]
map3 = Map.fromList [pair1]

data MyTree k a = Empty | Node k a (MyTree k a) (MyTree k a)
  deriving stock (Show, Read, Eq, Ord, Functor, Generic1)
  deriving (Eq1, Matchable) via (Generically1 (MyTree k))

myTree1 :: MyTree Int String
myTree1 = Node 0 "foo" Empty (Node 1 "bar" Empty Empty)

myTree2 :: MyTree Int Int
myTree2 = Node 0 200 Empty (Node 1 300 Empty Empty)

myTree3 :: MyTree Int Bool
myTree3 = Node 3 True Empty (Node 4 False Empty Empty)

myTree4 :: MyTree Int Char
myTree4 = Node 0 'a' Empty Empty


instance Bifunctor MyTree where
  bimap _ _ Empty          = Empty
  bimap f g (Node k a l r) = Node (f k) (g a) (bimap f g l) (bimap f g r)

instance Eq2 MyTree where
  liftEq2 = liftEq2Default

instance Bimatchable MyTree where
  bizipMatchWith _ _ Empty Empty = Just Empty
  bizipMatchWith f g (Node k a l r) (Node k' a' l' r') =
    Node <$> f k k'
         <*> g a a'
         <*> bizipMatchWith f g l l'
         <*> bizipMatchWith f g r r'
  bizipMatchWith _ _ _ _ = Nothing

