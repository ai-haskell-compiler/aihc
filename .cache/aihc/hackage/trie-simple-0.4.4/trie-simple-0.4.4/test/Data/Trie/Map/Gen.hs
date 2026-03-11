module Data.Trie.Map.Gen(
  C(..),
  TMap'(..),
  TMap''(..),
  genTMap,
  validTMap
) where

import           Test.QuickCheck hiding (shrinkMapBy)

import           Data.Maybe
import qualified Data.Map as Map

import           Data.Trie.Map
import           Data.Trie.Map.Internal
import           Common

newtype TMap' = TMap' (TMap C Int)

instance Show TMap' where
  show (TMap' t) = show t

instance Arbitrary TMap' where
  arbitrary = TMap' <$> genTMap 
  shrink (TMap' t) = TMap' <$> shrinkTMap t

newtype TMap'' = TMap'' (TMap B Int)

instance Show TMap'' where
  show (TMap'' t) = show t

instance Arbitrary TMap'' where
  arbitrary = TMap'' <$> genTMap 
  shrink (TMap'' t) = TMap'' <$> shrinkTMap t

genTMap :: (Ord c, Arbitrary c, Arbitrary a) => Gen (TMap c a)
genTMap = fromList <$> arbitrary

shrinkTMap :: (Ord c, Arbitrary c, Arbitrary a) => TMap c a -> [TMap c a]
shrinkTMap (TMap (Node ma e)) = filter validTMap $ 
  [ TMap (Node Nothing e) | Just _ <- [ma] ] ++
  [ TMap (Node (Just a') e) | Just a <- [ma], a' <- shrink a ] ++
  [ TMap (Node ma e') | e' <- shrinkMapBy shrinkTMap e ]

validTMap :: TMap c a -> Bool
validTMap = snd . foldTMap step
  where
    step (Node ma e) =
      let isEmpty = isNothing ma && all fst e
          isValid = (not isEmpty || Map.null e) && all snd e
      in (isEmpty, isValid)
