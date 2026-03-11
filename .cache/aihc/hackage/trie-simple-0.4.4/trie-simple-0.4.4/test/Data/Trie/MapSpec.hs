{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use curry" #-}
module Data.Trie.MapSpec(
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Foldable as F
import           Data.List (sortBy, foldl', inits)
import           Data.Ord
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Semigroup

import           Data.Trie.Map     as T
import           Data.Trie.Map.Gen
import qualified Data.Trie.Set     as TSet
import           Data.Trie.Set.Gen

import           Data.Bits ((.|.))

op :: Int -> Int -> Int
op x y = 1 + 2 * x - 3 * y - (x .|. y)

spec :: Spec
spec = do
  specify "toList empty = []" $
    toList (empty :: TMap C Int) `shouldSatisfy` Prelude.null
  specify "toList (just a) = [([], a)]" $
    property $ \a -> toList (just a :: TMap C Int) === [([], a)]
  specify "toList (singleton k a) = [(k,a)]" $
    property $ \k a -> toList (singleton k a :: TMap C Int) === [(k,a)]
  specify "toAscList . fromList = Map.toAscList . Map.fromList" $
    property $ \strs -> toAscList (fromList strs :: TMap C Int) ==
                        (Map.toAscList . Map.fromList) strs
  specify "toAscList . fromListWith op = Map.toAscList . Map.fromListWith op" $
    property $ \strs -> toAscList (fromListWith op strs :: TMap C Int) ==
                        (Map.toAscList . Map.fromListWith op) strs
  specify "fromAscList . sortBy (comparing fst) = fromList" $
    property $ \strs -> fromAscList (sortBy (comparing fst) strs) ==
                        (fromList strs :: TMap C Int)
  specify "fromAscListWith op . sortBy (comparing fst) = fromListWith op" $
    property $ \strs -> fromAscListWith op (sortBy (comparing fst) strs) ==
                        (fromListWith op strs :: TMap C Int)
  specify "fromMap . Map.fromList = fromList" $
    property $ \strs -> fromMap (Map.fromList strs) ==
                        (fromList strs :: TMap C Int)
  specify "null . toList = null" $
    property $ \(TMap' t) -> Prelude.null (toList t) === T.null t
  specify "length . toList = count" $
    property $ \(TMap' t) -> length (toList t) === count t
  specify "keys = map fst . toList" $
    property $ \(TMap' t) -> keys t == map fst (toList t)
  specify "elems = map snd . toList" $
    property $ \(TMap' t) -> elems t == map snd (toList t)
  specify "keysTSet = TSet.fromAscList . keys" $
    property $ \(TMap' t) -> keysTSet t == TSet.fromAscList (keys t)
  specify "fromTSet id = fromAscList . map (\\k -> (k,k)) . TSet.toAscList" $
    property $ \(TSet' t) ->
      fromTSet id t == fromAscList (map (\k -> (k,k)) (TSet.toAscList t))
  
  specify "member k t = (k `Map.member` toMap t)" $
    property $ \(TMap'' t) ->
      let strMap = toMap t
      in property $ \str -> member str t == Map.member str strMap
  specify "lookup k t = (k `Map.lookup` toMap t)" $
    property $ \(TMap'' t) ->
      let strMap = toMap t
      in property $ \str -> T.lookup str t == Map.lookup str strMap
  specify "lookupPrefixes k t = [ (prek, v) | prek <- inits k, v <- toList $ Map.lookup prek (toMap t) ]" $
    property $ \(TMap'' t) ->
      let strMap = toMap t
          slow str = [ (prek, v) | prek <- inits str, v <- F.toList $ Map.lookup prek strMap ]
      in property $ \str -> T.lookupPrefixes str t == slow str
  specify "lookup (k ++ l) t == lookup l (snd (match k t))" $
    property $ \k l (TMap'' t) ->
      T.lookup (k ++ l) t == T.lookup l (snd (T.match k t))

  specify "toMap (insert k 1 a) = Map.insert k 1 (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (insert k 1 a) == Map.insert k 1 (toMap a)
  specify "toMap (delete k a) = Map.delete k (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (delete k a) == Map.delete k (toMap a)
  specify "toMap (insertWith op k v a) = Map.insertWith op k v (toMap a)" $
    property $ \k v (TMap'' a) ->
      toMap (insertWith op k v a) ==
      Map.insertWith op k v (toMap a)
  let sub b a = if a < b then Nothing else Just (a - b)
  specify "toMap (deleteWith sub k v a) = Map.update (sub v) k (toMap a)" $
    property $ \k v (TMap'' a) ->
      toMap (deleteWith sub k v a) ==
      Map.update (sub v) k (toMap a)
  specify "toMap (adjust (*2) k a) = Map.adjust (*2) k (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (adjust (*2) k a) == Map.adjust (*2) k (toMap a)
  let alterer Nothing = Just 100
      alterer (Just a) | odd a = Nothing
                       | otherwise = Just (a `div` 2)
  specify "toMap (alter alterer k a) = Map.alter alterer k (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (alter alterer k a) == Map.alter alterer k (toMap a)

  specify "toMap (union a b) = Map.union (toMap a) (toMap b)" $
    property $ \(TMap' a) (TMap' b) ->
      toMap (union a b) == Map.union (toMap a) (toMap b)
  specify "toMap (unionWith f a b) = Map.unionWith f (toMap a) (toMap b)" $
    property $ \(Fn2 f) (TMap' a) (TMap' b) ->
      toMap (unionWith f a b) == Map.unionWith f (toMap a) (toMap b)
  specify "toMap (intersection a b) = Map.intersection (toMap a) (toMap b)" $
    property $ \(TMap' a) (TMap' b) ->
      toMap (intersection a b) == Map.intersection (toMap a) (toMap b)
  specify "toMap (difference a b) = Map.difference (toMap a) (toMap b)" $
    property $ \(TMap' a) (TMap' b) ->
      toMap (difference a b) == Map.difference (toMap a) (toMap b)
  specify "appendWith = appendWithSpec" $
    property $ \(TMap' a) (TMap' b) ->
      appendWith (\x y -> show (x,y)) a b ==
      appendWithSpec (\x y -> show (x,y)) a b 
      
  specify "validTMap (union a b)" $
    property $ \(TMap' a) (TMap' b) ->
      validTMap (union a b)
  specify "validTMap (intersection a b)" $
    property $ \(TMap' a) (TMap' b) ->
      validTMap (intersection a b)
  specify "validTMap (difference a b)" $
    property $ \(TMap' a) (TMap' b) ->
      validTMap (difference a b)
  specify "validTMap (append a b)" $
    property $ \(TMap' a) (TMap' b) ->
      validTMap (getSum <$> appendWith (\x y -> Sum (x * y)) a b)

appendWithSpec :: (Ord c, Semigroup z) => (x -> y -> z) ->
  TMap c x -> TMap c y -> TMap c z
appendWithSpec f x y = fromListWith (flip (<>))
  [ (kx ++ ky, f valx valy)
    | (kx, valx) <- toAscList x
    , (ky, valy) <- toAscList y ]
