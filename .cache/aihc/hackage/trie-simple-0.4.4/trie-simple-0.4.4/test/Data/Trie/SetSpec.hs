module Data.Trie.SetSpec(
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.List         (sort, inits, tails)
import           Data.Set          (Set)
import qualified Data.Set          as Set

import           Data.Trie.Set     as T
import           Data.Trie.Set.Gen

spec :: Spec
spec = do
  specify "enumerate empty = []" $
    enumerate (empty :: TSet C) `shouldSatisfy` Prelude.null
  specify "enumerate epsilon = [[]]" $
    enumerate (epsilon :: TSet C) `shouldBe` [[]]
  specify "enumerate . singleton = (:[])" $
    property $ \str -> enumerate (singleton (str :: [C])) === [str]
  specify "enumerate . fromList = Set.toAscList . Set.fromList" $
    property $ \strs -> (enumerate . fromList) (strs :: [[C]]) == (Set.toAscList . Set.fromList) strs
  specify "null . enumerate = null" $
    property $ \(TSet' t) -> Prelude.null (enumerate t) === T.null t
  specify "length . enumerate = count" $
    property $ \(TSet' t) -> length (enumerate t) === count t

  specify "toSet (insert xs a) = Set.insert xs (toSet a)" $
    property $ \xs (TSet' t) -> toSet (T.insert xs t) == Set.insert xs (toSet t)
  specify "toSet (delete xs a) = Set.delete xs (toSet a)" $
    property $ \xs (TSet' t) -> toSet (T.delete xs t) == Set.delete xs (toSet t)
  
  specify "member t = (`Set.member` toSet t)" $
    property $ \(TSet'' t) ->
      let strSet = toSet t
      in property $ \str -> member str t == Set.member str strSet
  specify "forAll (str `in` enumerate t). member str t" $
    property $ \(TSet'' t) -> all (`member` t) <$> acceptStrs t
  specify "member (xs ++ ys) t = member ys (beginWith t xs)" $
    property $ \xs ys (TSet'' t) ->
      member (xs ++ ys) t == member ys (beginWith t xs)

  specify "toSet (union a b) = Set.union (toSet a) (toSet b)" $
    property $ \(TSet' a) (TSet' b) ->
      toSet (union a b) == Set.union (toSet a) (toSet b)
  specify "toSet (intersection a b) = Set.intersection (toSet a) (toSet b)" $
    property $ \(TSet' a) (TSet' b) ->
      toSet (intersection a b) == Set.intersection (toSet a) (toSet b)
  specify "toSet (difference a b) = Set.difference (toSet a) (toSet b)" $
    property $ \(TSet' a) (TSet' b) ->
      toSet (difference a b) == Set.difference (toSet a) (toSet b)
  specify "toSet (append a b) = setAppend (toSet a) (toSet b)" $
    property $ \(TSet' a) (TSet' b) ->
      toSet (append a b) == setAppend (toSet a) (toSet b)

  specify "validTSet (union a b)" $
    property $ \(TSet' a) (TSet' b) ->
      validTSet (union a b)
  specify "validTSet (intersection a b)" $
    property $ \(TSet' a) (TSet' b) ->
      validTSet (intersection a b)
  specify "validTSet (difference a b)" $
    property $ \(TSet' a) (TSet' b) ->
      validTSet (difference a b)
  specify "validTSet (append a b)" $
    property $ \(TSet' a) (TSet' b) ->
      validTSet (append a b)
  
  specify "toSet (prefixes a) = setPrefixes (toSet a)" $
    property $ \(TSet' a) ->
      toSet (prefixes a) === setPrefixes (toSet a)
  specify "toSet (suffixes a) = setSuffixes (toSet a)" $
    property $ \(TSet' a) ->
      toSet (suffixes a) === setSuffixes (toSet a)

  specify "fromAscList . sort = fromList" $
    property $ \strs -> fromAscList (sort strs) == fromList (strs :: [[C]])
  specify "fromSet . Set.fromList = fromList" $
    property $ \strs -> fromSet (Set.fromList strs) == fromList (strs :: [[C]])

setAppend :: (Ord c) => Set [c] -> Set [c] -> Set [c]
setAppend ass bss = Set.unions
  [ Set.mapMonotonic (as ++) bss
      | as <- Set.toAscList ass ]

setPrefixes :: (Ord c) => Set [c] -> Set [c]
setPrefixes ass = Set.unions
  [ Set.fromDistinctAscList (inits as) | as <- Set.toAscList ass ]

setSuffixes :: (Ord c) => Set [c] -> Set [c]
setSuffixes ass = Set.fromList
  [ bs | as <- Set.toAscList ass, bs <- tails as ]
