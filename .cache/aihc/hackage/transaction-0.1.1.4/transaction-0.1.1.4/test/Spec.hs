{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding
  ( all
  , any
  , break
  , drop
  , dropWhile
  , filter
  , foldMap
  , foldl
  , foldl1
  , foldr
  , foldr1
  , head
  , init
  , last
  , length
  , repeat
  , null
  , replicate
  , reverse
  , seq
  , span
  , splitAt
  , tail
  , take
  , takeWhile
  )
import Data.Bifunctor (Bifunctor(..))
import Data.MonoTraversable
  ( MonoFoldable(..)
  , MonoFunctor(..)
  , MonoPointed(..)
  , MonoTraversable(..)
  )
import Data.Sequences (IsSequence(..), SemiSequence(..))
import Data.Transaction
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Exception

main :: IO ()
main = hspec spec

exceptionableEq :: Eq a => Either AnException a -> Either AnException a -> Bool
exceptionableEq (Left _) (Left _) = True
exceptionableEq (Right a) (Right b) = a == b
exceptionableEq _ _ = False

spec :: Spec
spec = do
  describe "MonoFunctor" $ do
    describe "omap" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (a :: [Int]) ->
          omap f a == (toList . omap f . listToTrans) a

  describe "MonoFoldable" $ do
    describe "ofoldMap" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (a :: [Int]) ->
          (ofoldMap f a :: String) ==
          (ofoldMap f $ listToTrans a)
    describe "ofoldr" $ do
      modifyMaxSize (const 20) $ prop "is equivalent to list" $
        \f (a :: String) (mono :: [Int]) ->
          (ofoldr (applyFun2 f) a mono) ==
          (ofoldr (applyFun2 f) a $ listToTrans mono)
    describe "ofoldl'" $ do
      modifyMaxSize (const 20) $ prop "is equivalent to list" $
        \f (a :: String) (mono :: [Int]) ->
          (ofoldl' (applyFun2 f) a mono) ==
          (ofoldl' (applyFun2 f) a $ listToTrans mono)
    describe "otoList" $ do
      prop "is equivalent to list" $
        \(mono :: [Int]) ->
          otoList (listToTrans mono) == mono
    describe "oall" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (mono :: [Int]) ->
          (oall f mono) ==
          (oall f $ listToTrans mono)
    describe "oany" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (mono :: [Int]) ->
          (oany f mono) ==
          (oany f $ listToTrans mono)
    describe "onull" $ do
      prop "is equivalent to list" $
        \(mono :: [Int]) ->
          (onull mono) ==
          (onull $ listToTrans mono)
    describe "olength" $ do
      prop "is equivalent to list" $
        \(mono :: [Int]) ->
          (olength mono) ==
          (olength $ listToTrans mono)
    describe "olength64" $ do
      prop "is equivalent to list" $
        \(mono :: [Int]) ->
          (olength64 mono) ==
          (olength64 $ listToTrans mono)
    describe "ocompareLength" $ do
      prop "is equivalent to list" $
        \(i :: Integer) (mono :: [String]) ->
          (ocompareLength mono i) ==
          (ocompareLength (listToTrans mono) i)
    describe "otraverse_" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (mono :: [Int]) ->
          (otraverse_ (f :: Int -> Maybe String) mono) ==
          (otraverse_ f $ listToTrans mono)
    describe "ofor_" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (mono :: [Int]) ->
          (ofor_ mono (f :: Int -> Maybe String)) ==
          (ofor_ (listToTrans mono) f)
    describe "omapM_" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (mono :: [Int]) ->
          (omapM_ (f :: Int -> Maybe ()) mono) ==
          (omapM_ f (listToTrans mono))
    describe "oforM_" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (mono :: [Int]) ->
          (ofor_ mono (f :: Int -> Maybe ())) ==
          (ofor_ (listToTrans mono) f)
    describe "ofoldlM" $ do
      modifyMaxSize (const 40) $ prop "is equivalent to list" $
        \f (a :: String) (mono :: [Int]) ->
          (ofoldlM (applyFun2 f) a mono :: Maybe String) ==
          (ofoldlM (applyFun2 f) a (listToTrans mono))
    describe "ofoldMap1Ex" $ do
      it "is equivalent to list" $ again $ \(Fun _ f) (mono :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (ofoldMap1Ex f mono :: String)
          <*> tryEvaluate (ofoldMap1Ex f (listToTrans mono))
    describe "ofoldr1Ex" $ do
      it "is equivalent to list" $ again $ \f (mono :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (ofoldr1Ex (applyFun2 f) mono)
          <*> tryEvaluate (ofoldr1Ex (applyFun2 f) (listToTrans mono))
    describe "ofoldl1Ex'" $ do
      it "is equivalent to list" $ again $ \f (mono :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (ofoldl1Ex' (applyFun2 f) mono)
          <*> tryEvaluate (ofoldl1Ex' (applyFun2 f) (listToTrans mono))
    describe "headEx" $ do
      it "is equivalent to list" $ again $ \(mono :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (headEx mono)
          <*> tryEvaluate (headEx (listToTrans mono))
    describe "lastEx" $ do
      it "is equivalent to list" $ again $ \(mono :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (lastEx mono)
          <*> tryEvaluate (lastEx (listToTrans mono))
    describe "unsafeHead" $ do
      it "is equivalent to list" $ again $ \(mono :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (unsafeHead mono)
          <*> tryEvaluate (unsafeHead (listToTrans mono))
    describe "unsafeLast" $ do
      it "is equivalent to list" $ again $ \(mono :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (unsafeLast mono)
          <*> tryEvaluate (unsafeLast (listToTrans mono))
    describe "maximumByEx" $ do
      it "is equivalent to list" $ again $ \f (mono :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (maximumByEx (applyFun2 f) mono)
          <*> tryEvaluate (maximumByEx (applyFun2 f) (listToTrans mono))
    describe "minimumByEx" $ do
      it "is equivalent to list" $ again $ \f (mono :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (minimumByEx (applyFun2 f) mono)
          <*> tryEvaluate (minimumByEx (applyFun2 f) (listToTrans mono))

#if MIN_VERSION_mono_traversable(1,0,5)
    describe "oelem" $ do
      prop "is equivalent to list" $
        \(a :: Int) (mono :: [Int]) ->
          (oelem a mono) ==
          (oelem a (listToTrans mono))
    describe "onotElem" $ do
      prop "is equivalent to list" $
        \(a :: Int) (mono :: [Int]) ->
          (onotElem a mono) ==
          (onotElem a (listToTrans mono))
#endif

  describe "MonoPointed" $ do
    describe "opoint" $ do
      prop "is equivalent to list" $
        \(a :: Int) ->
          (opoint a) ==
          (toList $ opoint a)

  describe "SemiSequence" $ do
    describe "intersperse" $ do
      prop "is equivalent to list" $
        \(seq :: [Int]) a ->
          (intersperse a seq) ==
          (toList $ intersperse a $ listToTrans seq)
    describe "reverse" $ do
      prop "is equivalent to list" $
        \(seq :: [Int]) ->
          (reverse seq) ==
          (toList $ reverse $ listToTrans seq)
    describe "sortBy" $ do
      prop "is equivalent to list" $
        \f (seq :: [Int]) ->
          (sortBy (applyFun2 f) seq) ==
          (toList $ sortBy (applyFun2 f) $ listToTrans seq)
    describe "cons" $ do
      prop "is equivalent to list" $
        \a (seq :: [Int]) ->
          (cons a seq) ==
          (toList $ cons a $ listToTrans seq)
    describe "snoc" $ do
      prop "is equivalent to list" $
        \a (seq :: [Int]) ->
          (snoc seq a) ==
          (toList $ snoc (listToTrans seq) a)

  describe "MonoTraversable" $ do
    describe "otraverse" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (mono :: [Int]) ->
          (otraverse (f :: Int -> Maybe Int) mono) ==
          (toList <$> otraverse f (listToTrans mono))
    describe "omapM" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (mono :: [Int]) ->
          (otraverse (f :: Int -> Maybe Int) mono) ==
          (toList <$> otraverse f (listToTrans mono))

  describe "IsSequence" $ do
    describe "fromList" $ do
      prop "is equivalent to list" $
        \(seq :: [Int]) ->
          (toList $ fromList seq) == seq
#if MIN_VERSION_mono_traversable(1,0,2)
    describe "lengthIndex" $ do
      prop "is equivalent to list" $
        \(seq :: [Int]) ->
          (lengthIndex seq) ==
          (lengthIndex (listToTrans seq))
#endif
    describe "break" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (seq :: [Int]) ->
          (break f seq) ==
          (let (a,b) = break f (listToTrans seq) in (toList a, toList b))
    describe "span" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (seq :: [Int]) ->
          (span f seq) ==
          (let (a,b) = span f (listToTrans seq) in (toList a, toList b))
    describe "dropWhile" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (seq :: [Int]) ->
          (dropWhile f seq) ==
          (toList $ dropWhile f $ listToTrans seq)
    describe "takeWhile" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (seq :: [Int]) ->
          (takeWhile f seq) ==
          (toList $ takeWhile f $ listToTrans seq)
    describe "splitAt" $ do
      prop "is equivalent to list" $
        \n (seq :: [Int]) ->
          (splitAt n seq) ==
          (let (a,b) = splitAt n (listToTrans seq) in (toList a, toList b))
    describe "unsafeSplitAt" $ do
      prop "is equivalent to list" $
        \n (seq :: [Int]) ->
          (unsafeSplitAt n seq) ==
          (let (a,b) = unsafeSplitAt n (listToTrans seq) in (toList a, toList b))
    describe "take" $ do
      prop "is equivalent to list" $
        \n (seq :: [Int]) ->
          (take n seq) ==
          (toList $ take n $ fromList seq)
    describe "unsafeTake" $ do
      prop "is equivalent to list" $
        \n (seq :: [Int]) ->
          (unsafeTake n seq) ==
          (toList $ unsafeTake n $ fromList seq)
    describe "drop" $ do
      prop "is equivalent to list" $
        \n (seq :: [Int]) ->
          (drop n seq) ==
          (toList $ drop n $ fromList seq)
    describe "unsafeDrop" $ do
      prop "is equivalent to list" $
        \n (seq :: [Int]) ->
          (unsafeDrop n seq) ==
          (toList $ unsafeDrop n $ fromList seq)
#if MIN_VERSION_mono_traversable(1,0,4)
    describe "dropEnd" $ do
      prop "is equivalent to list" $
        \n (seq :: [Int]) ->
          (dropEnd n seq) ==
          (toList $ dropEnd n $ fromList seq)
#endif
    describe "partition" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (seq :: [Int]) ->
          (partition f seq) ==
          (let (a, b) = partition f (listToTrans seq) in (toList a, toList b))
    describe "uncons" $ do
      prop "is equivalent to list" $
        \(seq :: [Int]) ->
          (uncons seq) ==
          (second toList <$> uncons (listToTrans seq))
    describe "unsnoc" $ do
      prop "is equivalent to list" $
        \(seq :: [Int]) ->
          (unsnoc seq) ==
          (first toList <$> unsnoc (listToTrans seq))
    describe "filter" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (seq :: [Int]) ->
          (filter f seq) ==
          (toList $ filter f $ listToTrans seq)
    describe "filterM" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (seq :: [Int]) ->
          (filterM (f :: Int -> Maybe Bool) seq) ==
          (toList <$> filterM f (listToTrans seq))
    describe "replicate" $ do
      prop "is equivalent to list" $
        \n (a :: Int) ->
          (replicate n a) ==
          (toList $ replicate n a)
    describe "replicateM" $ do
      prop "is equivalent to list" $
        \n (ma :: Maybe Int) ->
          (replicateM n ma) ==
          (toList <$> replicateM n ma)
    describe "groupBy" $ do
      prop "is equivalent to list" $
        \f (seq :: [Int]) ->
          (groupBy (applyFun2 f) seq) ==
          (map toList $ groupBy (applyFun2 f) $ listToTrans seq)
    describe "groupAllOn" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (seq :: [Int]) ->
          (groupAllOn (f :: Int -> String) seq) ==
          (map toList $ groupAllOn f $ listToTrans seq)
    describe "subsequences" $ do
      modifyMaxSize (const 10) $ prop "is equivalent to list" $
        \(seq :: [Int]) ->
          (subsequences seq) ==
          (map toList $ subsequences $ listToTrans seq)
    describe "permutations" $ do
      modifyMaxSize (const 10) $ prop "is equivalent to list" $
        \(seq :: [Int]) ->
          (permutations seq) ==
          (map toList $ permutations $ listToTrans seq)
    describe "tailEx" $ do
      it "is equivalent to list" $ again $ \(seq :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (tailEx seq)
          <*> tryEvaluate (toList $ tailEx $ listToTrans seq)
    describe "tailMay" $ do
      prop "is equivalent to list" $
        \(seq :: [Int]) ->
          (tailMay seq) ==
          (fmap toList $ tailMay $ listToTrans seq)
    describe "initEx" $ do
      it "is equivalent to list" $ again $ \(seq :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (initEx seq)
          <*> tryEvaluate (toList $ initEx $ listToTrans seq)
    describe "initMay" $ do
      prop "is equivalent to list" $
        \(seq :: [Int]) ->
          (initMay seq) ==
          (fmap toList $ initMay $ listToTrans seq)
    describe "unsafeTail" $ do
      it "is equivalent to list" $ again $ \(seq :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (unsafeTail seq)
          <*> tryEvaluate (toList $ unsafeTail $ listToTrans seq)
    describe "unsafeInit" $ do
      it "is equivalent to list" $ again $ \(seq :: [Int]) -> ioProperty $
        exceptionableEq
          <$> tryEvaluate (unsafeInit seq)
          <*> tryEvaluate (toList $ unsafeInit $ listToTrans seq)
    describe "index" $ do
      prop "is equivalent to list" $
        \(seq :: [Int]) n ->
          (index seq n) ==
          (index (listToTrans seq) n)
    describe "indexEx" $ do
      it "is equivalent to list" $ again $ \(seq :: [Int]) n -> ioProperty $
        exceptionableEq <$> tryEvaluate (indexEx seq n) <*> tryEvaluate (indexEx (listToTrans seq) n)
    describe "unsafeIndex" $ do
      it "is equivalent to list" $ again $ \(seq :: [Int]) n -> ioProperty $
        exceptionableEq <$> tryEvaluate (unsafeIndex seq n) <*> tryEvaluate (unsafeIndex (listToTrans seq) n)

    describe "splitWhen" $ do
      prop "is equivalent to list" $
        \(Fun _ f) (seq :: [Int]) ->
          (splitWhen f seq) ==
          (fmap toList $ splitWhen f $ listToTrans seq)

listToTrans :: [a] -> Transaction a
listToTrans = fromList
