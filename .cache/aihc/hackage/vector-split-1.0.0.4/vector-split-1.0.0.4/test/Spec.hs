
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           Test.QuickCheck
import           Test.QuickCheck.Function

import qualified Data.Vector as V
import qualified Data.Vector.Split as V
import qualified Data.Vector.Split.Internal as V

import qualified Data.List       as L
import qualified Data.List.Split as L

import           Data.Char

import           Debug.Trace

main :: IO ()
main = defaultMain tests



tests = testGroup "tests" 
  [ testGroup "basic strategies"
    [ QC.testProperty "oneOf" prop_oneOf
    , QC.testProperty "onSublist" prop_onSublist
    , QC.testProperty "whenElt" prop_whenElt
    ]
  , testGroup "strategy transformers"
    [ QC.testProperty "dropDelims" prop_dropDelims
    , QC.testProperty "keepDelimsL" prop_keepDelimsL
    , QC.testProperty "keepDelimsR" prop_keepDelimsR
    , QC.testProperty "condense" prop_condense
    , QC.testProperty "dropInitBlank" prop_dropInitBlank
    , QC.testProperty "dropFinalBlank" prop_dropFinalBlank
    , QC.testProperty "dropInnerBlank" prop_dropInnerBlank
    ]
  , testGroup "other splitting methods"
    [ QC.testProperty "chunksOf" prop_chunksOf
    , QC.testProperty "splitPlaces" prop_splitPlaces
    , QC.testProperty "splitPlacesBlanks" prop_splitPlacesBlanks
    , QC.testProperty "chop" prop_chop
    , QC.testProperty "chopGroup" prop_chopGroup
#if MIN_VERSION_split(2,3,0)
    , QC.testProperty "divvy" prop_divvy
#endif
    ]
  , testGroup "convenience functions"
    [ QC.testProperty "splitOn" prop_splitOn
    , QC.testProperty "splitOneOf" prop_splitOneOf
    , QC.testProperty "splitWhen" prop_splitWhen
    , QC.testProperty "endBy" prop_endBy
    , QC.testProperty "endByOneOf" prop_endByOneOf
    , QC.testProperty "wordsBy" prop_wordsBy
    , QC.testProperty "linesBy" prop_linesBy
    ]
  ]

-- small set of elements used for testing

newtype Elt = Elt { unElt :: Char } deriving (Eq)

type Elements = [Elt]
type SomeElements = NonEmptyList Elt

instance Show Elt where
    show (Elt c) = show c

instance Arbitrary Elt where
    arbitrary = elements (map Elt "abcde")

instance CoArbitrary Elt where
  coarbitrary = coarbitrary . ord . unElt


instance Function Elt where
  function = functionMap unElt Elt

-- Basic Strategies

prop_oneOf :: Elements -> Elements -> Property
prop_oneOf d = listVsVec (L.split (L.oneOf d)) (V.split (V.oneOf (V.fromList d)))

prop_onSublist :: SomeElements -> Elements -> Property
prop_onSublist (NonEmpty d) = listVsVec (L.split (L.onSublist d)) (V.split (V.onSublist (V.fromList d)))

prop_whenElt :: Fun Elt Bool -> Elements -> Property
prop_whenElt (Fun _ f) = listVsVec (L.split (L.whenElt f)) (V.split (V.whenElt f))



-- Strategy Transformers

prop_dropDelims :: SomeElements -> Elements -> Property
prop_dropDelims (NonEmpty d) = listVsVec (L.split (L.dropDelims (L.oneOf d)))
                                         (V.split (V.dropDelims . V.oneOf (V.fromList d)))

prop_keepDelimsL :: SomeElements -> Elements -> Property
prop_keepDelimsL (NonEmpty d) = listVsVec (L.split (L.keepDelimsL (L.oneOf d)))
                                          (V.split (V.keepDelimsL . V.oneOf (V.fromList d)))

prop_keepDelimsR :: SomeElements -> Elements -> Property
prop_keepDelimsR (NonEmpty d) = listVsVec (L.split (L.keepDelimsR (L.oneOf d)))
                                          (V.split (V.keepDelimsR . V.oneOf (V.fromList d)))

prop_condense :: SomeElements -> Elements -> Property
prop_condense (NonEmpty d) = listVsVec (L.split (L.condense (L.oneOf d)))
                                       (V.split (V.condense . V.oneOf (V.fromList d)))

prop_dropInitBlank :: SomeElements -> Elements -> Property
prop_dropInitBlank (NonEmpty d) = listVsVec (L.split (L.dropInitBlank (L.oneOf d)))
                                            (V.split (V.dropInitBlank . V.oneOf (V.fromList d)))

prop_dropFinalBlank :: SomeElements -> Elements -> Property
prop_dropFinalBlank (NonEmpty d) = listVsVec (L.split (L.dropFinalBlank (L.oneOf d)))
                                             (V.split (V.dropFinalBlank . V.oneOf (V.fromList d)))

prop_dropInnerBlank :: SomeElements -> Elements -> Property
prop_dropInnerBlank (NonEmpty d) = listVsVec (L.split (L.dropInnerBlanks (L.oneOf d)))
                                             (V.split (V.dropInnerBlanks . V.oneOf (V.fromList d)))

-- Other Splitting Methods

prop_chunksOf :: Positive Int -> [Double] -> Property
prop_chunksOf (Positive i) = listVsVec (L.chunksOf i) (V.chunksOf i)

prop_splitPlaces :: [Positive Int] -> [Double] -> Property
prop_splitPlaces is = listVsVec (L.splitPlaces is') (V.splitPlaces is')
    where is' = map getPositive is

prop_splitPlacesBlanks :: [Positive Int] -> [Double] -> Property
prop_splitPlacesBlanks is = listVsVec (L.splitPlacesBlanks is') (V.splitPlacesBlanks is')
    where is' = map getPositive is

prop_chop :: [Double] -> Property
prop_chop = listVsVec (L.chop (L.splitAt 10)) (V.chop (V.splitAt 10))

prop_chopGroup :: [Double] -> Property
prop_chopGroup = listVsVec (L.chop (\xs -> L.span (== L.head xs) xs)) (V.chop (\xs -> V.span (== V.head xs) xs))

#if MIN_VERSION_split(2,3,0)
prop_divvy :: Positive Int -> Positive Int -> [Double] -> Property
prop_divvy (Positive n) (Positive m) = listVsVec (L.divvy n m) (V.divvy n m)
#endif



-- Convenience functions

prop_splitOneOf :: SomeElements -> Elements -> Property
prop_splitOneOf (NonEmpty ds) = listVsVec (L.splitOneOf ds) (V.splitOneOf (V.fromList ds))

prop_splitOn :: SomeElements -> Elements -> Property
prop_splitOn (NonEmpty ds) = listVsVec (L.splitOn ds) (V.splitOn (V.fromList ds))

prop_splitWhen :: Fun Elt Bool -> Elements -> Property
prop_splitWhen (Fun _ f) = listVsVec (L.splitWhen f) (V.splitWhen f)

prop_endBy :: SomeElements -> Elements -> Property
prop_endBy (NonEmpty p) = listVsVec (L.endBy p) (V.endBy (V.fromList p))

prop_endByOneOf :: SomeElements -> Elements -> Property
prop_endByOneOf (NonEmpty p) = listVsVec (L.endByOneOf p) (V.endByOneOf (V.fromList p))

prop_wordsBy :: Fun Elt Bool -> Elements -> Property
prop_wordsBy (Fun _ f) = listVsVec (L.wordsBy f) (V.wordsBy f)

prop_linesBy :: Fun Elt Bool -> Elements -> Property
prop_linesBy (Fun _ f) = listVsVec (L.linesBy f) (V.linesBy f)




-- | compare a list splitting vs a vector splitting function
listVsVec :: (Show a, Eq a) => ([a] -> [[a]]) -> (V.Vector a -> [V.Vector a]) -> [a] -> Property
listVsVec a b l = ls === map V.toList vs
    where v = V.fromList l
          ls = a l
          vs = b v
