module Test.Mastermind where

import qualified Game.Mastermind.CodeSet.Tree as CodeSetTree
-- import qualified Game.Mastermind.CodeSet.Union as CodeSetUnion
import qualified Game.Mastermind.CodeSet as CodeSet
import qualified Game.Mastermind.NonEmptyEnumSet as NonEmptySet
import qualified Game.Mastermind as MM

import Control.Applicative (liftA2, (<$>))

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Traversable as Trav
import Data.EnumSet (EnumSet)
import Data.NonEmpty ((!:))

import qualified Test.QuickCheck as QC
import Test.QuickCheck (Property, Arbitrary(arbitrary), (==>), )


alphabet :: EnumSet Int
alphabet = NonEmptySet.flatten neAlphabet

neAlphabet :: NonEmptySet.T Int
neAlphabet = NonEmptySet.fromList $ 0!:[1..9]


newtype Code = Code [Int]
   deriving (Show)


genElement :: QC.Gen Int
genElement = QC.choose (0,9)

-- can we get it working with empty lists, too?
genCode :: Int -> QC.Gen Code
genCode width =
   fmap (Code . take width) $ QC.listOf1 genElement
--    fmap (Code . take width) (QC.listOf genElement)

instance Arbitrary Code where
   arbitrary = genCode 5


data CodePair = CodePair [Int] [Int]
   deriving (Show)

genCodePair :: Int -> QC.Gen CodePair
genCodePair width =
   liftA2
      (\(Code xs) (Code ys) -> uncurry CodePair $ unzip $ zip xs ys)
      (genCode width) (genCode width)

instance Arbitrary CodePair where
   arbitrary = genCodePair 5


genEval :: Int -> QC.Gen MM.Eval
genEval size = do
   total <- QC.frequency $ map (\k -> (k+1, return k)) [1 .. size]
   rightPlaces <- QC.choose (0,total)
   return $ MM.Eval rightPlaces (total - rightPlaces)

forAllEval :: QC.Testable prop => [a] -> (MM.Eval -> prop) -> Property
forAllEval code = QC.forAll (genEval (length code))


type CodeSetInt = CodeSetTree.T Int


genFixedLengthCodes :: (NonEmptyC.Gen f) => Int -> QC.Gen (f [Int])
genFixedLengthCodes width = NonEmptyC.genOf $ QC.vectorOf width genElement

bestSeparatingCode :: Property
bestSeparatingCode =
   QC.forAll (genCodePair 4) $ \(CodePair base0 base1) ->
   forAllEval base0 $ \eval0 ->
   forAllEval base1 $ \eval1 -> do
   let width = length base0
       set =
         CodeSet.intersection
            (MM.matching alphabet base0 eval0)
            (MM.matching alphabet base1 eval1)
   not (CodeSet.null set) ==>
      QC.forAll (fmap (NonEmpty.mapTail $ take 9) $ genFixedLengthCodes width) $
         MM.propBestSeparatingCode width (set :: CodeSetInt)

intersections :: Property
intersections =
   QC.forAll (genCode 4) $ \(Code code) ->
   QC.forAll (fmap (take 10) $ genFixedLengthCodes (length code)) $ \codes ->
   QC.forAll (Trav.mapM (\x -> (,) x <$> genEval (length code)) (code!:codes)) $
      CodeSetTree.propIntersections . fmap (uncurry $ MM.matching alphabet)



-- should also work, when selecting any code from the set of matching codes
solve :: Code -> Bool
solve (Code secret) =
   let recourse remain =
          case CodeSet.flatten remain of
             [] -> False
             [attempt] -> secret == attempt
             attempt:_ ->
                recourse $ CodeSet.intersection remain $
                MM.matching alphabet attempt $ MM.evaluate secret attempt
   in  recourse (CodeSet.cube neAlphabet (length secret) :: CodeSetInt)


{-
Other possible tests:

the products in a set produced by 'MM.matching' must be disjoint.

set laws for the two set implementations,
   such as distributivity of union and intersection

check member against intersection with singleton
-}
