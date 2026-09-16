{-# LANGUAGE OverloadedStrings #-}

-- | The compile-time folding of primitives agrees with the interpreter.
module Test.Fc.Fold
  ( fcFoldTests,
  )
where

import Aihc.Fc.Fold (PrimLiteral (..), foldPrimitive, foldedPrimitives)
import Aihc.Grin.Interpret (evalLiteralPrimitive)
import Aihc.Grin.Syntax (GrinLiteral (..), GrinRep (..))
import Data.Bits (shiftL)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, Property, annotateShow, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)
import Text.Read (readMaybe)

fcFoldTests :: TestTree
fcFoldTests =
  testGroup
    "primitive folding"
    [ testProperty "foldPrimitive agrees with the interpreter" prop_foldAgreesWithInterpreter,
      testCase "folds the conversions and comparisons a build leaves on literals" $ do
        assertEqual "int2Word#" (Just (PrimInt "WordRep" 0)) (foldPrimitive "int2Word#" [PrimInt "IntRep" 0])
        assertEqual "int2Word# of a negative" (Just (PrimInt "WordRep" (shiftL 1 64 - 1))) (foldPrimitive "int2Word#" [PrimInt "IntRep" (-1)])
        assertEqual "<#" (Just (PrimInt "IntRep" 1)) (foldPrimitive "<#" [PrimInt "IntRep" 3, PrimInt "IntRep" 4])
        assertEqual "-#" (Just (PrimInt "IntRep" (-1))) (foldPrimitive "-#" [PrimInt "IntRep" 0, PrimInt "IntRep" 1])
        assertEqual "intToInt32# wraps" (Just (PrimInt "Int32Rep" (-1))) (foldPrimitive "intToInt32#" [PrimInt "IntRep" 0xffffffff])
        assertEqual "chr#" (Just (PrimChar 'a')) (foldPrimitive "chr#" [PrimInt "IntRep" 97]),
      testCase "leaves a partial operation alone" $ do
        assertEqual "quotInt# by zero" Nothing (foldPrimitive "quotInt#" [PrimInt "IntRep" 1, PrimInt "IntRep" 0])
        assertEqual "shift by the width" Nothing (foldPrimitive "uncheckedShiftL#" [PrimInt "WordRep" 1, PrimInt "IntRep" 64])
        assertEqual "chr# out of range" Nothing (foldPrimitive "chr#" [PrimInt "IntRep" 0x110000])
        assertEqual "wrong representation" Nothing (foldPrimitive "+#" [PrimInt "WordRep" 1, PrimInt "IntRep" 1])
    ]

-- | The fold of a primitive on literals is what the interpreter computes.
-- An operation the fold refuses makes no claim.
prop_foldAgreesWithInterpreter :: Property
prop_foldAgreesWithInterpreter = property $ do
  (name, signature) <- forAll (Gen.element foldedPrimitives)
  arguments <- forAll (mapM genLiteral signature)
  case foldPrimitive name arguments of
    Nothing -> pure ()
    Just folded -> do
      evaluated <- evalIO (evalLiteralPrimitive name (map toGrin arguments))
      annotateShow evaluated
      fmap (map fromGrin) evaluated === Right [Just folded]

-- | A literal of the representation, drawn from the small values a program
-- has and from the whole range alike.
genLiteral :: Maybe Text -> Gen PrimLiteral
genLiteral signature =
  case signature of
    Nothing -> PrimChar <$> Gen.unicode
    Just rep -> PrimInt rep <$> Gen.choice [Gen.integral (Range.linear (max low (-100)) (min high 100)), Gen.integral (Range.linear low high)]
      where
        (low, high) = bounds rep

bounds :: Text -> (Integer, Integer)
bounds rep =
  case rep of
    "IntRep" -> signed 64
    "Int8Rep" -> signed 8
    "Int16Rep" -> signed 16
    "Int32Rep" -> signed 32
    "Int64Rep" -> signed 64
    "Word8Rep" -> unsigned 8
    "Word16Rep" -> unsigned 16
    "Word32Rep" -> unsigned 32
    _ -> unsigned 64
  where
    signed :: Int -> (Integer, Integer)
    signed bits = (negate (shiftL 1 (bits - 1)), shiftL 1 (bits - 1) - 1)
    unsigned :: Int -> (Integer, Integer)
    unsigned bits = (0, shiftL 1 bits - 1)

toGrin :: PrimLiteral -> GrinLiteral
toGrin literal =
  case literal of
    PrimInt rep value -> GrinLitInt (fromMaybe (error ("unknown representation " <> T.unpack rep)) (readMaybe (T.unpack rep))) value
    PrimChar value -> GrinLitChar WordRep value

fromGrin :: GrinLiteral -> Maybe PrimLiteral
fromGrin literal =
  case literal of
    GrinLitInt rep value -> Just (PrimInt (T.pack (show rep)) value)
    GrinLitChar _ value -> Just (PrimChar value)
    GrinLitAddr _ -> Nothing
