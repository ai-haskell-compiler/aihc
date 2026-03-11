module Main where

import qualified Control.Foldl as Fold
import qualified Data.Text as Text
import qualified NeatInterpolation as NeatInterpolation
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import qualified YamlUnscrambler as U
import Prelude hiding (assert)

main :: IO ()
main =
  defaultMain
    $ testGroup
      "All tests"
      [ testCase "Should fail on sequence when no sequence is specified"
          $ let unscrambler =
                  U.value [] (Just mapping) Nothing
                  where
                    mapping =
                      U.foldMapping (,) Fold.list U.textString value
                      where
                        value =
                          U.value [nullScalar, intScalar] Nothing Nothing
                          where
                            nullScalar =
                              U.nullScalar Nothing
                            intScalar =
                              fmap Just $ U.boundedIntegerScalar @Int (U.Signed True) U.DecimalNumeralSystem
                input =
                  [NeatInterpolation.text|
                  a: 2
                  b: 3
                  c:
                    - 1
                    - 2
                |]
             in case U.parseText unscrambler input of
                  Right res ->
                    assertFailure (show res)
                  Left failure ->
                    assertEqual (Text.unpack failure) "Error at path /c. Unexpected sequence value" failure,
        testCase "Domain sum-type regression"
          $ let unscrambler =
                  doc
                  where
                    doc =
                      U.mappingValue
                        $ U.byKeyMapping (U.CaseSensitive True)
                        $ asum
                          [ Just <$> U.atByKey "sums" sum,
                            pure Nothing
                          ]
                    sum =
                      U.mappingValue
                        $ U.foldMapping (,) Fold.list U.textString sumVariant
                    sumVariant =
                      U.mappingValue
                        $ U.foldMapping (,) Fold.list U.textString params
                    params =
                      U.value [nullScalar, intScalar] Nothing Nothing
                      where
                        nullScalar =
                          U.nullScalar Nothing
                        intScalar =
                          fmap Just $ U.boundedIntegerScalar @Int (U.Signed True) U.DecimalNumeralSystem
                input =
                  [NeatInterpolation.text|
                  sums:
                    A:
                      a:
                        - Int
                        - Bool
                      b: Char, Double
                |]
             in case U.parseText unscrambler input of
                  Right res ->
                    assertFailure (show res)
                  Left failure ->
                    assertEqual "" "Error at path /sums/A/a. Unexpected sequence value" failure,
        testCase "Domain sum-type correct"
          $ let unscrambler =
                  doc
                  where
                    doc =
                      U.mappingValue
                        $ U.byKeyMapping (U.CaseSensitive True)
                        $ asum
                          [ Just <$> U.atByKey "sums" sum,
                            pure Nothing
                          ]
                    sum =
                      U.mappingValue
                        $ U.foldMapping (,) Fold.list U.textString sumVariant
                    sumVariant =
                      U.mappingValue
                        $ U.foldMapping (,) Fold.list U.textString params
                    params =
                      U.value [nullScalar, valueScalar] Nothing Nothing
                      where
                        nullScalar =
                          U.nullScalar Nothing
                        valueScalar =
                          fmap Just $ U.stringScalar U.textString
                input =
                  [NeatInterpolation.text|
                  sums:
                    A:
                      a: Text
                      b: Int
                |]
             in case U.parseText unscrambler input of
                  Right res ->
                    assertEqual "" (Just [("A", [("a", Just "Text"), ("b", Just "Int")])]) res
                  Left failure ->
                    assertFailure (Text.unpack failure),
        testCase "Scalar errors are readable"
          $ let unscrambler =
                  doc
                  where
                    doc =
                      U.mappingValue
                        $ U.byKeyMapping (U.CaseSensitive True)
                        $ asum
                          [ Just <$> U.atByKey "sums" sum,
                            pure Nothing
                          ]
                    sum =
                      U.mappingValue
                        $ U.foldMapping (,) Fold.list U.textString sumVariant
                    sumVariant =
                      U.mappingValue
                        $ U.foldMapping (,) Fold.list U.textString params
                    params =
                      U.value [nullScalar, intScalar] Nothing Nothing
                      where
                        nullScalar =
                          U.nullScalar Nothing
                        intScalar =
                          fmap Just $ U.boundedIntegerScalar @Int (U.Signed True) U.DecimalNumeralSystem
                input =
                  [NeatInterpolation.text|
                  sums:
                    A:
                      a: Int
                      b: Char
                |]
             in case U.parseText unscrambler input of
                  Right res ->
                    assertFailure (show res)
                  Left failure ->
                    assertEqual "" "Error at path /sums/A/a. Expecting one of the following formats: null, signed decimal. Got input: \"Int\"" failure
      ]
