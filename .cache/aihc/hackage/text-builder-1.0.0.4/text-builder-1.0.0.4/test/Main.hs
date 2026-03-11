module Main where

import qualified Data.ByteString as ByteString
import Data.Char (intToDigit)
import Data.Int
import Data.Proxy
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word
import Numeric
import Numeric.Natural (Natural)
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import TextBuilder
import Util.ExtraInstances ()
import Util.TestTrees
import Prelude

main :: IO ()
main = (defaultMain . testGroup "All") tests

tests :: [TestTree]
tests =
  [ testGroup "Instances" $
      [ followsLaws $ showLaws (Proxy @TextBuilder),
        followsLaws $ eqLaws (Proxy @TextBuilder),
        followsLaws $ semigroupLaws (Proxy @TextBuilder),
        followsLaws $ monoidLaws (Proxy @TextBuilder)
      ],
    testGroup "Functions" $
      [ testGroup "decimal" $
          [ testGroup "Int" $
              [ mapsToMonoid @Int decimal,
                testProperty "Encodes the same as show" $ \(x :: Int) ->
                  (fromString . show) x === toText (decimal x)
              ],
            testGroup "Int8" $
              [ mapsToMonoid @Int8 decimal,
                testProperty "Encodes the same as show" $ \(x :: Int8) ->
                  (fromString . show) x === toText (decimal x)
              ],
            testGroup "Word" $
              [ mapsToMonoid @Word decimal,
                testProperty "Encodes the same as show" $ \(x :: Word) ->
                  (fromString . show) x === toText (decimal x)
              ],
            testGroup "Word8" $
              [ mapsToMonoid @Word8 decimal,
                testProperty "Encodes the same as show" $ \(x :: Word8) ->
                  (fromString . show) x === toText (decimal x)
              ],
            testGroup "Integer" $
              [ mapsToMonoid @Integer decimal,
                testProperty "Encodes the same as show" $ \(x :: Integer) ->
                  (fromString . show) x === toText (decimal x)
              ],
            testGroup "Natural" $
              [ mapsToMonoid @Natural decimal,
                testProperty "Encodes the same as show" $ \(x :: Natural) ->
                  (fromString . show) x === toText (decimal x)
              ]
          ],
        testGroup "fixedLengthDecimal" $
          [ testGroup "Word" $
              [ mapsToMonoid @Word (fixedLengthDecimal 42)
              ],
            testGroup "Natural" $
              [ mapsToMonoid @Natural (fixedLengthDecimal 42),
                testProperty "Encodes the same as printf" $ \(size :: Word8, val :: Natural) ->
                  let rendered = show val
                      renderedLength = length rendered
                      intSize = fromIntegral size
                      padded =
                        if renderedLength > intSize
                          then drop (renderedLength - intSize) rendered
                          else replicate (intSize - renderedLength) '0' <> rendered
                   in fromString padded
                        === toText (fixedLengthDecimal (fromIntegral size) val)
              ]
          ],
        testGroup "thousandSeparatedDecimal" $
          [ testGroup "Int" $
              [ mapsToMonoid @Int (thousandSeparatedDecimal ','),
                testProperty "Encodes the same as show" $ \(x :: Int) ->
                  (fromString . show) x === Text.filter (/= ',') (toText (thousandSeparatedDecimal ',' x))
              ],
            testGroup "Int8" $
              [ mapsToMonoid @Int8 (thousandSeparatedDecimal ','),
                testProperty "Encodes the same as show" $ \(x :: Int8) ->
                  (fromString . show) x === Text.filter (/= ',') (toText (thousandSeparatedDecimal ',' x))
              ],
            testGroup "Integer" $
              [ mapsToMonoid @Integer (thousandSeparatedDecimal ','),
                testProperty "Encodes the same as show" $ \(x :: Integer) ->
                  (fromString . show) x === Text.filter (/= ',') (toText (thousandSeparatedDecimal ',' x))
              ]
          ],
        testGroup "binary" $
          [ testGroup "Int" $
              [ mapsToMonoid @Int binary,
                testProperty "Encodes the same as showIntAtBase" $ \(x :: Word32) ->
                  (Text.justifyRight 32 '0' . Text.pack . showIntAtBase 2 intToDigit x) "" === toText (binary x)
              ]
          ],
        testGroup "octal" $
          [ testGroup "Int" $
              [ mapsToMonoid @Int octal,
                testProperty "Encodes the same as showIntAtBase" $ \(x :: Word32) ->
                  (Text.justifyRight 11 '0' . Text.pack . showIntAtBase 8 intToDigit x) "" === toText (octal x)
              ]
          ],
        testGroup "hexadecimal" $
          [ testGroup "Int" $
              [ mapsToMonoid @Int hexadecimal,
                testProperty "Encodes the same as showHex" $ \(x :: Word32) ->
                  (Text.justifyRight 8 '0' . Text.pack . showHex x) "" === toText (hexadecimal x)
              ]
          ],
        testGroup "unsafeUtf8ByteString" $
          [ mapsToMonoid (unsafeUtf8ByteString . Text.encodeUtf8),
            testProperty "Works on ASCII" $
              let gen = listOf do
                    list <- listOf (choose (0, 127))
                    return (ByteString.pack list)
               in forAll gen \chunks ->
                    mconcat chunks
                      === Text.encodeUtf8 (toText (foldMap unsafeUtf8ByteString chunks))
          ],
        testGroup "intercalate" $
          [ customGenMonoid do
              sep <- arbitrary
              texts <- listOf arbitrary
              return (intercalate (text sep) (fmap text texts)),
            testProperty "Has the same effect as in Text" $
              \separator texts ->
                Text.intercalate separator texts
                  === toText (intercalate (text separator) (fmap text texts))
          ],
        testGroup "intercalateMap" $
          [ customGenMonoid do
              sep <- arbitrary
              texts <- listOf arbitrary
              return (intercalateMap (text sep) text texts),
            testProperty "intercalateMap sep mapper == intercalate sep . fmap mapper" $
              \separator ints ->
                Text.intercalate separator (fmap (fromString . show @Int) ints)
                  === toText (intercalateMap (text separator) decimal ints)
          ]
      ]
  ]
