module Main (main) where

import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.Text (double, float, integral)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.ByteString.Char8 as B

-- Integral values should be rendered exactly as Haskell does.
t_integral :: (Integral a, Show a) => a -> a -> Bool
t_integral _ i = toByteString (integral i) == B.pack (show i)

-- This package doesn't render floating point numbers exactly as
-- Haskell does, but the numbers it renders should read back exactly.
-- So that's the property we check.
t_real :: (RealFloat a, Show a, Read a) => (a -> Builder) -> a -> a -> Bool
t_real f i j =
    case read (B.unpack . toByteString . f $ ij) of
      r | isNaN r      -> isNaN ij
        | isInfinite r -> isInfinite ij && signum r == signum ij
        | otherwise    -> r == ij
  where ij = i / j

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "int" [ testProperty "Integer" $ t_integral (undefined::Integer)
                    , testProperty "Int" $ t_integral (undefined::Int)
                    , testProperty "Int8" $ t_integral (undefined::Int8)
                    , testProperty "Int16" $ t_integral (undefined::Int16)
                    , testProperty "Int32" $ t_integral (undefined::Int32)
                    , testProperty "Int64" $ t_integral (undefined::Int64)
                    ]
  , testGroup "word" [ testProperty "Word" $ t_integral (undefined::Word)
                     , testProperty "Word8" $ t_integral (undefined::Word8)
                     , testProperty "Word16" $ t_integral (undefined::Word16)
                     , testProperty "Word32" $ t_integral (undefined::Word32)
                     , testProperty "Word64" $ t_integral (undefined::Word64)
                     ]
  , testProperty "Double" $ t_real double
  , testProperty "Float" $ t_real float
  ]
