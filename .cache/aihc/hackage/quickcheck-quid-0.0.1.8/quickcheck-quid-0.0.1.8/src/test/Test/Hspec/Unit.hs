{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hspec.Unit where

import Control.Monad
    ( forM_ )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( property, (===) )
import Text.Pretty.Simple
    ( pShow )

import qualified Data.Text.Lazy as TL

--------------------------------------------------------------------------------
-- Unit test support
--------------------------------------------------------------------------------

data UnitTestData params result = UnitTestData
    { params :: params
    , result :: result
    }
    deriving (Eq, Show)

unitTests
    :: (Eq result, Show result)
    => String
    -> (params -> result)
    -> [UnitTestData params result]
    -> Spec
unitTests title f unitTestData =
    describe title $
    forM_ (zip testNumbers unitTestData) $
        \(testNumber :: Int, test) -> do
            let subtitle = "Unit test #" <> show testNumber
            it subtitle $
                let resultExpected = result test in
                let resultActual = f (params test) in
                property $ Pretty resultExpected === Pretty resultActual
  where
    testNumbers :: [Int]
    testNumbers = [1 ..]

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

-- | A combinator that causes the output of `show` to be pretty-printed.
--
newtype Pretty a = Pretty { unPretty :: a }
    deriving Eq

instance Show a => Show (Pretty a) where
    show (Pretty a) = TL.unpack ("\n" <> pShow a <> "\n")
