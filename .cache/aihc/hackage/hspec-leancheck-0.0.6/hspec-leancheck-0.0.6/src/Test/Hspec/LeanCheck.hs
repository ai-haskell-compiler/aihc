-- |
-- Module      : Test.Hspec.LeanCheck
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- LeanCheck support for the Hspec test framework.
--
-- Here's how your @spec.hs@ might look like:
--
-- > import Test.Hspec
-- > import Test.Hspec.LeanCheck as LC
-- >
-- > import Data.List (sort)
-- >
-- > main :: IO ()
-- > main = hspec spec
-- >
-- > spec :: Spec
-- > spec = do
-- >   describe "sort" $ do
-- >     it "is idempotent" $
-- >       LC.property $ \xs -> sort (sort xs :: [Int]) == sort xs
-- >     it "is identity" $ -- not really
-- >       LC.property $ \xs -> sort (xs :: [Int]) == xs
--
-- The output for the above program is:
--
-- > $ ./eg/minimal
-- >
-- > sort
-- >   is idempotent
-- >   is identity FAILED [1]
-- >
-- > Failures:
-- >
-- >   eg/minimal.hs:17:5:
-- >   1) sort is identity
-- >        [1,0]
-- >
-- >   To rerun use: --match "/sort/is identity/"
-- >
-- > 2 examples, 1 failure
--
-- Please see the documentation of
-- "Test.LeanCheck" and Hspec
-- for more details.
module Test.Hspec.LeanCheck
  ( property
  , propertyFor
  , prop
  , Property
  , module Test.LeanCheck
  )
where

import Test.Hspec.Core.Spec
import Test.LeanCheck
import Test.LeanCheck.Core (resultiers)
import Control.Exception (try)
import System.IO.Unsafe (unsafePerformIO) -- LeanCheck is pure
import qualified Test.HUnit.Lang as HUnit
import Data.Maybe (fromMaybe)

-- | A LeanCheck property.  See 'property', 'propertyFor' and 'prop'.
data Property = Ok
              | Failed String

-- | Like 'property' but allows setting the maximum number of tests.
--
-- > spec :: Spec
-- > spec = do
-- >   describe "thing" $ do
-- >    it "is so and so" $ propertyFor 100 $ \... -> ...
-- >    it "is like this" $ propertyFor 200 $ \... -> ...
-- >    it "does a thing" $ propertyFor 300 $ \... -> ...
-- >    ...
propertyFor :: Testable a => Int -> a -> Property
propertyFor m p = case counterExample m p of
  Nothing -> Ok
  Just ce -> Failed $ unwords ce
-- TODO: catch errors above

-- | Allows a LeanCheck 'Testable' property to appear in a Spec.
--   Like so:
--
-- > spec :: Spec
-- > spec = do
-- >   describe "thing" $ do
-- >    it "is so and so" $ property $ \x... -> ...
-- >    it "is like this" $ property $ \y... -> ...
-- >    ...
property :: Testable a => a -> Property
property = propertyFor 200

-- | Allows a named LeanCheck 'Testable' property to appear in a Spec.
--
-- > prop "does so and so" $ ...
--
-- is a shortcut for
--
-- > it "does so an so" $ property $ ...
--
-- > spec :: Spec
-- > spec = do
-- >   describe "thing" $ do
-- >    prop "is so and so" $ \x... -> ...
-- >    prop "is like this" $ \y... -> ...
-- >    ...
prop :: Testable a => String -> a -> Spec
prop s = it s . property

instance Example Property where
  evaluateExample p _ _ _ = return . Result ""
                          $ case p of
                            Ok -> Success
                            Failed s -> Failure Nothing (Reason s)

-- | Allows @should*@ to appear inside LeanCheck properties
--
--   Example:
--
-- > describe "sort" $ do
-- >   it "is idempotent" $
-- >     LC.property $ \xs -> sort (sort xs :: [Int]) `shouldBe` sort xs
instance Testable (IO a) where
  resultiers action = unsafePerformIO $ do
    r <- try action
    return . (:[]) . (:[]) $ case r of
      Right _ -> ([],True)
      Left (HUnit.HUnitFailure loc reason) ->
        case reason of
        HUnit.Reason s -> (["--", s],False)
        HUnit.ExpectedButGot prefix expected actual ->
          ([fromMaybe "" prefix, "--", "expected", expected, "but got", actual], False)
