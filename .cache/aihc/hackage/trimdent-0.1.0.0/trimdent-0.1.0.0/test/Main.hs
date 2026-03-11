module Main (main) where

import Data.Char (isSpace)
import Data.List (isInfixOf)
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Text.RawString.QQ (r)
import Trimdent (trim, trimdent)

main :: IO ()
main = Hspec.hspec tests

tests :: Hspec.Spec
tests = modifyMaxSuccess (const 1000) $ do
  describe "trim" $ do
    prop "preserves content" prop_trim_preservesContent
    prop "commutes with reverse" prop_trim_commutesWithReverse
    prop "result doesnt start with whitespace" prop_trim_removesStartSpace
  describe "trimdent" $ do
    prop "preserves nonwhitespace content" prop_trimdent_preservesNonwhitespaceContent
    prop "result doesnt start with whitespace" prop_trimdent_removesStartSpace
    it "trimdents the README string" itTrimdentsReadmeString
    it "trimdents a headerless string" itTrimdentsHeaderlessString

prop_f_removesStartSpace :: (String -> String) -> String -> Bool
prop_f_removesStartSpace f s =
  case f s of
    "" -> True
    c : _ -> not $ isSpace c

prop_trim_preservesContent :: String -> Bool
prop_trim_preservesContent s = trim s `isInfixOf` s

prop_trim_commutesWithReverse :: String -> Bool
prop_trim_commutesWithReverse s = (trim . reverse) s == (reverse . trim) s

prop_trim_removesStartSpace :: String -> Bool
prop_trim_removesStartSpace = prop_f_removesStartSpace trim

prop_trimdent_preservesNonwhitespaceContent :: String -> Bool
prop_trimdent_preservesNonwhitespaceContent s =
  dropWhitespace (trimdent s)
    `isInfixOf` dropWhitespace s
 where
  dropWhitespace = filter (not . isSpace)

prop_trimdent_removesStartSpace :: String -> Bool
prop_trimdent_removesStartSpace = prop_f_removesStartSpace trimdent

itTrimdentsReadmeString :: Hspec.Expectation
itTrimdentsReadmeString =
  trimdent
    [r|func add(x int, y int) int {
                return x + y
              }
              |]
    `shouldBe` "func add(x int, y int) int {\n\
               \  return x + y\n\
               \}"

itTrimdentsHeaderlessString :: Hspec.Expectation
itTrimdentsHeaderlessString =
  trimdent
    "\n\
    \   func add(x int, y int) int {\n\
    \     return x + y\n\
    \   }"
    `shouldBe` "func add(x int, y int) int {\n\
               \  return x + y\n\
               \}"
