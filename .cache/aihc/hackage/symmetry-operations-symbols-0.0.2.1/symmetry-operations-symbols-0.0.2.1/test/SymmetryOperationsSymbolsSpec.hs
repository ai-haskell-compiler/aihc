module SymmetryOperationsSymbolsSpec where

import Test.Hspec
import Text.ParserCombinators.Parsec

import Data.Matrix.SymmetryOperationsSymbols
import Data.Matrix.SymmetryOperationsSymbols.Parser
import Data.Matrix.SymmetryOperationsSymbols.Symbol

import Data.Matrix
import Data.Matrix.AsXYZ

import TestDataOthers
import TestDataHex

parseSymmetryOperation :: String -> Either String (Symbol,String,String,String)
parseSymmetryOperation st = case parse (symmetryElement return) st st of
  Right a -> Right a
  Left m -> Left "Parse Error."

toMatrix' st = case toMatrix st of
  Left _ -> Nothing
  Right a -> Just a

toMatrixHex' st = case toMatrixHex st of
  Left _ -> Nothing
  Right a -> Just a

spec :: Spec
spec = do
   describe "Data.Matrix.SymmetryOperationsSymbols.toMatrix" $ do
     mapM_ (toMatrixTest toMatrix') $ zip [0..] testDataOthers

   describe "Data.Matrix.SymmetryOperationsSymbols.toMatrixHex" $ do
     mapM_ (toMatrixTest toMatrixHex') $ zip [0..] testDataHex

   describe "Data.Matrix.SymmetryOperationsSymbols.fromMatrix" $ do
     mapM_ (fromMatrixTest) $ zip [0..] testDataOthers

   describe "Data.Matrix.SymmetryOperationsSymbols.fromMatrix" $ do
     mapM_ (fromMatrixTest) $ zip [0..] testDataHex

mat34 = submatrix 1 3 1 4

toMatrixTest f (n,(a,b)) = it (show n ++ " : '" ++ b ++ "' -> '" ++ a ++ "'") $ do
  -- (prettyXYZ <$> f b) `shouldBe` (Just . prettyXYZ . fromXYZ $ a)
  (mat34 <$> f b) `shouldBe` (Just . mat34 . fromXYZ $ a)

fromMatrixTest (n,(a,b)) = it (show n ++ " : '" ++ a ++ "' -> '" ++ b ++ "'") $ do
  (parseSymmetryOperation =<< (fromMatrix' . fromXYZ $ a)) `shouldBe` (parseSymmetryOperation b)
