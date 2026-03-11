{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN10Spec (spec) where

import           Data.ISBN.ISBN10
import           Data.ISBN.ISBN13 ( unsafeToISBN13 )

import           Data.Text        ( pack )
import           Test.Hspec


spec :: Spec
spec = do
    describe "Testing validity of ISBN-10 check digit characters" $ do
        let test_isValidISBN10CheckDigit char expecting =
              it ("can " ++ (if expecting then "accept" else "reject") ++ " '" ++ [char] ++ "'") $
                  isValidISBN10CheckDigit char `shouldBe` expecting

        test_isValidISBN10CheckDigit 'X' True
        test_isValidISBN10CheckDigit '1' True
        test_isValidISBN10CheckDigit '2' True
        test_isValidISBN10CheckDigit '3' True
        test_isValidISBN10CheckDigit '4' True
        test_isValidISBN10CheckDigit '5' True
        test_isValidISBN10CheckDigit '6' True
        test_isValidISBN10CheckDigit '7' True
        test_isValidISBN10CheckDigit '8' True
        test_isValidISBN10CheckDigit '9' True
        test_isValidISBN10CheckDigit '0' True
        test_isValidISBN10CheckDigit 'C' False
        test_isValidISBN10CheckDigit 'F' False
        test_isValidISBN10CheckDigit 'a' False
        test_isValidISBN10CheckDigit 'x' False
        test_isValidISBN10CheckDigit '-' False
        test_isValidISBN10CheckDigit '_' False
        test_isValidISBN10CheckDigit '*' False
        test_isValidISBN10CheckDigit '!' False
        test_isValidISBN10CheckDigit ' ' False

    describe "Testing ISBN-10 check digit confirmation" $ do
        let test_confirmISBN10CheckDigit isbn10string expecting =
              it ("can " ++ (if expecting then "confirm" else "reject ") ++ " '" ++ isbn10string ++ "'") $
                  confirmISBN10CheckDigit (pack isbn10string) `shouldBe` expecting

        test_confirmISBN10CheckDigit "0306406152" True
        test_confirmISBN10CheckDigit "030640615X" False
        test_confirmISBN10CheckDigit "0345816021" True
        test_confirmISBN10CheckDigit "080701429X" True
        test_confirmISBN10CheckDigit "0060899220" True
        test_confirmISBN10CheckDigit "2222222222" True
        test_confirmISBN10CheckDigit "4444444444" True
        test_confirmISBN10CheckDigit "9999999999" True
        test_confirmISBN10CheckDigit "999999999X" False

    describe "Testing ISBN-10 checking" $ do
        let test_isISBN10 isbn10Val expecting =
              it (concat
                    [ "can equate: isISBN10 ("
                    , show isbn10Val
                    , ")"
                    , spacer isbn10Val
                    , " == "
                    , show expecting
                    ]) $
                  isISBN10 isbn10Val `shouldBe` expecting
            spacer val = take (22 - (length $ show val)) $ cycle " "

        test_isISBN10 (unsafeToISBN10 "0345816021") True
        test_isISBN10 (unsafeToISBN13 "9780345816023") False

    describe "Testing ISBN-10 validation with errors" $ do
        let test_validateISBN10 isbn10 expecting =
                it (concat
                        [ "can "
                        , case expecting of Right _ -> "validate"; Left _ -> "reject  "
                        , " '"
                        , isbn10
                        , "'"
                        , take (14 - length isbn10) $ cycle " "
                        , "as '"
                        , show expecting
                        , "'"
                        ]) $
                    validateISBN10 (pack isbn10) `shouldBe` expecting

        test_validateISBN10 "0-345-81602-1" (Right $ unsafeToISBN10 "0345816021")
        test_validateISBN10 "0345816021"    (Right $ unsafeToISBN10 "0345816021")
        test_validateISBN10 "080701429X"    (Right $ unsafeToISBN10 "080701429X")
        test_validateISBN10 "0-345-81602-0" (Left ISBN10InvalidCheckDigit)
        test_validateISBN10 "0-345-X1602-0" (Left ISBN10IllegalCharactersInBody)
        test_validateISBN10 "A-345-X1602-0" (Left ISBN10IllegalCharactersInBody)
        test_validateISBN10 "A2345-X1602-0" (Left ISBN10InvalidInputLength)
        test_validateISBN10 "345-81602-1"   (Left ISBN10InvalidInputLength)
