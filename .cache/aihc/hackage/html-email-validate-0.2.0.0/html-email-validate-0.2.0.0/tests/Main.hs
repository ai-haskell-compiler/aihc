{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Regex.PCRE
import Control.Monad
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Test.QuickCheck
import Data.Monoid

import Text.Html.Email.Validate

main :: IO ()
main = hspec $ do
    describe "Checking the correctness of parsing" $ do
        forM_ validEmails $ \(raw,parsed) -> do
            it (unpack raw) $ do
                parseEmail raw  `shouldBe` Right parsed

    describe "Checking against the regex" $ do
        describe "Emails from wikipedia:" $ do
            forM_ (map unpack emails) $ \email -> do
                it email $ do
                    testEmail email
        describe "QuickCheck property:" $ do
            modifyMaxSuccess (const 100000) $ prop "random@random:" $
                \local domain -> testEmail $ local ++ "@" ++ domain

        {-describe "Very long label:" $ do-}
            {-it "really long" $ isValidEmail ("j@aa" <> T.replicate 10000000 "-a") `shouldBe` False-}

testEmail email = (isValidEmail $ pack email) `shouldBe` validateEmailRegex email
            

validateEmailRegex :: String -> Bool
validateEmailRegex email
    | any (== '\n') email = False -- For some reason when using Text.Regex.PCRE
                                  -- the javascript regex matches '\n' even 
                                  -- though when the same regex is used in 
                                  -- javascript it doesn't
    | otherwise = email =~ emailRegex

emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$" :: String

emails :: [Text]
emails = map fst validEmails ++ invalidEmails

validEmails :: [(Text, EmailAddress)]
validEmails = [ ("niceandsimple@example.com", EmailAddress "niceandsimple" "example.com")
              , ("very.common@example.com", EmailAddress "very.common" "example.com")
              , ("a.little.lengthy.but.fine@dept.example.com", EmailAddress "a.little.lengthy.but.fine" "dept.example.com")
              , ("disposable.style.email.with+symbol@example.com", EmailAddress "disposable.style.email.with+symbol" "example.com")
              , ("other.email-with-dash@example.com", EmailAddress "other.email-with-dash" "example.com")
              , ("admin@mailserver1", EmailAddress "admin" "mailserver1")
              , ("!#$%&'*+-/=?^_`{}|~@example.org", EmailAddress "!#$%&'*+-/=?^_`{}|~" "example.org")
              , ("john..doe@example.com", EmailAddress "john..doe" "example.com")
              , let domain = "a" <> T.replicate 31 "-a"
                in ("j@" <> domain, EmailAddress "j" domain) -- 63 characters in one label (maximum)
              ]

invalidEmails :: [Text]
invalidEmails = [ "\"much.more unusual\"@example.com"
                , "\"very.unusual.@.unusual.com\"@example.com"
                , "\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"
                , "\"()<>[]:,;@\\\"!#$%&'*+-/=?^_`{}| ~.a\"@example.org","\" \"@example.org"
                , "\252\241\238\231\248\240\233@example.com"
                , "\252\241\238\231\248\240\233@\252\241\238\231\248\240\233.com"
                , "Abc.example.com"
                , "A@b@c@example.com","a\"b(c)d,e:f;g<h>i[j\\k]l@example.com"
                , "just\"not\"right@example.com"
                , "this is\"not\allowed@example.com"
                , "this\\ still\\\"not\\\\allowed@example.com"
                , "john.doe@example..com"
                , "j@aa" <> T.replicate 31 "-a" -- 64 characters in one label (just out of limit)
                ]

