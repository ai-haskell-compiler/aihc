{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Text.Html.Email.Validate
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

main :: IO ()
main = defaultMain [
      bgroup "Valid emails" $ 
          map (\email -> bench (unpack email) $ whnf isValidEmail email) validEmails
    , bgroup "Invalid emails" $ 
          map (\email -> bench (unpack email) $ whnf isValidEmail email) invalidEmails
    , bgroup "Long emails" $ 
          map (\email -> bench (unpack $ T.take 15 email) $ whnf isValidEmail email) longEmails
    ]

validEmails :: [Text]
validEmails = [ "niceandsimple@example.com"
              , "very.common@example.com"
              , "a.little.lengthy.but.fine@dept.example.com"
              , "disposable.style.email.with+symbol@example.com"
              , "other.email-with-dash@example.com"
              , "admin@mailserver1"
              , "!#$%&'*+-/=?^_`{}|~@example.org"
              , "john..doe@example.com"
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
                , "john.doe@example..com"]

longEmails :: [Text]
longEmails = map longEmail [3 .. 20]

longEmail :: Int -> Text
longEmail n = T.concat ["john.doe@", T.replicate n "longey-"]
