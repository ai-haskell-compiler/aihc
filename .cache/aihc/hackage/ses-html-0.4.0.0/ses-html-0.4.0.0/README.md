ses-html 
========
![Hackage](https://img.shields.io/hackage/v/ses-html.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/ses-html.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
![Build Status](https://img.shields.io/circleci/project/dmjio/ses-html.svg)

Send [blaze-html](http://jaspervdj.be/blaze/) emails via the AWS SES API using http-streams
<http://hackage.haskell.org/package/ses-html>
### Example
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Network.SES ( sendEmailBlaze
                   , PublicKey(..)
                   , SecretKey(..)
                   , SESResult(..)
                   , SESError
                   , Region(USEast1)
                   )

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = sendMail >>= \case
         Error _ -> putStrLn "There was an error :("
         Success -> putStrLn "Email sent successfully!"

sendMail :: IO SESREsult
sendMail = sendEmailBlaze publicKey secretKey region from to subject html
 where
   publicKey = PublicKey "public key goes here"
   secretKey = SecretKey "secret key goes here"
   region    = USEast1
   from    = "support@solidtranslate.com"
   to      = ["david@solidtranslate.com"]
   subject = "Test Subject"
   html = H.html $ do
            H.body $ do
               H.img H.! A.src "http://haskell-lang.org/static/img/logo.png"
               H.h1 "Html email! Hooray"
```
### Result
<img src="http://i.imgur.com/AREDbNk.png"></img>
