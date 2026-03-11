{-# LANGUAGE OverloadedStrings #-}

import Data.Either (isLeft)

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Network.HTTP.Client (Request, getUri, parseRequest)
import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP.Client.Overrides
import Network.HTTP.Client.Overrides.Internal.RequestOverrides
import Network.HTTP.Client.Overrides.Internal.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ urlParserUnitTests
    , matchUnitTests
    , requestOverrideUnitTests
    ]

urlParserUnitTests :: TestTree
urlParserUnitTests = testGroup "URL parser"
    [ testCase "URL" . isValidURL "https://test.domain:8443/path" $ URL
        (Just True)
        (Just "test.domain")
        (Just 8443)
        (Just "/path")
    , testCase "HTTP scheme" $ do
        isValidURL "http://" $ schemeOnly False
    , testCase "HTTPS scheme" $ do
        isValidURL "https://" $ schemeOnly True
    , testCase "No scheme" . isValidURL "test.domain:8443/path" $ URL
        Nothing
        (Just "test.domain")
        (Just 8443)
        (Just "/path")
    , testCase "Unsupported scheme" $ do
        isInvalidURL "ftp://test.domain"
    , testCase "Invalid scheme" $ do
        isInvalidURL "x^y://test.domain"
    , testCase "Host" $ do
        isValidURL "test.domain" $ hostOnly "test.domain"
    , testCase "No host" . isValidURL "https://:8443/path" $ URL
        (Just True)
        Nothing
        (Just 8443)
        (Just "/path")
    , testCase "Invalid host" $ do
        isInvalidURL "https://test^domain"
    , testCase "Port" $ do
        isValidURL ":8443" $ portOnly 8443
    , testCase "Empty port" $
        isValidURL "test.domain:" $ hostOnly "test.domain"
    , testCase "No port" . isValidURL "https://test.domain/path" $ URL
        (Just True)
        (Just "test.domain")
        Nothing
        (Just "/path")
    , testCase "Invalid port" $ do
        isInvalidURL "test.domain:https"
    , testCase "Path" $ do
        isValidURL "/path" $ pathOnly "/path"
    , testCase "Root path" . isValidURL "test.domain/" $ URL
        Nothing
        (Just "test.domain")
        Nothing
        (Just "/")
    , testCase "No path" . isValidURL "https://test.domain:8443" $ URL
        (Just True)
        (Just "test.domain")
        (Just 8443)
        Nothing
    , testCase "Invalid path" $ do
        isInvalidURL "test.domain/`"
    ]

matchUnitTests :: TestTree
matchUnitTests = testGroup "Request overrides"
    [ testCase "URL" $
        isMatch "https://test.domain:8443/path" standardURL
    , testCase "URL with prefixed path" $
        isMatch "https://test.domain:8443/path/x" standardURL
    , testCase "Different scheme" $
        noMatch "http://test.domain:8443/path" standardURL
    , testCase "Different host" $
        noMatch "https://toast.domain:8443/path" standardURL
    , testCase "Different port" $
        noMatch "https://test.domain:8000/path" standardURL
    , testCase "Different path" $
        noMatch "https://test.domain/x/path" standardURL
    , testCase "Scheme only" $
        isMatch "https://test.domain:8443/path" (schemeOnly True)
    , testCase "Host only" $
        isMatch "https://test.domain:8443/path" (hostOnly "test.domain")
    , testCase "Port only" $
        isMatch "https://test.domain:8443/path" (portOnly 8443)
    , testCase "Path only" $
        isMatch "https://test.domain:8443/path" (pathOnly "/path")
    ]
  where
    standardURL = URL
        (Just True)
        (Just "test.domain")
        (Just 8443)
        (Just "/path")

requestOverrideUnitTests :: TestTree
requestOverrideUnitTests = testGroup "Request overrides"
    [ testCase "Set scheme" $ do
        r  <- parseRequest "https://test.domain:8443/path"
        r' <- parseRequest "http://test.domain:8443/path"
        assertOverride r r' . overrideAny $ schemeOnly False
    , testCase "Set host" $ do
        r  <- parseRequest "https://test.domain:8443/path"
        r' <- parseRequest "https://test.xyz:8443/path"
        assertOverride r r' . overrideAny $ hostOnly "test.xyz"
    , testCase "Set port" $ do
        r  <- parseRequest "https://test.domain:8443/path"
        r' <- parseRequest "https://test.domain:8000/path"
        assertOverride r r' . overrideAny $ portOnly 8000
    , testCase "Set path" $ do
        r  <- parseRequest "https://test.domain:8443"
        r' <- parseRequest "https://test.domain:8443/xyz"
        assertOverride r r' . overrideAny $ pathOnly "/xyz"
    , testCase "Add prefix to path" $ do
        r  <- parseRequest "https://test.domain:8443/path"
        r' <- parseRequest "https://test.domain:8443/xyz/path"
        assertOverride r r' . overrideAny $ pathOnly "/xyz/"
    , testCase "Substitute path" $ do
        r  <- parseRequest "https://test.domain:8443/abc"
        r' <- parseRequest "https://test.domain:8443/xyz"
        assertOverride r r' . overridePath "/abc" $ pathOnly "/xyz"
    , testCase "Substitute path prefix" $ do
        r  <- parseRequest "https://test.domain:8443/abc/path"
        r' <- parseRequest "https://test.domain:8443/xyz/path"
        assertOverride r r' . overridePath "/abc/" $ pathOnly "/xyz/"
    ]

-------------------
-- Helper functions
-------------------

isValidURL :: Text -> URL -> Assertion
isValidURL str url = parseURL str @?= Right url

isInvalidURL :: Text -> Assertion
isInvalidURL str = isLeft (parseURL str) @?= True

isMatch :: String -> URL -> Assertion
isMatch request url = do
    r <- parseRequest request
    r `matches` url @?= True

noMatch :: String -> URL -> Assertion
noMatch request url = do
    r <- parseRequest request
    r `matches` url @?= False

schemeOnly :: Bool -> URL
schemeOnly x = URL (Just x) Nothing Nothing Nothing

hostOnly :: BS.ByteString -> URL
hostOnly x = URL Nothing (Just x) Nothing Nothing

portOnly :: Int -> URL
portOnly x = URL Nothing Nothing (Just x) Nothing

pathOnly :: BS.ByteString -> URL
pathOnly x = URL Nothing Nothing Nothing (Just x)

assertOverride :: Request -> Request -> RequestOverride -> Assertion
assertOverride r r' o = getUri (overrideRequest r o) @?= getUri r'

overrideAny :: URL -> RequestOverride
overrideAny = RequestOverride (URL Nothing Nothing Nothing Nothing)

overridePath :: BS.ByteString -> URL -> RequestOverride
overridePath p = RequestOverride . URL Nothing Nothing Nothing $ Just p
