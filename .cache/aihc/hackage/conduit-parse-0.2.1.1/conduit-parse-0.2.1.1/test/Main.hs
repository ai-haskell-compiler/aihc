{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Exception.Safe       as Exception
import           Control.Monad.Error.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit                 hiding (await, leftover)
import           Data.Conduit.List            hiding (peek)
import           Data.Conduit.Parser
import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck

import           Text.Parser.Combinators

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ unitTests
  -- , properties
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ awaitCase
  , peekCase
  , leftoverCase
  , errorCase
  , alternativeCase
  , catchCase
  , parsingCase
  ]

awaitCase :: TestTree
awaitCase = testCase "await" $ do
  i <- runResourceT . runConduit $ sourceList [1 :: Int] .| runConduitParser parser
  i @=? (1, Nothing)
  where parser = (,) <$> await <*> optional await

peekCase :: TestTree
peekCase = testCase "peek" $ do
  result <- runResourceT . runConduit $ sourceList [1 :: Int, 2] .| runConduitParser parser
  result @=? (Just 1, 1, 2, Nothing)
  where parser = (,,,) <$> peek <*> await <*> await <*> peek

leftoverCase :: TestTree
leftoverCase = testCase "leftover" $ do
  result <- runResourceT . runConduit $ sourceList [1 :: Int, 2, 3] .| runConduitParser parser
  result @?= (3, 2, 1)
  where parser = do
          (a, b, c) <- (,,) <$> await <*> await <*> await
          leftover a >> leftover b >> leftover c
          (,,) <$> await <*> await <*> await

errorCase :: TestTree
errorCase = testCase "error" $ do
  result1 <- Exception.try . runResourceT . runConduit $ sourceList [] .| runConduitParser parser
  result2 <- Exception.try . runResourceT . runConduit $ sourceList [] .| runConduitParser (parser <?> "Name1")
  result3 <- Exception.try . runResourceT . runConduit $ sourceList [] .| runConduitParser ((parser <?> "Name1") <?> "Name2")
  result1 @?= Left (Unexpected "ERROR")
  result2 @?= Left (NamedParserException "Name1" $ Unexpected "ERROR")
  result3 @?= Left (NamedParserException "Name2" $ NamedParserException "Name1" $ Unexpected "ERROR")
  where parser = unexpected "ERROR" >> return (1 :: Int)


alternativeCase :: TestTree
alternativeCase = testCase "alternative" $ do
  result <- runResourceT . runConduit $ sourceList [1 :: Int, 2, 3] .| runConduitParser parser
  result @?= (1, 2, Nothing)
  where parser = do
          a <- parseInt 1 <|> parseInt 2
          b <- parseInt 1 <|> parseInt 2
          c <- optional $ parseInt 1 <|> parseInt 2
          await
          eof
          return (a, b, c)
        parseInt :: (Monad m) => Int -> ConduitParser Int m Int
        parseInt i = do
          a <- await
          if i == a then return a else unexpected ("Expected " ++ show i ++ ", got " ++ show a)

catchCase :: TestTree
catchCase = testCase "catch" $ do
  result <- runResourceT . runConduit $ sourceList [1 :: Int, 2] .| runConduitParser parser
  result @?= (1, 2)
  where parser = catchError (await >> await >> throwError (Unexpected "ERROR")) . const $ (,) <$> await <*> await

parsingCase :: TestTree
parsingCase = testCase "parsing" $ do
  result <- runResourceT . runConduit $ sourceList [1 :: Int, 2] .| runConduitParser parser
  result @?= (1, 2)
  where parser = (,) <$> await <*> await <* notFollowedBy await <* eof
