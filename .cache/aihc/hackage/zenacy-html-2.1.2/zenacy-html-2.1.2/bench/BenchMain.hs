{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Char
import Zenacy.HTML.Internal.Lexer
import Zenacy.HTML.Internal.Token
import Criterion.Main
import Control.Monad
import Control.Monad.ST
import Data.ByteString
  ( ByteString
  )
import qualified Data.ByteString as S
import Data.Default
  ( Default(..)
  )
import Text.RawString.QQ

main :: IO ()
main = do
  putStrLn $ "data size: " ++ show (S.length (htmlBuild 1000) `div` 1024) ++ "KB"
  defaultMain
    [ env setupEnv $ \ ~x -> bgroup "a"
      [ bench "1" $ whnf countTokens x
      ]
    ]

setupEnv = do
  pure $ htmlBuild 1000

countTokens :: ByteString -> Int
countTokens s =
  runST $ do
    lexerNew def { lexerOptionInput = s } >>= \case
      Left x -> do
        error $ bcUnpack x
      Right x -> do
        go x 0
  where
    go x a = do
      lexerNext x
      n <- tokenCount (lexerToken x)
      if n == 0
         then pure a
         else do
           eof <- tokenHasEOF (lexerToken x)
           if eof
              then pure $ a + n
              else go x $ a + n

htmlData = [r|<html><body><a href="https://example.com">sjdkhfljksdhfjshdlkjfhsdjkhfljskdhlfjshldjkfhsd</a></body></html>|]

htmlBuild x = S.concat $ take x $ repeat htmlData
