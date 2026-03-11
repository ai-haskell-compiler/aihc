{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Criterion.Main
import           System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.CSS.Syntax.Tokens
import           Control.DeepSeq

-- We're benchmarking speed of processing of 1MB input.
-- So we repeat any input to fill 1MB text.

fill :: T.Text -> T.Text
fill t = T.take size $ T.replicate (size `div` T.length t + 1) t
    where size = 1000000

instance NFData NumericValue where rnf x = seq x ()
instance NFData Token where rnf x = seq x ()

fileBenchmarks :: NFData b => (a -> b) -> (T.Text -> a) -> IO [Benchmark]
fileBenchmarks f preprocess = do
    cwd <- getCurrentDirectory
    websites <- drop 2 <$> getDirectoryContents (cwd ++ "/benchmark/fixtures")
    forM websites $ \website -> do
        files <- drop 2 <$> getDirectoryContents (cwd ++ "/benchmark/fixtures/" ++ website)
        benchmarks <- forM files $ \file -> do
            body <- T.readFile $ cwd ++ "/benchmark/fixtures/" ++ website ++ "/" ++ file
            pure $ bench file $ nf f $ preprocess $ fill body

        pure $ bgroup website benchmarks

main :: IO ()
main = do
    tokenizeBenchmarks <- fileBenchmarks tokenize id
    serializeBenchmarks <- fileBenchmarks serialize tokenize
    serializeTokenizeBenchmarks <- fileBenchmarks (serialize . tokenize) id

    defaultMain [
        bgroup "tokenize" tokenizeBenchmarks,
        bgroup "serialize" serializeBenchmarks,
        bgroup "serialize/tokenize" serializeTokenizeBenchmarks,

        bgroup "tokenize"
            [ tBench' "whitespace" " "
            , bench "comment" $ nf tokenize $ "/*" <> fill " " <> "*/"
            , numBench 1000
            , numBench 10000
            , numBench 100000
            , numBench 1000000
            , tBench' "aaa.." "a"
            , tBench' "esc2" "\\ab"
            , tBench' "esc5" "\\abcde"
            , bench "url(aaa..)"   $ nf tokenize $ "url(" <> fill "a" <> ")"
            , bench "url(esc2)"    $ nf tokenize $ "url(" <> fill "\\ab" <> ")"
            , bench "badUrl(aaa..)"$ nf tokenize $ "url((" <> fill "a" <> ")"
            , bench "badUrl(esc2)" $ nf tokenize $ "url((" <> fill "\\ab" <> ")"
            , tBench ";"
            , tBench "||"
            , tBench "a;"
            , tBench "1;"
            , tBench "a:1;"
            , tBench "1234567890;"
            , tBench "z-index:1;"
            , tBench "#123456 "
            , tBench "#FFFFFF "
            ],

        bgroup "serialize"
            [ sBench "aaa"
            , sBench ";"
            , sBench "||"
            , sBench "1;"
            , sBench "1234567890;"
            , sBench "a:1;"
            , sBench "z-index:1;"
            ]
     ]
    where sBench n =
              let t = tokenize $ fill $ T.pack n in
              rnf t `seq` bench n $ nf serialize t
          tBench n = tBench' n n
          tBench' nm n = bench nm $ nf tokenize $ fill $ T.pack n
          numBench n = bench ("1e" ++ show n) $
              nf tokenize $ fill $ "1" <> T.replicate n "0" <> " "
