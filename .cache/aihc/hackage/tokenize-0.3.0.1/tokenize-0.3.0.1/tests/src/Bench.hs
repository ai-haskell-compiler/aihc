{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Bench where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.DeepSeq
import Criterion.Main

import System.Environment (getArgs, withArgs)

import qualified NLP.Tokenize.String as StrTok
import qualified NLP.Tokenize.Text as TextTok


readF :: FilePath -> IO Text
readF file = do
  bs <- BS.readFile file
  return $ TE.decodeLatin1 bs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: bench <input_text_file>"
    (f:rest) -> do
      plugTxt <- mapM readF [f]
      let plugStr = map T.unpack plugTxt
      deepseq plugStr $ withArgs rest $ defaultMain
        [ bgroup "tokenizing"
          [ bench "Native String Tokenizer" $ nf (map StrTok.tokenize) plugStr
          , bench "Native Text Tokenizer" $ nf (map TextTok.tokenize) plugTxt
          , bench "Text->Text based on String Tokenizer" $ nf (map strTokenizer) plugTxt
          , bench "String->String based on Text Tokenizer" $ nf (map txtTokenizer) plugStr
          ]
        ]

strTokenizer :: Text -> [Text]
strTokenizer txt = map T.pack (StrTok.tokenize $ T.unpack txt)

txtTokenizer :: String -> [String]
txtTokenizer str = map T.unpack (TextTok.tokenize $ T.pack str)
