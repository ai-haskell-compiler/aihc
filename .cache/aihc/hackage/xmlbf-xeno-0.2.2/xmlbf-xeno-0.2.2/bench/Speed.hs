{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Benchmark speed.

module Main where

import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import qualified Data.ByteString as S
import           GHC.Generics
import           Text.XML.Light as XML
import qualified Xmlbf.Xeno as Xeno

main :: IO ()
main =
  defaultMain
    [ env
        (S.readFile "data/books-4kb.xml")
        (\input -> bench "4KB" (whnf Xeno.fromRawXml input))
    , env
        (S.readFile "data/text-31kb.xml")
        (\input -> bench "31KB" (whnf Xeno.fromRawXml input))
    , env
        (S.readFile "data/fabricated-211kb.xml")
        (\input -> bench "211KB" (whnf Xeno.fromRawXml input))
    ]

deriving instance Generic Content
deriving instance Generic Element
deriving instance Generic CData
deriving instance Generic CDataKind
deriving instance Generic QName
deriving instance Generic Attr
instance NFData Content
instance NFData Element
instance NFData CData
instance NFData CDataKind
instance NFData QName
instance NFData Attr
