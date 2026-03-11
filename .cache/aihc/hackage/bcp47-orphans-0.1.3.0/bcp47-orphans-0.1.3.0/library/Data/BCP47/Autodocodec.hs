{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Autodocodec () where

import Autodocodec
import Data.BCP47
import Data.Bifunctor (first)
import Data.Text (unpack)

instance HasCodec BCP47 where
  codec =
    bimapCodec (first unpack . fromText) toText textCodec
      <?> "BCP47 language tag, such as 'en-US' or 'zh-Hant-TW'"
