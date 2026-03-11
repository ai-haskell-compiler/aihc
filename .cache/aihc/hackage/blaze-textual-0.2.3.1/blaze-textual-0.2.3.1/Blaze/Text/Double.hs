{-# LANGUAGE CPP #-}

-- Module:      Blaze.Text.Double
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a Double as a lazy 'L.ByteString'.

module Blaze.Text.Double
    (
      float
    , double
    ) where

#ifdef NATIVE
import Blaze.Text.Double.Native
#else
import Blaze.ByteString.Builder (Builder, fromByteString)
import Data.Double.Conversion.ByteString (toShortest)

float :: Float -> Builder
float = double . realToFrac

double :: Double -> Builder
double f = fromByteString (toShortest f)
#endif
