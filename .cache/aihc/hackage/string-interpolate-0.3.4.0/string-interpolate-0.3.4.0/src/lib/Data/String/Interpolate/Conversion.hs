-- |
-- Module      : Data.String.Interpolate.Conversion
-- Copyright   : (c) William Yao, 2019-2020
-- License     : BSD-3
-- Maintainer  : williamyaoh@gmail.com
-- Stability   : experimental
-- Portability : POSIX

{-# OPTIONS -Wno-orphans           #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.String.Interpolate.Conversion
  ( IsCustomSink, InterpSink(..), Interpolatable(..)
  , bsToTextBuilder, lbsToTextBuilder, encodeCharUTF8, proxyWrapper
  )
where

import Data.String ( IsString, fromString )

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as LB
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT hiding ( singleton )
import qualified Data.Text.Lazy.Builder  as LT

import qualified "utf8-string" Data.ByteString.Lazy.UTF8 as LUTF8
import qualified "utf8-string" Data.ByteString.UTF8      as UTF8

import Data.String.Interpolate.Conversion.ByteStringSink ()
import Data.String.Interpolate.Conversion.Classes
import Data.String.Interpolate.Conversion.Encoding
import Data.String.Interpolate.Conversion.TextSink       ()

-- Remove some imports above GHC 8.8.X
#if MIN_VERSION_base(4,13,0)
#else
import "base" Text.Show ( ShowS, showChar, showString )
#endif

instance (IsCustomSink str ~ 'False, IsString str) => InterpSink 'False str where
  type Builder 'False str = ShowS

  ofString _ = B . showString
  build _ (B f) (B g) = B $ f . g
  finalize _ = fromString . ($ "") . unB

instance {-# OVERLAPPABLE #-} (Show src, IsString dst, IsCustomSink dst ~ 'False) => Interpolatable 'False src dst where
  interpolate _ = B . shows
instance {-# OVERLAPS #-} (IsString dst, IsCustomSink dst ~ 'False) => Interpolatable 'False Char dst where
  interpolate _ = B . showChar
instance {-# OVERLAPS #-} (IsString dst, IsCustomSink dst ~ 'False) => Interpolatable 'False String dst where
  interpolate _ = B . showString
instance {-# OVERLAPS #-} (IsString dst, IsCustomSink dst ~ 'False) => Interpolatable 'False T.Text dst where
  interpolate _ = B . showString . T.unpack
instance {-# OVERLAPS #-} (IsString dst, IsCustomSink dst ~ 'False) => Interpolatable 'False LT.Text dst where
  interpolate _ = B . showString . LT.unpack
instance {-# OVERLAPS #-} (IsString dst, IsCustomSink dst ~ 'False) => Interpolatable 'False LT.Builder dst where
  interpolate _ = B . showString . LT.unpack . LT.toLazyText
instance {-# OVERLAPS #-} (IsString dst, IsCustomSink dst ~ 'False) => Interpolatable 'False B.ByteString dst where
  interpolate _ = B . showString . UTF8.toString
instance {-# OVERLAPS #-} (IsString dst, IsCustomSink dst ~ 'False) => Interpolatable 'False LB.ByteString dst where
  interpolate _ = B . showString . LUTF8.toString
instance {-# OVERLAPS #-} (IsString dst, IsCustomSink dst ~ 'False) => Interpolatable 'False LB.Builder dst where
  interpolate _ = B . showString . LUTF8.toString . LB.toLazyByteString
