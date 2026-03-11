
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Text.ToTextBuilder where

import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int       (decimal)
import           Data.Text.Lazy.Builder.RealFloat (realFloat)

import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Encoding          as TL

class ToTextBuilder a where
  toTextBuilder ::  a -> Builder

instance ToTextBuilder Int where
    toTextBuilder = decimal

instance ToTextBuilder Integer where
    toTextBuilder = decimal

instance ToTextBuilder Float where
    toTextBuilder = realFloat

instance ToTextBuilder Double where
    toTextBuilder = realFloat

instance ToTextBuilder Char where
    toTextBuilder = singleton

instance ToTextBuilder T.Text where
    toTextBuilder = fromText

instance ToTextBuilder TL.Text where
    toTextBuilder = fromLazyText

instance ToTextBuilder String where
    toTextBuilder = fromString

