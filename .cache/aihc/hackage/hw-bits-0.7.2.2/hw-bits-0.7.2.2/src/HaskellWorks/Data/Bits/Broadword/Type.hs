{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.Broadword.Type
  ( Broadword(..)
  , broadword
  ) where

import Control.DeepSeq
import Data.Word
import GHC.Generics

newtype Broadword a = Broadword a deriving (Eq, Show, Functor, Generic, NFData)

broadword :: Broadword Word64 -> Word64
broadword (Broadword a) = a
{-# INLINE broadword #-}
