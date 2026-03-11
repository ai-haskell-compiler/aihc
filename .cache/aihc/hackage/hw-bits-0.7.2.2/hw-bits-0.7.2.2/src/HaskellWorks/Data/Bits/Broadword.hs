{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.Broadword
  {-# DEPRECATED "Import the relevant module instead" #-}
  ( BW.Broadword(..)
  , BW.broadword
  , W64.lsb
  , W64.h
  , W64.l
  , W64.kBitDiff
  , W64.kBitDiffPos
  , W64.kBitDiffUnsafe
  ) where

import qualified HaskellWorks.Data.Bits.Broadword.Type   as BW
import qualified HaskellWorks.Data.Bits.Broadword.Word64 as W64
import qualified HaskellWorks.Data.Bits.Word64           as W64
