{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}

module Data.EDN.AST.Types.Tagged
  ( Tagged(..)
  , stripTag
  ) where

import Data.Data (Data)

data Tagged tag a
  = Tagged !tag !tag !a -- ^ @ #prefix/tag value @
  | NoTag !a            -- ^ @             value @
  deriving (Eq, Ord, Show, Data, Functor)

stripTag :: Tagged tag a -> a
stripTag (NoTag value)           = value
stripTag (Tagged _ns _tag value) = value
