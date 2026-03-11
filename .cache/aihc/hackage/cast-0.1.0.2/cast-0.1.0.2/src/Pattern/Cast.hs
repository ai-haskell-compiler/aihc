{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Pattern.Cast
  ( Cast (..)
  ) where

-- | The 'Cast' describes values that can be converted from one type to another
class Cast a b where
  cast :: a -> b

instance Cast a a where
  cast = id
