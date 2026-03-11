{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
module CoerceCompat where

import Data.Coerce (Coercible, coerce)

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
_ #. x = coerce x

(#..) :: Coercible b c => (b -> c) -> (i -> a -> b) -> (i -> a -> c)
_ #.. x = coerce x

infixr 9 #., #..
{-# INLINE (#.) #-}
{-# INLINE (#..) #-}
