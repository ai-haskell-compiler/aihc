{-# LANGUAGE TypeFamilies #-}
module NewtypeInstance where

data family T a
newtype instance T Char = TC Bool
