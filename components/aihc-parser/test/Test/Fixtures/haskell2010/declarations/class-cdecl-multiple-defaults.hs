module ClassMultipleDefaults where

class C a where
  foo :: a -> a
  foo = id

  bar :: a -> Int
  bar x = 42
