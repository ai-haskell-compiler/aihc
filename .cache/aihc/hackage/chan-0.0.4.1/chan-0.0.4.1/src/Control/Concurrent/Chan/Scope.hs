{-# LANGUAGE
    DataKinds
  , KindSignatures
  #-}

module Control.Concurrent.Chan.Scope where


data Scope = Read | Write | ReadWrite


class Readable (a :: Scope) where
instance Readable 'Read where
instance Readable 'ReadWrite where


class Writable (a :: Scope) where
instance Writable 'Write where
instance Writable 'ReadWrite where
