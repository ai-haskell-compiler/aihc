module Foreign.Ptr
  ( Ptr (..),
    castPtr,
  )
where

data Ptr a = Ptr

castPtr :: Ptr a -> Ptr b
castPtr _ = Ptr
