-- | Direction of reading (the compressed data) and writing (the uncompressed data)

module Exomizer.Data.Direction
  ( -- * Direction
    Direction(..)
  , ReadWrite(..)
  , forward
  , backwards
  , ifForward
  , ifBackwards
  ) where

-- | Direction of reading (the compressed data) and writing (the uncompressed data)
--
-- > readDirection `ReadWrite` writeDirection
--
-- @since 1.0.0
data ReadWrite
  = Direction
    `ReadWrite`
    Direction

-- | Direction
--
-- @since 1.0.0
data Direction
  = Forward -- ^ Starting at the lowest and ending the byte after the highest
  | Backwards -- ^ Starting one byte after the highest and ending on the lowest
  deriving (Eq)

-- | Forward: both read and write
--
-- @since 1.0.0
forward :: ReadWrite
{-# INLINE forward #-}
forward = Forward `ReadWrite` Forward

-- | Backwards: both read and write
--
-- @since 1.0.0
backwards :: ReadWrite
{-# INLINE backwards #-}
backwards = Backwards `ReadWrite` Backwards

-- | Utility: apply the function to the input only if the direction is forward
--
-- @since 1.0.0
ifForward :: Direction -> (a -> a) -> a -> a
{-# INLINABLE ifForward #-}
ifForward Forward   f a = f a
ifForward Backwards _ a = a

-- | Utility: apply the function to the input only if the direction is backwards
--
-- @since 1.0.0
ifBackwards :: Direction -> (a -> a) -> a -> a
{-# INLINABLE ifBackwards #-}
ifBackwards Forward   _ a = a
ifBackwards Backwards f a = f a
