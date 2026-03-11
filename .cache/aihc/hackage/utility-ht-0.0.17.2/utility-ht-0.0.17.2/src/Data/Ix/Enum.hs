{- |
Implementations of 'Ix' methods in terms of 'Enum' methods.

For a type @T@ of class 'Enum' you can easily define an 'Ix' instance
by copying the following code into your module:

>import qualified Data.Ix.Enum as IxEnum
>
>instance Ix T where
>   range           = IxEnum.range
>   index           = IxEnum.index
>   inRange         = IxEnum.inRange
>   rangeSize       = IxEnum.rangeSize
>   unsafeIndex     = IxEnum.unsafeIndex
>   unsafeRangeSize = IxEnum.unsafeRangeSize

-}
module Data.Ix.Enum where

import qualified Data.Ix as Ix
import qualified GHC.Arr as Arr

{-# INLINE range #-}
{-# INLINE index #-}
{-# INLINE unsafeIndex #-}
{-# INLINE inRange #-}
{-# INLINE rangeSize #-}
{-# INLINE unsafeRangeSize #-}

{- |
>>> range ('x','z')
"xyz"
>>> range (LT,GT)
[LT,EQ,GT]
-}
range :: Enum a => (a, a) -> [a]

{- |
>>> index ('a','z') 'e'
4
-}
index :: Enum a => (a, a) -> a -> Int

{- |
>>> unsafeIndex ('a','z') 'e'
4
-}
unsafeIndex :: Enum a => (a, a) -> a -> Int

{- |
>>> inRange ('a','z') 'e'
True
>>> inRange ('x','z') 'a'
False
-}
inRange :: Enum a => (a, a) -> a -> Bool

{- |
>>> rangeSize ('x','z')
3
-}
rangeSize :: Enum a => (a, a) -> Int

{- |
>>> unsafeRangeSize ('x','z')
3
-}
unsafeRangeSize :: Enum a => (a, a) -> Int

range (l,r) = map toEnum $ Ix.range (fromEnum l, fromEnum r)
index (l,r) i = Ix.index (fromEnum l, fromEnum r) (fromEnum i)
unsafeIndex (l,r) i = Arr.unsafeIndex (fromEnum l, fromEnum r) (fromEnum i)
inRange (l,r) i = Ix.inRange (fromEnum l, fromEnum r) (fromEnum i)
rangeSize (l,r) = Ix.rangeSize (fromEnum l, fromEnum r)
unsafeRangeSize (l,r) = Arr.unsafeRangeSize (fromEnum l, fromEnum r)
