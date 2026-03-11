{-# LANGUAGE TypeFamilies #-}
module Data.Array.Comfort.Shape.Static where

import qualified Data.FixedLength as FL

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable (Array)

import qualified Type.Data.Num.Unary as Unary
import Type.Data.Num (integralFromProxy)
import Type.Base.Proxy (Proxy(Proxy))

import Foreign.Storable (Storable)


{- $setup
>>> import qualified Data.Array.Comfort.Shape.Static as Static
>>> import qualified Data.Array.Comfort.Shape as Shape
>>>
>>> import qualified Type.Data.Num.Unary.Literal as TypeNum
>>> import qualified Type.Data.Num.Unary as Unary
-}


{- |
'ZeroBased' denotes a range starting at zero and has a certain length.

>>> Shape.indices (Static.ZeroBased (Unary.unary TypeNum.u0))
[]
>>> Shape.indices (Static.ZeroBased (Unary.unary TypeNum.u1))
[i0]
>>> Shape.indices (Static.ZeroBased (Unary.unary TypeNum.u7))
[i0,i1,i2,i3,i4,i5,i6]
-}
newtype ZeroBased n = ZeroBased {zeroBasedSize :: Proxy (Unary.Un n)}
   deriving (Eq, Show)

instance (Unary.Natural n) => Shape.C (ZeroBased n) where
   size (ZeroBased len) = integralFromProxy len

instance (Unary.Natural n) => Shape.Indexed (ZeroBased n) where
   type Index (ZeroBased n) = FL.Index n
   indices _len = FL.toList FL.indices
   unifiedOffset _len = return . fromIntegral . FL.numFromIndex
   inBounds _len _ix = True

instance (Unary.Natural n) => Shape.InvIndexed (ZeroBased n) where
   -- could be implemented using new fixed-length-0.2.1:FL.indexFromNum
   unifiedIndexFromOffset len k =
      case (0<=k, drop k $ Shape.indices len) of
         (True, i:_) -> return i
         _ ->
            Shape.throwOrError $
            Shape.messageIndexFromOffset "ShapeStatic.ZeroBased" k

instance (Unary.Natural n) => Shape.Static (ZeroBased n) where
   static = ZeroBased Proxy


vector :: (Unary.Natural n, Storable a) => FL.T n a -> Array (ZeroBased n) a
vector = Array.fromList (ZeroBased Proxy) . FL.toList
