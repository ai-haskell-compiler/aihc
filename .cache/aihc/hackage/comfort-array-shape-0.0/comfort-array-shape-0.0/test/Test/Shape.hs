module Test.Shape where

import qualified Data.Array.Comfort.Shape.Static as Static
import qualified Data.Array.Comfort.Shape.Extra as ShapeExtra
import qualified Data.Array.Comfort.Shape.Test as ShapeTest
import qualified Data.Array.Comfort.Shape as Shape

import qualified Type.Data.Num.Unary.Literal as TypeNum
import qualified Type.Data.Num.Unary as Unary
import Type.Base.Proxy (Proxy)

import qualified Test.QuickCheck as QC
import Test.Utility (prefix)

import Control.Applicative ((<$>))


simplex :: (Unary.Natural n) => Proxy n -> [(String, QC.Property)]
simplex n =
   prefix
      ("Simplex." ++ show (Unary.integralFromProxy n :: Int))
      (ShapeTest.tests $
         (ShapeExtra.Simplex (Unary.unary n) . Shape.Range 'a' <$>
            QC.choose ('a','m')))

staticZeroBased :: (Unary.Natural n) => Proxy n -> [(String, QC.Property)]
staticZeroBased n =
   prefix
      ("Static.ZeroBased." ++ show (Unary.integralFromProxy n :: Int))
      (ShapeTest.tests $ return $ Static.ZeroBased $ Unary.unary n)

tests :: [(String, QC.Property)]
tests =
   simplex TypeNum.u0 ++
   simplex TypeNum.u1 ++
   simplex TypeNum.u2 ++
   simplex TypeNum.u3 ++
   simplex TypeNum.u4 ++
   staticZeroBased TypeNum.u1 ++
   staticZeroBased TypeNum.u2 ++
   staticZeroBased TypeNum.u23 ++
   []
