{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.Slice where

import qualified Numeric.BLAS.Matrix.RowMajor as Matrix
import qualified Numeric.BLAS.Vector as Vector
import qualified Numeric.BLAS.Slice as Slice
import Numeric.BLAS.Vector (Vector)

import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Shape ((::+)((::+)))

import qualified Numeric.Netlib.Class as Class

import qualified Test.QuickCheck as QC

import Prelude hiding (Either(Left, Right))


data Extraction sh0 sh1 a =
   Extraction
      (Slice.T sh0 -> Slice.T sh1)
      (Vector sh0 a -> Vector sh1 a)


type ShapeInt = Shape.ZeroBased Int

shapeInt :: Int -> ShapeInt
shapeInt = Shape.ZeroBased

data Select sh where
   All :: Select ShapeInt
   Left :: (Shape.Indexed shL, Shape.C shR) => Select shL -> Select (shL::+shR)
   Right :: (Shape.Indexed shR, Shape.C shL) => Select shR -> Select (shL::+shR)
   Row ::
      (Shape.Indexed shR, Shape.Index shR ~ ixR, Show ixR, Shape.C shC) =>
      ixR -> Select shC -> Select (shR,shC)
   Column ::
      (Shape.Indexed shC, Shape.Index shC ~ ixC, Show ixC, Shape.C shR) =>
      ixC -> Select shR -> Select (shR,shC)

data ShapeSelect =
   forall sh ix.
   (Shape.Indexed sh, Show sh, Shape.Index sh ~ ix, Show ix) =>
   ShapeSelect sh (Select sh)

deriving instance Show (Select sh)
deriving instance Show ShapeSelect


instantiate :: (Class.Floating a) => sh -> Select sh -> Extraction sh ShapeInt a
instantiate sh select =
   case select of
      All -> Extraction id id
      Left sel ->
         case instantiate (case sh of shL::+_shR -> shL) sel of
            Extraction fs fv ->
               Extraction (fs . Slice.left) (fv . Vector.takeLeft)
      Right sel ->
         case instantiate (case sh of _shL::+shR -> shR) sel of
            Extraction fs fv ->
               Extraction (fs . Slice.right) (fv . Vector.takeRight)
      Row ix sel ->
         case instantiate (snd sh) sel of
            Extraction fs fv ->
               Extraction (fs . Slice.row ix) (fv . Matrix.takeRow ix)
      Column ix sel ->
         case instantiate (fst sh) sel of
            Extraction fs fv ->
               Extraction (fs . Slice.column ix) (fv . Matrix.takeColumn ix)


genPrimShape :: Int -> QC.Gen ShapeInt
genPrimShape maxSize =
   fmap Shape.ZeroBased $ QC.choose (1, maxSize)

genShapeSelect :: Int -> Int -> QC.Gen ShapeSelect
genShapeSelect maxDepth maxSize =
   if maxDepth <= 1 || maxSize < 2
      then fmap (flip ShapeSelect All) $ genPrimShape maxSize
      else
         QC.oneof $
            fmap (flip ShapeSelect All) (genPrimShape maxSize) :
            (do
               pivot <- QC.choose (1, maxSize)
               left <- genShapeSelect (maxDepth-1) pivot
               right <- genShapeSelect (maxDepth-1) (maxSize-pivot)
               case (left, right) of
                  (ShapeSelect shl sell, ShapeSelect shr selr) ->
                     fmap (ShapeSelect (shl::+shr)) $
                     QC.elements [Left sell, Right selr]) :
            (do
               let sizeReal :: Double
                   sizeReal = fromIntegral maxSize
               pivot <- QC.choose (0, logBase 2 sizeReal)
               let maxRows = 2**pivot
               let maxColumns = sizeReal / maxRows
               rows <- genShapeSelect (maxDepth-1) (floor maxRows)
               columns <- genShapeSelect (maxDepth-1) (floor maxColumns)
               case (rows, columns) of
                  (ShapeSelect shr selr, ShapeSelect shc selc) ->
                     fmap (ShapeSelect (shr,shc)) $
                     QC.oneof $
                        (do
                           ix <- QC.elements (Shape.indices shr)
                           return $ Row ix selc) :
                        (do
                           ix <- QC.elements (Shape.indices shc)
                           return $ Column ix selr) :
                        []) :
            []
