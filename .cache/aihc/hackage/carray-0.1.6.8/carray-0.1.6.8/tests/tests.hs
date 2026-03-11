{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Property, (==>))
import Control.Arrow ((&&&), (***))
import Control.Monad (liftM2)
import Text.Show.Functions ()
import Data.Array.CArray
          (CArray, flatten, ixmapWithInd, rank, reshape, shape, size, sliceWith)
import Data.Ix.Shapable (shapeToStride)
import Data.Array.Unboxed
          (IArray, Ix, UArray,
           accum, amap, bounds, elems, ixmap, listArray, rangeSize)
import Data.Word (Word32)
import Foreign.Storable (Storable)
import Text.Printf (printf)
import System.Environment (getArgs)
-- import System.Random


-- cf. storablevector/test
class Model a b where model :: a -> b

instance (Ix i, IArray a e, Model i i', Model e e') => Model (a i e) ((i',i'),[e']) where
    model = (model . bounds &&& map model . elems)
instance (Model i i', Model e e', Ix i', IArray a e') => Model ((i,i),[e]) (a i' e') where
    model = uncurry listArray . (model *** map model)
instance (Ix i, Ix i', Model i i', Model e e', Storable e, IArray UArray e')
    => Model (CArray i e) (UArray i' e') where
    model = uncurry listArray . (model . bounds &&& map model . elems)
instance (Ix i, Ix i', Model i i', Model e e', Storable e', IArray UArray e)
    => Model (UArray i e) (CArray i' e') where
    model = uncurry listArray . (model . bounds &&& map model . elems)

-- Types are trivially modeled by themselves
instance Model Bool  Bool         where model = id
instance Model Int   Int          where model = id
instance Model Float Float        where model = id
instance Model Double Double      where model = id
instance (Model a a', Model b b') => Model (a,b) (a',b') where
    model (a,b) = (model a, model b)
instance (Model a a', Model b b', Model c c') => Model (a,b,c) (a',b',c') where
    model (a,b,c) = (model a, model b, model c)
instance (Model a a', Model b b', Model c c', Model d d') => Model (a,b,c,d) (a',b',c',d') where
    model (a,b,c,d) = (model a, model b, model c, model d)

(=||=) ::
   (Model x1 y1, Model x y, Eq y) =>
   (x2 -> x1 -> x) -> (x2 -> y1 -> y) -> x2 -> x1 -> Bool

(=|||=) ::
   (Model x2 y2, Model x y, Eq y) =>
   (x3 -> x2 -> x1 -> x) -> (x3 -> y2 -> x1 -> y) -> x3 -> x2 -> x1 -> Bool


infix 1 =||=, =|||=

f =||= g = \a b       ->
    model (f a b)       == g a (model b)
f =|||= g = \a b c     ->
    model (f a b c)     == g a (model b) c

(===) :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
(f === g) x = f x == g x
infixl 1 ===

transposeArray :: CArray (Int,Int) Double -> CArray (Int,Int) Double
transposeArray a = ixmap ((swap *** swap) (bounds a)) swap a
    where swap = (\(i,j) -> (j,i))


type Test2D = CArray (Int,Int) Double -> Bool

prop_flatten_flatten, prop_reshape_flatten, prop_rank,
    prop_shape_size, prop_size, prop_shape_stride_last, prop_transpose :: Test2D

prop_flatten_flatten = flatten . flatten === flatten
prop_reshape_flatten a = reshape (0, size a - 1) a == flatten a
prop_rank = length . shape === rank
prop_shape_size = product . shape === size
prop_size = size === rangeSize . bounds
prop_shape_stride_last = last . shapeToStride . shape === const 1
prop_transpose = transposeArray . transposeArray === id

ca_tests :: [(String, CArray (Int,Int) Double -> Bool)]
ca_tests =
    ("flatten flatten"   , prop_flatten_flatten) :
    ("reshape flatten"   , prop_reshape_flatten) :
    ("rank"              , prop_rank) :
    ("shape size"        , prop_shape_size) :
    ("size"              , prop_size) :
    ("shape stride last" , prop_shape_stride_last) :
    ("transpose^2"       , prop_transpose) :
    []

prop_amap :: (Int -> Double) -> CArray Int Int -> Bool
prop_amap =    (amap :: (Int -> Double) -> CArray Int Int -> CArray Int Double)
          =||= (amap :: (Int -> Double) -> UArray Int Int -> UArray Int Double)

prop_slice_all :: (Int -> Double) -> CArray (Int,Int) Int -> Property
prop_slice_all f a = size a > 0 ==> sliceWith (bounds a) (bounds a) f a == amap f a
prop_ixmapWithInd_amap :: (Int -> Double) -> CArray (Int,Int) Int -> Property
prop_ixmapWithInd_amap f a = size a > 0 ==> ixmapWithInd (bounds a) id (\_ e _ -> f e) a == amap f a

type Acc = Word32
prop_accum :: (Int -> Acc -> Int) -> CArray Int Int -> Property
prop_accum f a =
    QC.forAll (QC.listOf $ liftM2 (,) (QC.choose (bounds a)) QC.arbitrary) $ \ies ->
        (      (accum :: (Int -> Acc -> Int) -> CArray Int Int -> [(Int, Acc)] -> CArray Int Int)
         =|||= (accum :: (Int -> Acc -> Int) -> UArray Int Int -> [(Int, Acc)] -> UArray Int Int)) f a ies

type Transform = CArray Int Int -> CArray Int Int

prop_composeAssoc ::
    Transform -> Transform -> Transform -> CArray Int Int -> Bool
prop_composeAssoc f g h = (f . g) . h === f . (g . h)

main :: IO ()
main = do
    args <- getArgs
    n <- case args of [] -> return 100; str:_ -> readIO str
    let mycheck (s,a) =
            printf "%-25s: " s >>
            QC.quickCheckWith (QC.stdArgs {QC.maxSuccess = n}) a
    mapM_ mycheck ca_tests
    mapM_ mycheck [ ("amap"        , prop_amap) ]
    mapM_ mycheck [ ("accum"       , prop_accum) ]
    mapM_ mycheck [ ("composeAssoc", prop_composeAssoc) ]
    mapM_ mycheck [ ("slice all"         , prop_slice_all)
                  , ("ixmapWithInd amap" , prop_ixmapWithInd_amap) ]

-- arb n k = generate n (mkStdGen k) arbitrary
