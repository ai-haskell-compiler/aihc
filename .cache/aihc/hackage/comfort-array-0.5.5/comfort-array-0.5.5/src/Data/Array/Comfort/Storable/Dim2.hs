{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Comfort.Storable.Dim2 (
   Array2,
   singleRow, flattenRow,
   singleColumn, flattenColumn,
   takeRow,
   toRowArray,
   fromRowArray,
   above, beside,
   takeTop, takeBottom,
   takeLeft, takeRight,

   fromNonEmptyBlockArray,
   fromBlockArray,
   fromBlocks, BlockFunction, RowFunction,
   ShapeSequence(switchSequence),

   BlockArray, BlockMatrix, Block,
   fromBlockMatrix, block, blockAbove, blockBeside, (&===), (&|||),
   ) where

import qualified Data.Array.Comfort.Boxed as BoxedArray
import qualified Data.Array.Comfort.Storable.Unchecked as Array
import qualified Data.Array.Comfort.Shape.SubSize as SubSize
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Unchecked (Array(Array))
import Data.Array.Comfort.Shape ((::+)((::+)))

import Foreign.Marshal.Array (copyArray, advancePtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable)

import qualified Data.StorableVector as SV

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Map (Map)
import Data.Set (Set)
import Data.Foldable (forM_)
import Data.Tuple.HT (mapPair, mapFst)
import Data.Proxy (Proxy(Proxy))


{- $setup
>>> import qualified DocTest.Data.Array.Comfort.Boxed.Unchecked
>>>                                              as TestBoxedArray
>>> import DocTest.Data.Array.Comfort.Storable (ShapeInt, shapeInt)
>>>
>>> import qualified Data.Array.Comfort.Boxed as BoxedArray
>>> import qualified Data.Array.Comfort.Storable.Dim2 as Array2
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import Data.Array.Comfort.Storable.Dim2 (Array2, (&===), (&|||))
>>> import Data.Array.Comfort.Storable (Array, (!))
>>> import Data.Array.Comfort.Shape ((::+)((::+)))
>>>
>>> import qualified Test.QuickCheck as QC
>>>
>>> import Control.Monad (replicateM)
>>> import Control.Applicative (liftA2, (<$>), (<*>))
>>>
>>> import qualified Data.Map as Map
>>> import qualified Data.Set as Set
>>> import Data.Map (Map)
>>> import Data.Function.HT (Id)
>>> import Data.Tuple.HT (swap)
>>> import Data.Word (Word16)
>>> import Data.Proxy (Proxy(Proxy))
>>>
>>> import Foreign.Storable (Storable)
>>>
>>> genArray2 :: QC.Gen (Array2 ShapeInt ShapeInt Word16)
>>> genArray2 = do
>>>    xs <- QC.arbitrary
>>>    let n = length xs
>>>    (k,m) <-
>>>       if n == 0
>>>          then QC.elements [(,) 0, flip (,) 0] <*> QC.choose (1,20)
>>>          else fmap (\m -> (div n m, m)) $ QC.choose (1,n)
>>>    return $ Array.fromList (Shape.ZeroBased k, Shape.ZeroBased m) xs
>>>
>>> genArrayForShape :: (Shape.C shape) => shape -> QC.Gen (Array shape Word16)
>>> genArrayForShape sh =
>>>    Array.fromList sh <$> replicateM (Shape.size sh) QC.arbitrary
>>>
>>> genNonEmptyArray2 :: QC.Gen (Array2 ShapeInt ShapeInt Word16)
>>> genNonEmptyArray2 = do
>>>    xs <- QC.getNonEmpty <$> QC.arbitrary
>>>    let n = length xs
>>>    m <- QC.choose (1,n)
>>>    return $ Array.fromList (Shape.ZeroBased (div n m), Shape.ZeroBased m) xs
>>>
>>>
>>> transpose ::
>>>    (Shape.Indexed sh0, Shape.Indexed sh1, Storable a) =>
>>>    Array2 sh0 sh1 a -> Array2 sh1 sh0 a
>>> transpose a = Array.sample (swap $ Array.shape a) (\(i,j) -> a!(j,i))
-}


type Array2 sh0 sh1 = Array (sh0,sh1)

singleRow :: Array width a -> Array2 () width a
singleRow = Array.mapShape ((,) ())

singleColumn :: Array height a -> Array2 height () a
singleColumn = Array.mapShape (flip (,) ())

flattenRow :: Array2 () width a -> Array width a
flattenRow = Array.mapShape snd

flattenColumn :: Array2 height () a -> Array height a
flattenColumn = Array.mapShape fst


{- |
prop> :{
   QC.forAll genNonEmptyArray2 $ \xs ->
   QC.forAll (QC.elements $ Shape.indices $ Array.shape xs) $ \(ix0,ix1) ->
      Array2.takeRow xs ix0 ! ix1 == xs!(ix0,ix1)
:}
-}
takeRow ::
   (Shape.Indexed sh0, Shape.C sh1, Storable a) =>
   Array2 sh0 sh1 a -> Shape.Index sh0 -> Array sh1 a
takeRow (Array (sh0,sh1) x) ix0 =
   Array.unsafeCreateWithSize sh1 $ \k yPtr ->
   withForeignPtr x $ \xPtr ->
      copyArray yPtr (advancePtr xPtr (Shape.offset sh0 ix0 * k)) k

toRowArray ::
   (Shape.C sh0, Shape.C sh1, Storable a) =>
   Array2 sh0 sh1 a -> BoxedArray.Array sh0 (Array sh1 a)
toRowArray x =
   let y = Array.mapShape (mapFst Shape.Deferred) x in
   BoxedArray.mapShape (\(Shape.Deferred sh0) -> sh0) $
   fmap (takeRow y) $ BoxedArray.indices $ fst $ Array.shape y

{- |
It is a checked error if a row width differs from the result array width.

prop> :{
   QC.forAll genArray2 $ \xs ->
      xs == Array2.fromRowArray (snd $ Array.shape xs) (Array2.toRowArray xs)
:}
-}
fromRowArray ::
   (Shape.C sh0, Shape.C sh1, Eq sh1, Storable a) =>
   sh1 -> BoxedArray.Array sh0 (Array sh1 a) -> Array2 sh0 sh1 a
fromRowArray sh1 x =
   Array.unsafeCreateWithAutoSizes (BoxedArray.shape x, sh1) $
      \(SubSize.Atom _, SubSize.Atom k) yPtr ->
   forM_ (zip [0,k..] (BoxedArray.toList x)) $ \(j, Array sh1i row) ->
   if sh1 == sh1i
      then withForeignPtr row $ \xPtr -> copyArray (advancePtr yPtr j) xPtr k
      else errorArray "fromRowArray" "mismatching row width"


infixr 2 `above`
infixr 3 `beside`

{- |
prop> :{
   QC.forAll genArray2 $ \xs ->
   let (Shape.ZeroBased m, width) = Array.shape xs in
   QC.forAll (QC.choose (0, m)) $ \k ->
      let ys = Array.reshape
                  (Shape.ZeroBased k ::+ Shape.ZeroBased (m-k), width) xs in
      ys == Array2.above (Array2.takeTop ys) (Array2.takeBottom ys)
:}
-}
above ::
   (Shape.C heightA, Shape.C heightB) =>
   (Shape.C width, Eq width) =>
   (Storable a) =>
   Array2 heightA width a ->
   Array2 heightB width a ->
   Array2 (heightA::+heightB) width a
above a b =
   Array.mapShape
      (\((heightA,widthA)::+(heightB,widthB)) ->
         if widthA == widthB
            then (heightA::+heightB, widthA)
            else error "Array.Dim2.above: widths mismatch") $
   Array.append a b

{- |
prop> :{
   QC.forAll genArray2 $ \xs ->
   let (height, Shape.ZeroBased n) = Array.shape xs in
   QC.forAll (QC.choose (0, n)) $ \k ->
      let ys = Array.reshape
                  (height, Shape.ZeroBased k ::+ Shape.ZeroBased (n-k)) xs in
      ys == Array2.beside (Array2.takeLeft ys) (Array2.takeRight ys)
:}
-}
beside ::
   (Shape.C height, Eq height) =>
   (Shape.C widthA, Shape.C widthB) =>
   (Storable a) =>
   Array2 height widthA a ->
   Array2 height widthB a ->
   Array2 height (widthA::+widthB) a
beside a b =
   case (Array.shape a, Array.shape b) of
      ((heightA, widthA), (heightB, widthB)) ->
         if heightA == heightB
            then
               Array.reshape (heightA, widthA::+widthB) .
               Array.fromStorableVector .
               SV.concat . concat . take (Shape.size heightA) $
               zipWith
                  (\arow brow -> [arow, brow])
                  (toRowSlicesInf a)
                  (toRowSlicesInf b)
            else error "Array.Dim2.beside: heights mismatch"


takeTop ::
   (Shape.C heightA, Shape.C heightB, Shape.C width, Storable a) =>
   Array2 (heightA::+heightB) width a ->
   Array2 heightA width a
takeTop = Array.takeLeft . splitVertically

takeBottom ::
   (Shape.C heightA, Shape.C heightB, Shape.C width, Storable a) =>
   Array2 (heightA::+heightB) width a ->
   Array2 heightB width a
takeBottom = Array.takeRight . splitVertically

splitVertically ::
   (Shape.C heightA, Shape.C heightB, Shape.C width) =>
   Array2 (heightA::+heightB) width a ->
   Array ((heightA,width)::+(heightB,width)) a
splitVertically =
   Array.mapShape
      (\(heightA::+heightB, width) -> (heightA,width)::+(heightB,width))


takeLeft ::
   (Shape.C height, Shape.C widthA, Shape.C widthB, Storable a) =>
   Array2 height (widthA::+widthB) a ->
   Array2 height widthA a
takeLeft a =
   case Array.shape a of
      (height, widthA::+widthB) ->
         let m = Shape.size height
             na = Shape.size widthA
             nb = Shape.size widthB
         in Array.reshape (height, widthA) . Array.fromStorableVector .
            SV.concat . take m . map (SV.take na) .
            iterate (SV.drop (na+nb)) .
            Array.toStorableVector $ a

takeRight ::
   (Shape.C height, Shape.C widthA, Shape.C widthB, Storable a) =>
   Array2 height (widthA::+widthB) a ->
   Array2 height widthB a
takeRight a =
   case Array.shape a of
      (height, widthA::+widthB) ->
         let m = Shape.size height
             na = Shape.size widthA
             nb = Shape.size widthB
         in Array.reshape (height, widthB) . Array.fromStorableVector .
            SV.concat . take m . map (SV.take nb) .
            iterate (SV.drop (na+nb)) . SV.drop na .
            Array.toStorableVector $ a


{- |
Only the outer @BoxedArray@ need to be non-empty.

>>> :{
   let shapeR0 = shapeInt 2; shapeR1 = shapeInt 3 in
   let shapeC0 = shapeInt 3; shapeC1 = shapeInt 2 in
   let block sh a = Array.replicate sh (a::Word16) in
   Array2.fromBlockArray
      (Map.singleton 'A' shapeR0 <> Map.singleton 'B' shapeR1)
      (Map.singleton '1' shapeC0 <> Map.singleton '2' shapeC1) $
   BoxedArray.fromList (Set.fromList "AB", Set.fromList "12")
      [block (shapeR0,shapeC0) 0, block (shapeR0,shapeC1) 1,
       block (shapeR1,shapeC0) 2, block (shapeR1,shapeC1) 3]
:}
StorableArray.fromList (fromList [('A',ZeroBased {... 2}),('B',ZeroBased {... 3})],fromList [('1',ZeroBased {... 3}),('2',ZeroBased {... 2})]) [0,0,0,1,1,0,0,0,1,1,2,2,2,3,3,2,2,2,3,3,2,2,2,3,3]

prop> :{
   QC.forAll genArray2 $ \blockA1 ->
   QC.forAll genArray2 $ \blockB2 ->
   let shapeR0 = fst $ Array.shape blockA1 in
   let shapeC0 = snd $ Array.shape blockA1 in
   let shapeR1 = fst $ Array.shape blockB2 in
   let shapeC1 = snd $ Array.shape blockB2 in
   QC.forAll (genArrayForShape (shapeR0, shapeC1)) $ \blockA2 ->
   QC.forAll (genArrayForShape (shapeR1, shapeC0)) $ \blockB1 ->
   let blocked =
         BoxedArray.fromList (Set.fromList "AB", Set.fromList "12")
            [blockA1, blockA2, blockB1, blockB2] in

   transpose (Array2.fromNonEmptyBlockArray blocked)
   QC.===
   Array2.fromNonEmptyBlockArray
      (TestBoxedArray.transpose (fmap transpose blocked))
:}

prop> :{
   QC.forAll genArray2 $ \blockA1 ->
   QC.forAll genArray2 $ \blockB2 ->
   QC.forAll genArray2 $ \blockC3 ->
   let shapeR0 = fst $ Array.shape blockA1 in
   let shapeC0 = snd $ Array.shape blockA1 in
   let shapeR1 = fst $ Array.shape blockB2 in
   let shapeC1 = snd $ Array.shape blockB2 in
   let shapeR2 = fst $ Array.shape blockC3 in
   let shapeC2 = snd $ Array.shape blockC3 in
   QC.forAll (genArrayForShape (shapeR0, shapeC1)) $ \blockA2 ->
   QC.forAll (genArrayForShape (shapeR0, shapeC2)) $ \blockA3 ->
   QC.forAll (genArrayForShape (shapeR1, shapeC0)) $ \blockB1 ->
   QC.forAll (genArrayForShape (shapeR1, shapeC2)) $ \blockB3 ->
   QC.forAll (genArrayForShape (shapeR2, shapeC0)) $ \blockC1 ->
   QC.forAll (genArrayForShape (shapeR2, shapeC1)) $ \blockC2 ->
   let blocked =
         BoxedArray.fromList (Set.fromList "ABC", Set.fromList "123")
            [blockA1, blockA2, blockA3,
             blockB1, blockB2, blockB3,
             blockC1, blockC2, blockC3] in

   transpose (Array2.fromNonEmptyBlockArray blocked)
   QC.===
   Array2.fromNonEmptyBlockArray
      (TestBoxedArray.transpose (fmap transpose blocked))
:}
-}
fromNonEmptyBlockArray ::
   (Ord row,    Shape.C height, Eq height) =>
   (Ord column, Shape.C width,  Eq width) =>
   (Storable a) =>
   BoxedArray.Array (Set row, Set column) (Array2 height width a) ->
   Array2 (Map row height) (Map column width) a
fromNonEmptyBlockArray arr =
   let shapes = List.map Array.shape $ BoxedArray.toList arr in
   let width = Set.size $ snd $ BoxedArray.shape arr in
   let (rowIxs, columnIxs) =
         mapPair (Set.toAscList, Set.toAscList) $ BoxedArray.shape arr in
   case (ListHT.sieve width shapes, take width shapes) of
      (leftColumn@(_:_), topRow@(_:_)) ->
         fromBlockArray
            (Map.fromList $ List.zip rowIxs $ List.map fst leftColumn)
            (Map.fromList $ List.zip columnIxs $ List.map snd topRow)
            arr
      _ -> errorArray "fromNonEmptyBlockArray" "empty array"

{- |
Explicit parameters for the shape of the result matrix
allow for working with arrays of zero rows or columns.

>>> :{
   (id :: Id (array (height, Map Char ShapeInt) Word16)) $
   Array2.fromBlockArray
      (Map.singleton 'A' (shapeInt 2) <> Map.singleton 'B' (shapeInt 3))
      Map.empty $
   BoxedArray.fromList (Set.fromList "AB", Set.empty) []
:}
StorableArray.fromList (fromList [('A',ZeroBased {... 2}),('B',ZeroBased {... 3})],fromList []) []

prop> :{
   QC.forAll genArray2 $ \block ->
   let height = Map.singleton 'A' $ fst $ Array.shape block in
   let width  = Map.singleton '1' $ snd $ Array.shape block in

   Array.reshape (height,width) block
   QC.===
   Array2.fromBlockArray height width
      (BoxedArray.replicate (Set.singleton 'A', Set.singleton '1') block)
:}
-}
fromBlockArray ::
   (Ord row,    Shape.C height, Eq height) =>
   (Ord column, Shape.C width,  Eq width) =>
   (Storable a) =>
   Map row height -> Map column width ->
   BoxedArray.Array (Set row, Set column) (Array2 height width a) ->
   Array2 (Map row height) (Map column width) a
fromBlockArray height width =
   Array.reshape (height, width) . Array.fromStorableVector .
   SV.concat . List.concat . List.concatMap List.transpose .
   ListHT.sliceVertical (Map.size width) . BoxedArray.toList .
   BoxedArray.zipWith checkSliceBlock
      (BoxedArray.cartesian
         (BoxedArray.fromMap height) (BoxedArray.fromMap width))
{-
[[[111,111],[222,222]],[[333,333],[444,444]]]
  |
  v
[111,222,111,222,333,444,333,444]
-}



class (Shape.C sh) => ShapeSequence sh where
   switchSequence ::
      f Shape.Zero ->
      (forall sh0 shs. (Shape.C sh0, Eq sh0, ShapeSequence shs) =>
         f (sh0::+shs)) ->
      f sh

instance ShapeSequence Shape.Zero where
   switchSequence f _ = f

instance
   (Shape.C sh, Eq sh, ShapeSequence shs) =>
      ShapeSequence (sh::+shs) where
   switchSequence _ f = f


type family BlockFunction heights widths a r
type instance BlockFunction Shape.Zero widths a r = r
type instance BlockFunction (height::+heights) widths a r =
       RowFunction height widths a (BlockFunction heights widths a r)

newtype CreateBig widths a r heights =
   CreateBig {
      getCreateBig ::
         heights -> widths ->
         ([[SV.Vector a]] -> r) ->
         BlockFunction heights widths a r
   }

createBig ::
   (ShapeSequence heights, ShapeSequence widths, Storable a) =>
   heights -> widths ->
   ([[SV.Vector a]] -> r) ->
   BlockFunction heights widths a r
createBig =
   getCreateBig $
   switchSequence
      (CreateBig $ \Shape.Zero _widths cons -> cons [])
      (CreateBig $ \(height::+heights) widths cons ->
         createBlockRow heights widths cons height widths id)


type family RowFunction height widths a r
type instance RowFunction height Shape.Zero a r = r
type instance RowFunction height (width::+widths) a r =
       Array2 height width a -> RowFunction height widths a r

newtype CreateBlockRow heightsRem widthsRem height a r widths =
   CreateBlockRow {
      getCreateBlockRow ::
         heightsRem -> widthsRem -> ([[SV.Vector a]] -> r) ->
         height ->     widths ->    ([[SV.Vector a]] -> [[SV.Vector a]]) ->
         RowFunction height widths a
            (BlockFunction heightsRem widthsRem a r)
   }

createBlockRow ::
   (ShapeSequence heightsRem, ShapeSequence widthsRem) =>
   (Shape.C height, Eq height, ShapeSequence widths, Storable a) =>
   heightsRem -> widthsRem -> ([[SV.Vector a]] -> r) ->
   height ->     widths ->    ([[SV.Vector a]] -> [[SV.Vector a]]) ->
   RowFunction height widths a
      (BlockFunction heightsRem widthsRem a r)
createBlockRow =
   getCreateBlockRow $
   switchSequence
      (CreateBlockRow $
         \heightsRem widthsRem consBig _height Shape.Zero consRow ->
            createBig heightsRem widthsRem
               (consBig . (List.transpose (consRow []) ++)))
      (CreateBlockRow $
         \heightsRem widthsRem consBig height (width::+widths) consRow blk ->
            createBlockRow
               heightsRem widthsRem consBig
               height widths
                  (consRow . (checkSliceBlock (height,width) blk :)))


{- |
prop> :{
   QC.forAll genArray2 $ \blockA1 ->
   QC.forAll genArray2 $ \blockB2 ->
   let shapeR0 = fst $ Array.shape blockA1 in
   let shapeC0 = snd $ Array.shape blockA1 in
   let shapeR1 = fst $ Array.shape blockB2 in
   let shapeC1 = snd $ Array.shape blockB2 in
   let shapeR = shapeR0::+shapeR1::+Shape.Zero in
   let shapeC = shapeC0::+shapeC1::+Shape.Zero in
   QC.forAll (genArrayForShape (shapeR0, shapeC1)) $ \blockA2 ->
   QC.forAll (genArrayForShape (shapeR1, shapeC0)) $ \blockB1 ->
   let blocked =
         BoxedArray.fromList (Set.fromList "AB", Set.fromList "12")
            [blockA1, blockA2, blockB1, blockB2] in

   Array.reshape (shapeR, shapeC)
      (Array2.fromNonEmptyBlockArray blocked)
   QC.===
   Array2.fromBlocks shapeR shapeC Proxy
      blockA1 blockA2
      blockB1 blockB2
:}
-}
fromBlocks ::
   (ShapeSequence height, ShapeSequence width, Storable a) =>
   height -> width -> Proxy a ->
   BlockFunction height width a (Array2 height width a)
fromBlocks height width proxy =
   createBig height width
      (Array.reshape (height, width) . Array.fromStorableVector .
       idSV proxy . SV.concat . List.concat)

idSV :: Proxy a -> SV.Vector a -> SV.Vector a
idSV Proxy = id



data BlockArray shape a = BlockArray shape [[SV.Vector a]]
type BlockMatrix height width = BlockArray (height, width)

block ::
   (Block block, Shape.C height, Shape.C width, Storable a) =>
   block (height, width) a -> BlockMatrix height width a
block = blockPrivate

class Block block where
   blockPrivate ::
      (Shape.C height, Shape.C width, Storable a) =>
      block (height, width) a -> BlockMatrix height width a

instance Block BlockArray where
   blockPrivate = id

instance Block Array where
   blockPrivate arr =
      BlockArray (Array.shape arr)
         (map (:[]) $ take (Shape.size $ fst $ Array.shape arr) $
         toRowSlicesInf arr)

blockAbove ::
   (Eq width) =>
   BlockMatrix heightA width a -> BlockMatrix heightB width a ->
   BlockMatrix (heightA::+heightB) width a
blockAbove (BlockArray (heightA,widthA) a) (BlockArray (heightB,widthB) b) =
   BlockArray
      (if widthA == widthB
         then (heightA::+heightB, widthA)
         else error "Array.Dim2.blockAbove: widths mismatch")
      (a ++ b)

blockBeside ::
   (Eq height) =>
   BlockMatrix height widthA a -> BlockMatrix height widthB a ->
   BlockMatrix height (widthA::+widthB) a
blockBeside (BlockArray (heightA,widthA) a) (BlockArray (heightB,widthB) b) =
   BlockArray
      (if heightA == heightB
         then (heightA, widthA::+widthB)
         else error "Array.Dim2.beside: heights mismatch")
      (zipWith (++) a b)

infixr 2 &===
infixr 3 &|||

(&===) ::
   (Block blockA, Block blockB) =>
   (Shape.C heightA, Shape.C heightB) =>
   (Shape.C width, Eq width) =>
   (Storable a) =>
   blockA (heightA,width) a -> blockB (heightB,width) a ->
   BlockMatrix (heightA::+heightB) width a
(&===) a b = blockAbove (block a) (block b)

(&|||) ::
   (Block blockA, Block blockB) =>
   (Shape.C height, Eq height) =>
   (Shape.C widthA, Shape.C widthB) =>
   (Storable a) =>
   blockA (height,widthA) a -> blockB (height,widthB) a ->
   BlockMatrix height (widthA::+widthB) a
(&|||) a b = blockBeside (block a) (block b)



{- |
prop> :{
   QC.forAll genArray2 $ \blockA1 ->
   QC.forAll genArray2 $ \blockB3 ->
   QC.forAll
      (liftA2
         (\char0 char1 -> Shape.Range (min char0 char1) (max char0 char1))
         (QC.choose ('a','k')) (QC.choose ('a','k'))) $
      \shapeC1 ->
   let shapeR0 = fst $ Array.shape blockA1 in
   let shapeC0 = snd $ Array.shape blockA1 in
   let shapeR1 = fst $ Array.shape blockB3 in
   let shapeC2 = snd $ Array.shape blockB3 in
   QC.forAll (genArrayForShape (shapeR0, shapeC1)) $ \blockA2 ->
   QC.forAll (genArrayForShape (shapeR0, shapeC2)) $ \blockA3 ->
   QC.forAll (genArrayForShape (shapeR1, shapeC0)) $ \blockB1 ->
   QC.forAll (genArrayForShape (shapeR1, shapeC1)) $ \blockB2 ->

   Array2.fromBlockMatrix
      (blockA1 &||| Array2.beside blockA2 blockA3
       &===
       blockB1 &||| blockB2 &||| blockB3)
   QC.===
   Array.reshape
      (shapeR0::+shapeR1, shapeC0::+shapeC1::+shapeC2)
      (Array2.fromBlocks
         (shapeR0::+shapeR1::+Shape.Zero)
         (shapeC0::+shapeC1::+shapeC2::+Shape.Zero)
         Proxy
         blockA1 blockA2 blockA3
         blockB1 blockB2 blockB3)
:}

prop> :{
   QC.forAll
      (liftA2
         (\char0 char1 -> Shape.Range (min char0 char1) (max char0 char1))
         (QC.choose ('a','k')) (QC.choose ('a','k'))) $
      \shapeR0 ->
   QC.forAll
         (liftA2 Shape.Shifted (QC.choose (-10,10)) (QC.choose (0,10::Int))) $
      \shapeR1 ->
   let shapeR2 = () in
   QC.forAll (fmap Shape.ZeroBased (QC.choose (0,10::Int))) $
      \shapeC0 ->
   QC.forAll (fmap Shape.OneBased (QC.choose (0,10::Int))) $
      \shapeC1 ->
   let shapeC2 :: Shape.Enumeration Ordering
       shapeC2 = Shape.Enumeration in

   QC.forAll (genArrayForShape (shapeR0, shapeC0)) $ \blockA1 ->
   QC.forAll (genArrayForShape (shapeR0, shapeC1)) $ \blockA2 ->
   QC.forAll (genArrayForShape (shapeR0, shapeC2)) $ \blockA3 ->
   QC.forAll (genArrayForShape (shapeR1, shapeC0)) $ \blockB1 ->
   QC.forAll (genArrayForShape (shapeR1, shapeC1)) $ \blockB2 ->
   QC.forAll (genArrayForShape (shapeR1, shapeC2)) $ \blockB3 ->
   QC.forAll (genArrayForShape (shapeR2, shapeC0)) $ \blockC1 ->
   QC.forAll (genArrayForShape (shapeR2, shapeC1)) $ \blockC2 ->
   QC.forAll (genArrayForShape (shapeR2, shapeC2)) $ \blockC3 ->

   Array2.fromBlockMatrix
      (blockA1 &||| blockA2 &||| blockA3
       &===
       blockB1 &||| blockB2 &||| blockB3
       &===
       blockC1 &||| blockC2 &||| blockC3)
   QC.===
   Array2.beside
      (Array2.above blockA1 $ Array2.above blockB1 blockC1)
      (Array2.above
         (Array2.beside blockA2 blockA3)
         (Array2.beside
            (Array2.above blockB2 blockC2)
            (Array2.above blockB3 blockC3)))
:}
-}
fromBlockMatrix ::
   (Shape.C height, Shape.C width, Storable a) =>
   BlockMatrix height width a -> Array2 height width a
fromBlockMatrix (BlockArray (height, width) rows) =
   Array.reshape (height, width) .
   Array.fromStorableVector . SV.concat . List.concat $ rows


checkSliceBlock ::
   (Shape.C sh0, Eq sh0, Shape.C sh1, Eq sh1, Storable a) =>
   (sh0, sh1) -> Array (sh0, sh1) a -> [SV.Vector a]
checkSliceBlock sh blk =
   if sh == Array.shape blk
      then toRowSlices blk
      else errorArray "fromBlockArray" "block shapes mismatch"

toRowSlices ::
   (Shape.C sh0, Shape.C sh1, Storable a) =>
   Array2 sh0 sh1 a -> [SV.Vector a]
toRowSlices arr =
   SV.sliceVertical (Shape.size $ snd $ Array.shape arr) $
   Array.toStorableVector arr

toRowSlicesInf ::
   (Shape.C sh0, Shape.C sh1, Storable a) =>
   Array2 sh0 sh1 a -> [SV.Vector a]
toRowSlicesInf arr =
   let n = Shape.size $ snd $ Array.shape arr in
   map (SV.take n) . iterate (SV.drop n) . Array.toStorableVector $ arr


errorArray :: String -> String -> a
errorArray name msg =
   error ("Array.Comfort.Storable.Dim2." ++ name ++ ": " ++ msg)
