-- Do not edit! Automatically created with doctest-extract from src/Data/Array/Comfort/Storable/Dim2.hs
{-# LINE 49 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}

module DocTest.Data.Array.Comfort.Storable.Dim2 where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 50 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
import     qualified DocTest.Data.Array.Comfort.Boxed.Unchecked
                                                 as TestBoxedArray
import     DocTest.Data.Array.Comfort.Storable (ShapeInt, shapeInt)

import     qualified Data.Array.Comfort.Boxed as BoxedArray
import     qualified Data.Array.Comfort.Storable.Dim2 as Array2
import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     Data.Array.Comfort.Storable.Dim2 (Array2, (&===), (&|||))
import     Data.Array.Comfort.Storable (Array, (!))
import     Data.Array.Comfort.Shape ((::+)((::+)))

import     qualified Test.QuickCheck as QC

import     Control.Monad (replicateM)
import     Control.Applicative (liftA2, (<$>), (<*>))

import     qualified Data.Map as Map
import     qualified Data.Set as Set
import     Data.Map (Map)
import     Data.Function.HT (Id)
import     Data.Tuple.HT (swap)
import     Data.Word (Word16)
import     Data.Proxy (Proxy(Proxy))

import     Foreign.Storable (Storable)

genArray2     :: QC.Gen (Array2 ShapeInt ShapeInt Word16)
genArray2     = do
       xs <- QC.arbitrary
       let n = length xs
       (k,m) <-
          if n == 0
             then QC.elements [(,) 0, flip (,) 0] <*> QC.choose (1,20)
             else fmap (\m -> (div n m, m)) $ QC.choose (1,n)
       return $ Array.fromList (Shape.ZeroBased k, Shape.ZeroBased m) xs

genArrayForShape     :: (Shape.C shape) => shape -> QC.Gen (Array shape Word16)
genArrayForShape     sh =
       Array.fromList sh <$> replicateM (Shape.size sh) QC.arbitrary

genNonEmptyArray2     :: QC.Gen (Array2 ShapeInt ShapeInt Word16)
genNonEmptyArray2     = do
       xs <- QC.getNonEmpty <$> QC.arbitrary
       let n = length xs
       m <- QC.choose (1,n)
       return $ Array.fromList (Shape.ZeroBased (div n m), Shape.ZeroBased m) xs


transpose     ::
       (Shape.Indexed sh0, Shape.Indexed sh1, Storable a) =>
       Array2 sh0 sh1 a -> Array2 sh1 sh0 a
transpose     a = Array.sample (swap $ Array.shape a) (\(i,j) -> a!(j,i))

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:122: "
{-# LINE 122 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 122 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
   QC.forAll genNonEmptyArray2 $ \xs ->
   QC.forAll (QC.elements $ Shape.indices $ Array.shape xs) $ \(ix0,ix1) ->
      Array2.takeRow xs ix0 ! ix1 == xs!(ix0,ix1)
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:147: "
{-# LINE 147 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 147 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
   QC.forAll genArray2 $ \xs ->
      xs == Array2.fromRowArray (snd $ Array.shape xs) (Array2.toRowArray xs)
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:168: "
{-# LINE 168 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 168 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
   QC.forAll genArray2 $ \xs ->
   let (Shape.ZeroBased m, width) = Array.shape xs in
   QC.forAll (QC.choose (0, m)) $ \k ->
      let ys = Array.reshape
                  (Shape.ZeroBased k ::+ Shape.ZeroBased (m-k), width) xs in
      ys == Array2.above (Array2.takeTop ys) (Array2.takeBottom ys)
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:193: "
{-# LINE 193 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 193 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
   QC.forAll genArray2 $ \xs ->
   let (height, Shape.ZeroBased n) = Array.shape xs in
   QC.forAll (QC.choose (0, n)) $ \k ->
      let ys = Array.reshape
                  (height, Shape.ZeroBased k ::+ Shape.ZeroBased (n-k)) xs in
      ys == Array2.beside (Array2.takeLeft ys) (Array2.takeRight ys)
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:279: "
{-# LINE 279 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.example(
{-# LINE 279 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
      
   let shapeR0 = shapeInt 2; shapeR1 = shapeInt 3 in
   let shapeC0 = shapeInt 3; shapeC1 = shapeInt 2 in
   let block sh a = Array.replicate sh (a::Word16) in
   Array2.fromBlockArray
      (Map.singleton 'A' shapeR0 <> Map.singleton 'B' shapeR1)
      (Map.singleton '1' shapeC0 <> Map.singleton '2' shapeC1) $
   BoxedArray.fromList (Set.fromList "AB", Set.fromList "12")
      [block (shapeR0,shapeC0) 0, block (shapeR0,shapeC1) 1,
       block (shapeR1,shapeC0) 2, block (shapeR1,shapeC1) 3]
  )
  [ExpectedLine [LineChunk "StorableArray.fromList (fromList [('A',ZeroBased {",WildCardChunk,LineChunk " 2}),('B',ZeroBased {",WildCardChunk,LineChunk " 3})],fromList [('1',ZeroBased {",WildCardChunk,LineChunk " 3}),('2',ZeroBased {",WildCardChunk,LineChunk " 2})]) [0,0,0,1,1,0,0,0,1,1,2,2,2,3,3,2,2,2,3,3,2,2,2,3,3]"]]
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:292: "
{-# LINE 292 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 292 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
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
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:311: "
{-# LINE 311 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 311 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
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
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:362: "
{-# LINE 362 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.example(
{-# LINE 362 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
      
   (id :: Id (array (height, Map Char ShapeInt) Word16)) $
   Array2.fromBlockArray
      (Map.singleton 'A' (shapeInt 2) <> Map.singleton 'B' (shapeInt 3))
      Map.empty $
   BoxedArray.fromList (Set.fromList "AB", Set.empty) []
  )
  [ExpectedLine [LineChunk "StorableArray.fromList (fromList [('A',ZeroBased {",WildCardChunk,LineChunk " 2}),('B',ZeroBased {",WildCardChunk,LineChunk " 3})],fromList []) []"]]
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:371: "
{-# LINE 371 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 371 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
   QC.forAll genArray2 $ \block ->
   let height = Map.singleton 'A' $ fst $ Array.shape block in
   let width  = Map.singleton '1' $ snd $ Array.shape block in

   Array.reshape (height,width) block
   QC.===
   Array2.fromBlockArray height width
      (BoxedArray.replicate (Set.singleton 'A', Set.singleton '1') block)
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:484: "
{-# LINE 484 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 484 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
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
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:589: "
{-# LINE 589 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 589 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
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
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Dim2:621: "
{-# LINE 621 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
 DocTest.property(
{-# LINE 621 "src/Data/Array/Comfort/Storable/Dim2.hs" #-}
        
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
  )
