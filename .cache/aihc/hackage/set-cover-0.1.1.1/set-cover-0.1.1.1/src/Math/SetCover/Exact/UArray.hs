{- |
This implements "Math.SetCover.Exact" using unboxed arrays of bit vectors.
It should always be faster than using 'Integer's as bit vectors.
In contrast to 'IntSet' the set representation here is dense,
but has a much simpler structure.
It should be faster than 'IntSet' for most applications.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.SetCover.Exact.UArray (
   partitions, search, step,
   State(..), initState, updateState,
   ) where

import qualified Math.SetCover.Exact as ESC
import qualified Math.SetCover.Bit as Bit
import Math.SetCover.Exact.Block (blocksFromSets)

import Control.Monad.ST.Strict (ST)
import Control.Monad (foldM, forM_, when)

import qualified Data.Array.ST as STUArray
import qualified Data.Array.Unboxed as UArray
import qualified Data.List.Match as Match
import qualified Data.Set as Set
import qualified Data.Word as Word
import Data.Array.ST (STUArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray (listArray, bounds, range, (!))
import Data.Array (Array, Ix)
import Data.Set (Set)
import Data.Tuple.HT (mapPair, mapSnd, fst3)
import Data.Bits (xor, (.&.), (.|.))



type Block = Word.Word64

newtype SetId = SetId Int deriving (Eq,Ord,Ix,Enum,Show)
newtype DigitId = DigitId Int deriving (Eq,Ord,Ix,Enum,Show)
newtype BlockId = BlockId Int deriving (Eq,Ord,Ix,Show)


data State label =
   State {
      availableSubsets :: (Array SetId label, UArray (SetId,BlockId) Block),
      freeElements :: UArray BlockId Block,
      usedSubsets :: [label]
   }

initState :: (Ord a) => [ESC.Assign label (Set a)] -> State label
initState assigns =
   let neAssigns = filter (not . Set.null . ESC.labeledSet) assigns
       (avails, free) = blocksFromSets $ map ESC.labeledSet neAssigns
       firstSet = SetId 0; lastSet = SetId $ length neAssigns - 1
       firstBlock = BlockId 0; lastBlock = BlockId $ length free - 1
   in State {
         availableSubsets =
            (listArray (firstSet,lastSet) $ map ESC.label neAssigns,
             listArray ((firstSet,firstBlock), (lastSet,lastBlock)) $
             concatMap (Match.take free) avails),
         freeElements = listArray (firstBlock,lastBlock) free,
         usedSubsets = []
      }


type DifferenceWithRow k =
   UArray BlockId Block -> k ->
   UArray (k,BlockId) Block -> UArray BlockId Block

{-# SPECIALISE differenceWithRow :: DifferenceWithRow SetId #-}
{-# SPECIALISE differenceWithRow :: DifferenceWithRow DigitId #-}
differenceWithRow :: (Ix k) => DifferenceWithRow k
differenceWithRow x k bag =
   listArray (bounds x) $
   map (\j -> Bit.difference (x!j) (bag!(k,j))) (range $ bounds x)


disjoint :: Block -> Block -> Bool
disjoint x y  =  x.&.y == 0

disjointRow :: SetId -> SetId -> UArray (SetId, BlockId) Block -> Bool
disjointRow k0 k1 sets =
   all
      (\j -> disjoint (sets!(k0,j)) (sets!(k1,j)))
      (range $ mapPair (snd,snd) $ bounds sets)

filterDisjointRows ::
   SetId ->
   (Array SetId label, UArray (SetId,BlockId) Block) ->
   (Array SetId label, UArray (SetId,BlockId) Block)
filterDisjointRows k0 (labels,sets) =
   let ((kl,jl), (ku,ju)) = bounds sets
       rows = filter (\k1 -> disjointRow k0 k1 sets) $ range (kl,ku)
       firstSet = SetId 0; lastSet = SetId $ length rows - 1
       rowsArr = listArray (firstSet, lastSet) rows
       bnds = ((firstSet,jl), (lastSet,ju))
   in  (UArray.amap (labels!) rowsArr,
        listArray bnds $ map (\(n,j) -> sets!(rowsArr!n,j)) $ range bnds)


{-# INLINE updateState #-}
updateState :: SetId -> State label -> State label
updateState k s =
   State {
      availableSubsets = filterDisjointRows k $ availableSubsets s,
      freeElements =
         differenceWithRow (freeElements s) k $ snd $ availableSubsets s,
      usedSubsets = fst (availableSubsets s) ! k : usedSubsets s
   }



halfBags :: SetId -> SetId -> (SetId, SetId)
halfBags (SetId firstBag) (SetId lastBag) =
   (SetId $ div (lastBag-firstBag) 2,
    SetId $ div (lastBag-firstBag-1) 2)

double :: SetId -> SetId
double (SetId n) = SetId (2*n)

add2TransposedST ::
   UArray (SetId, BlockId, DigitId) Block ->
   ST s (STUArray s (SetId, BlockId, DigitId) Block)
add2TransposedST xs = do
   let ((firstBag,firstBlock,firstDigit), (lastBag,lastBlock,lastDigit)) =
         UArray.bounds xs
   let newFirstBag = SetId 0
   let (newLastBag, newLastFullBag) = halfBags firstBag lastBag

   let mostSigNull =
         all (\(n,j) -> xs!(n,j,lastDigit) == 0) $
         range ((firstBag,firstBlock), (lastBag,lastBlock))
   let newLastDigit = if mostSigNull then lastDigit else succ lastDigit

   ys <- STUArray.newArray_
            ((newFirstBag, firstBlock, firstDigit),
             (newLastBag, lastBlock, newLastDigit))
   forM_ (range (newFirstBag,newLastFullBag)) $ \n ->
      forM_ (range (firstBlock,lastBlock)) $ \j ->
         writeArray ys (n,j,newLastDigit) =<<
            foldM
               (\carry k -> do
                  let a = xs ! (double n, j, k)
                  let b = xs ! (succ $ double n, j, k)
                  writeArray ys (n,j,k) $ xor carry (xor a b)
                  return $ carry.&.(a.|.b) .|. a.&.b)
               0 (range (firstDigit, pred newLastDigit))
   when (newLastFullBag<newLastBag) $ do
      let n = newLastBag
      forM_ (range (firstBlock,lastBlock)) $ \j -> do
         forM_ (range (firstDigit, pred newLastDigit)) $ \k ->
            writeArray ys (n,j,k) $ xs!(double n,j,k)
         writeArray ys (n,j,newLastDigit) 0
   return ys

add2ST ::
   UArray (SetId, DigitId, BlockId) Block ->
   ST s (STUArray s (SetId, DigitId, BlockId) Block)
add2ST xs = do
   let ((firstBag,firstDigit,firstBlock), (lastBag,lastDigit,lastBlock)) =
         UArray.bounds xs
   let newFirstBag = SetId 0
   let (newLastBag, newLastFullBag) = halfBags firstBag lastBag

   let mostSigNull =
         all (\(n,j) -> xs!(n,lastDigit,j) == 0) $
         range ((firstBag,firstBlock), (lastBag,lastBlock))
   let newLastDigit = if mostSigNull then lastDigit else succ lastDigit

   ys <- STUArray.newArray_
            ((newFirstBag, firstDigit, firstBlock),
             (newLastBag, newLastDigit, lastBlock))
   forM_ (range (newFirstBag,newLastFullBag)) $ \n ->
      forM_ (range (firstBlock,lastBlock)) $ \j ->
         writeArray ys (n,newLastDigit,j) =<<
            foldM
               (\carry k -> do
                  let a = xs ! (double n, k, j)
                  let b = xs ! (succ $ double n, k, j)
                  writeArray ys (n,k,j) $ xor carry (xor a b)
                  return $ carry.&.(a.|.b) .|. a.&.b)
               0 (range (firstDigit, pred newLastDigit))
   when (newLastFullBag<newLastBag) $ do
      let n = newLastBag
      forM_ (range (firstBlock,lastBlock)) $ \j -> do
         forM_ (range (firstDigit,pred newLastDigit)) $ \k ->
            writeArray ys (n,k,j) $ xs!(double n,k,j)
         writeArray ys (n,newLastDigit,j) 0
   return ys

add2 ::
   UArray (SetId, DigitId, BlockId) Block ->
   UArray (SetId, DigitId, BlockId) Block
add2 xs = runSTUArray (add2ST xs)

sumBags :: UArray (SetId,BlockId) Block -> UArray (DigitId,BlockId) Block
sumBags arr =
   let go xs =
         if (UArray.rangeSize $ mapPair (fst3,fst3) $ bounds xs) > 1
           then go $ add2 xs
           else UArray.ixmap
                  (case bounds xs of
                     ((_,kl,jl), (_,ku,ju)) -> ((kl,jl), (ku,ju)))
                  (\(k,j) -> (SetId 0, k, j)) xs
   in  go $
       UArray.ixmap
         (case bounds arr of
            ((nl,jl), (nu,ju)) -> ((nl, DigitId 0, jl), (nu, DigitId 0, ju)))
         (\(n,_,j) -> (n,j)) arr

_sumBagsTransposed ::
   UArray (SetId,BlockId) Block -> UArray (DigitId,BlockId) Block
_sumBagsTransposed arr =
   let go xs =
         if (UArray.rangeSize $ mapPair (fst3,fst3) $ bounds xs) > 1
           then go $ runSTUArray (add2TransposedST xs)
           else UArray.ixmap
                  (case bounds xs of
                     ((_,jl,kl), (_,ju,ku)) -> ((kl,jl), (ku,ju)))
                  (\(k,j) -> (SetId 0, j, k)) xs
   in  go $
       UArray.ixmap
         (case bounds arr of
            ((nl,jl), (nu,ju)) -> ((nl, jl, DigitId 0), (nu, ju, DigitId 0)))
         (\(n,j,_) -> (n,j)) arr


nullSet :: UArray BlockId Block -> Bool
nullSet = all (0==) . UArray.elems

minimumSet ::
   UArray BlockId Block ->
   UArray (DigitId, BlockId) Block -> UArray BlockId Block
minimumSet baseSet bag =
   foldr
      (\k mins ->
         case differenceWithRow mins k bag of
            newMins -> if nullSet newMins then mins else newMins)
      baseSet
      (range $ mapPair (fst,fst) $ bounds bag)

keepMinimum :: UArray BlockId Block -> (BlockId,Block)
keepMinimum =
   mapSnd Bit.keepMinimum . head . dropWhile ((0==) . snd) . UArray.assocs

affectedRows :: (Ix n) => UArray (n,BlockId) Block -> (BlockId,Block) -> [n]
affectedRows arr (j,bit) =
   filter (\n -> not $ disjoint bit $ arr!(n,j)) $
   range $ mapPair (fst,fst) $ bounds arr

minimize :: UArray BlockId Block -> UArray (SetId,BlockId) Block -> [SetId]
minimize free arr =
   affectedRows arr . keepMinimum . minimumSet free $ sumBags arr

step :: State label -> [State label]
step s =
   map (flip updateState s) $
   minimize (freeElements s) (snd $ availableSubsets s)

search :: State label -> [[label]]
search s =
   if nullSet (freeElements s)
     then [usedSubsets s]
     else search =<< step s

partitions :: (Ord a) => [ESC.Assign label (Set a)] -> [[label]]
partitions = search . initState
