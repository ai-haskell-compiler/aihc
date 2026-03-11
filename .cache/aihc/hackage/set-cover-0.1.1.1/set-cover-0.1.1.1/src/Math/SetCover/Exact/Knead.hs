{- |
This re-implements "Math.SetCover.Exact.UArray" using LLVM.
-}
module Math.SetCover.Exact.Knead (
   partitionsIO, searchIO, stepIO,
   partitions,
   State(..), initStateIO, updateStateIO,
   BitSet(..),
   SetId, SetDim, BlockId, BlockDim,
   ) where

import qualified Math.SetCover.Exact as ESC
import Math.SetCover.Exact.Knead.Symbolic (
   SetId, SetDim, BlockId, BlockDim, DigitId, DigitDim,
   Block,
   BitSet(nullBlock, blocksFromSets, keepMinimumBit),
   sumBags3,
   difference,
   getRow,
   nullSet,
   disjoint,
   differenceWithRow,
   findIndices,
   filterDisjointRows,
   )

import Control.Monad.HT ((<=<))
import Control.Monad (foldM)
import Control.Applicative (liftA3, pure, (<$>))

import qualified Data.Array.Knead.Symbolic.Render as Render
import qualified Data.Array.Knead.Symbolic.Physical as Phys
import qualified Data.Array.Knead.Symbolic as Symb
import qualified Data.Array.Knead.Symbolic.Slice as Slice
import qualified Data.Array.Knead.Shape as Shape
import qualified Data.Array.Knead.Expression as Expr
import Data.Array.Knead.Expression ((.|.*))

import qualified Data.Array.Comfort.Shape as ComfortShape
import qualified Data.Array.Comfort.Boxed as Array
import Data.Array.Comfort.Boxed (Array)


import qualified System.IO.Lazy as LazyIO
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.List.Match as Match
import qualified Data.Set as Set
import qualified Data.Bool8 as Bool8
import Data.Set (Set)
import Data.Bool8 (Bool8)

import Prelude2010
import Prelude ()


data State label =
   State {
      availableSubsets ::
         (Array SetDim label, Phys.Array (SetDim,BlockDim) Block),
      freeElements :: Phys.Array BlockDim Block,
      usedSubsets :: [label]
   }

initStateIO :: (Ord a) => [ESC.Assign label (Set a)] -> IO (State label)
initStateIO assigns = do
   let neAssigns = filter (not . Set.null . ESC.labeledSet) assigns
       (avails, freeBlocks) = blocksFromSets $ map ESC.labeledSet neAssigns
       shSets = Shape.ZeroBased $ fromIntegral $ length neAssigns
   free <- Phys.vectorFromList freeBlocks
   avail <-
      Phys.fromList (shSets, Phys.shape free) $
      concatMap (Match.take freeBlocks) avails
   return $
      State {
         availableSubsets =
            (Array.fromList shSets $ map ESC.label neAssigns, avail),
         freeElements = free,
         usedSubsets = []
      }


updateStateIO :: IO (SetId -> State label -> LazyIO.T (State label))
updateStateIO = do
   filt <- filterDisjointRows
   diff <- Render.run differenceWithRow
   return $ \k s ->
      LazyIO.interleave $
      liftA3 State
         (filt k $ availableSubsets s)
         (diff (freeElements s) k $ snd $ availableSubsets s)
         (pure (fst (availableSubsets s) Array.! k : usedSubsets s))



_minimumSet ::
   IO (Phys.Array BlockDim Block ->
       Phys.Array (DigitDim, BlockDim) Block -> IO (Phys.Array BlockDim Block))
_minimumSet = do
   runNullSet <- Render.run (Expr.bool8FromP . nullSet)
   runDifferenceWithRow <- Render.run differenceWithRow
   return $ \baseSet bag ->
      foldM
         (\mins k -> do
            newMins <- runDifferenceWithRow mins k bag
            isNull <- runNullSet newMins
            return $ if Bool8.toBool isNull then mins else newMins)
         baseSet
         (reverse $ ComfortShape.indices $ fst $ Phys.shape bag)


differenceWithRowNull ::
   IO (Phys.Array BlockDim Block ->
       DigitId -> Phys.Array (DigitDim, BlockDim) Block ->
       IO (Bool8, Phys.Array BlockDim Block))
differenceWithRowNull =
   Render.run $ \set k bag ->
   Render.MapAccumLSequence
      (\acc x -> Expr.zip (acc.|.*x) x)
      (Expr.bool8FromP . nullBlock)
      Expr.zero
      (Symb.zipWith difference set $ getRow k bag)

minimumSet ::
   IO (Phys.Array BlockDim Block ->
       Phys.Array (DigitDim, BlockDim) Block -> IO (Phys.Array BlockDim Block))
minimumSet = do
   runDifferenceWithRow <- differenceWithRowNull
   return $ \baseSet bag ->
      foldM
         (\mins k -> do
            (isNull,newMins) <- runDifferenceWithRow mins k bag
            return $ if Bool8.toBool isNull then mins else newMins)
         baseSet
         (reverse $ ComfortShape.indices $ fst $ Phys.shape bag)



keepMinimum :: IO (Phys.Array BlockDim Block -> IO (BlockId,Block))
keepMinimum =
   Render.run $ \xs ->
      Render.MarshalExp $
      Expr.maybe Expr.zero (Expr.mapSnd keepMinimumBit) $
      Symb.findAll (Expr.not . nullBlock . Expr.snd) $
      Symb.zip (Symb.id (Symb.shape xs)) xs

affectedRows ::
   IO (Phys.Array (SetDim,BlockDim) Block -> (BlockId,Block) -> IO [SetId])
affectedRows = do
   affected <-
      Render.run $ \arr (j,bit) ->
         findIndices $ Symb.map (Expr.not . disjoint bit) $
         Slice.apply (Slice.pickSnd j) $ Symb.fix arr
   return $ \arr bit -> Phys.toList =<< affected arr bit

minimize ::
   IO (Phys.Array BlockDim Block ->
       Phys.Array (SetDim,BlockDim) Block -> IO [SetId])
minimize = do
   smBags <- sumBags3
   minSet <- minimumSet
   keepMin <- keepMinimum
   affected <- affectedRows
   return $ \free arr ->
      affected arr =<< keepMin =<< minSet free =<< smBags arr

stepIO :: IO (State label -> LazyIO.T [State label])
stepIO = do
   update <- updateStateIO
   minim <- minimize
   return $ \s ->
      mapM (flip update s) =<<
      LazyIO.interleave (minim (freeElements s) (snd $ availableSubsets s))

searchIO :: IO (State label -> LazyIO.T [[label]])
searchIO = do
   stp <- stepIO
   nullSt <- Render.run (Expr.bool8FromP . nullSet)
   let srch s = do
         isNull <- LazyIO.interleave $ nullSt (freeElements s)
         if Bool8.toBool isNull
           then return [usedSubsets s]
           else concat <$> (mapM srch =<< stp s)
   return srch

partitionsIO :: (Ord a) => IO ([ESC.Assign label (Set a)] -> LazyIO.T [[label]])
partitionsIO = do
   srch <- searchIO
   return $ srch <=< LazyIO.interleave . initStateIO

partitions :: (Ord a) => [ESC.Assign label (Set a)] -> [[label]]
partitions =
   let parts = unsafePerformIO partitionsIO
   in  unsafePerformIO . LazyIO.run . parts
