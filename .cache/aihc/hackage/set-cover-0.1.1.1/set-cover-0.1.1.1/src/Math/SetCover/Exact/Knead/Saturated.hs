{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Math.SetCover.Exact.Knead.Saturated (
   partitionsIO, searchIO, stepIO,
   partitions,
   State(..), initStateIO, updateStateIO,
   ) where

import qualified Math.SetCover.Exact as ESC
import Math.SetCover.Exact.Knead.Vector (Block)
import Math.SetCover.Exact.Knead.Symbolic
         (SetId, SetDim, BlockId, BlockDim, blocksFromSets,
          nullSet, disjoint, differenceWithRow,
          filterDisjointRows, findIndices)

import qualified Control.Monad.HT as Monad
import Control.Monad.HT ((<=<))
import Control.Monad (foldM)
import Control.Applicative (liftA3, pure, (<$>))

import qualified Data.Array.Knead.Symbolic.Render as Render
import qualified Data.Array.Knead.Symbolic.Physical as Phys
import qualified Data.Array.Knead.Symbolic as Symb
import qualified Data.Array.Knead.Symbolic.Slice as Slice
import qualified Data.Array.Knead.Shape as Shape
import qualified LLVM.DSL.Expression.Vector as ExprVec
import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp, (/=*), (<*), (.&.*), )

import qualified Data.Array.Comfort.Boxed as Array
import Data.Array.Comfort.Boxed (Array)

import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value.Vector as MultiValueVec
import qualified LLVM.Extra.Multi.Value as MultiValue
import LLVM.Extra.Multi.Value (atom)

import qualified LLVM.Util.Intrinsic as Intr
import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal ((:+:))

import qualified System.IO.Lazy as LazyIO
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.List.Match as Match
import qualified Data.Set as Set
import qualified Data.Word as Word
import qualified Data.Int as Int
import qualified Data.Bool8 as Bool8
import qualified Data.Bits as Bits
import Data.Set (Set)
import Data.Maybe (fromMaybe)

import Prelude2010
import Prelude ()


type NumCounters = TypeNum.D16
type Counter = Word.Word8
type Counters = LLVM.Vector NumCounters Counter
type Subblock = Word.Word8
type Block16 = LLVM.Vector TypeNum.D8 Word.Word16
-- type Block128 = LLVM.WordN TypeNum.D128

bitSize :: Int
bitSize = Bits.bitSize (0::Counter)

numCounters :: Integer
numCounters =
   TypeNum.integerFromSingleton
      (TypeNum.singleton :: TypeNum.Singleton NumCounters)

type CounterId = Int.Int16
type BitId = Int.Int8

type CounterDim = Shape.ZeroBased CounterId
type BitDim = Shape.ZeroBased BitId


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


repVec :: Counter -> Exp Counters
repVec = Expr.fromInteger' . toInteger

{- |
We add bytes with saturation.
The first operand must consist exclusively of zeros and ones.

With saturation we perform the same as the unoptimized algorithm
if the element with minimum occurrence is contained in at most 254 sets.
This is pretty much and should never happen.
If all elements occur in more than 254 sets
then we will choose the first one
which might lead to an unnecessary long case analysis,
but would still yield correct results.
-}
incSat :: Exp Counters -> Exp Counters -> Exp Counters
incSat x y =
   let maxBnd = repVec maxBound
   in  ExprVec.select (ExprVec.cmp LLVM.CmpEQ y maxBnd)
         maxBnd (Expr.add x y)

incSatGeneric ::
   LLVM.Value Counters -> LLVM.Value Counters ->
   LLVM.CodeGenFunction r (LLVM.Value Counters)
incSatGeneric x y =
   (\(MultiValue.Cons z) -> z)
   <$>
   Expr.unliftM2 incSat (MultiValue.Cons x) (MultiValue.Cons y)

incSatX86 :: Exp Counters -> Exp Counters -> Exp Counters
incSatX86 =
   Expr.liftReprM2 (\x y -> fromMaybe incSatGeneric Intr.maybeUAddSat x y)

sumRows ::
   Symb.Array (SetDim, blockDim) Counters ->
   Render.FoldOuterL SetDim blockDim Counters Counters
sumRows xs =
   Render.FoldOuterL (flip incSatX86)
      (Symb.fill (Expr.snd $ Symb.shape xs) Expr.zero) xs


extrudeBits :: Slice.T sh (sh, BitDim)
extrudeBits =
   Slice.extrudeSnd $ Expr.compose $ Shape.ZeroBased $
   Expr.fromInteger' $ toInteger bitSize

extrudeCounters :: Slice.T sh (sh, CounterDim)
extrudeCounters =
   Slice.extrudeSnd $ Expr.compose $ Shape.ZeroBased $
   Expr.fromInteger' numCounters

toCounters :: Exp Block -> Exp Counters
toCounters = Expr.liftReprM LLVM.bitcast

_pickBits :: Exp BitId -> Exp Block -> Exp Counters
_pickBits k block =
   repVec 1 .&.* Expr.shr (toCounters block) (ExprVec.replicate (bitPos k))


word16 :: Exp BitId -> Exp Word.Word16
word16 = Expr.liftReprM LLVM.ext . bitPos

toBlock16 :: Exp Block -> Exp Block16
toBlock16 = Expr.liftReprM LLVM.bitcast

fromBlock16 :: Exp Block16 -> Exp Counters
fromBlock16 = Expr.liftReprM LLVM.bitcast

pickBitsX86 :: Exp BitId -> Exp Block -> Exp Counters
pickBitsX86 k block =
   repVec 1 .&.*
   fromBlock16 (Expr.shr (toBlock16 block) (ExprVec.replicate (word16 k)))

uninterleaveBits ::
   Symb.Array (SetDim, BlockDim) Block ->
   Symb.Array (SetDim, (BlockDim, BitDim)) Counters
uninterleaveBits =
   Symb.mapWithIndex (\ix block -> pickBitsX86 (Expr.snd (Expr.snd ix)) block) .
   Slice.apply (Slice.second extrudeBits)


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


mvvec :: MultiValue.T (LLVM.Vector n a) -> MultiVector.T n a
mvvec (MultiValue.Cons x) = MultiVector.Cons x

extract ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Exp CounterId -> Exp (LLVM.Vector n a) -> Exp a
extract =
   Expr.liftM2
      (\(MultiValue.Cons k) v ->
         flip MultiVector.extract (mvvec v) =<< LLVM.zext k)

extractBlock :: Exp CounterId -> Exp Block -> Exp Subblock
extractBlock =
   Expr.liftReprM2 (\k v -> LLVM.extractelement v =<< LLVM.zext k)

flattenCounters ::
   Symb.Array (BlockDim, BitDim) Counters ->
   Symb.Array ((BlockDim,CounterDim), BitDim) Counter
flattenCounters =
   Symb.mapWithIndex (\ix block -> extract (Expr.snd (Expr.fst ix)) block) .
   Slice.apply (Slice.first extrudeCounters)


bitPos :: Exp BitId -> Exp Subblock
bitPos = Expr.liftReprM LLVM.bitcast

singleBit :: Exp BitId -> Exp Subblock
singleBit = Expr.shl 1 . bitPos


argMin ::
   (MultiValue.Select x, MultiValue.Select y, MultiValue.Comparison y) =>
   Exp (x,y) -> Exp (x,y) -> Exp (x,y)
argMin xy0 xy1 = Expr.select (Expr.snd xy0 <* Expr.snd xy1) xy0 xy1

argMinimum ::
   (Shape.C sh, Shape.Index sh ~ ix, MultiValue.Select ix) =>
   Symb.Array sh Counter -> Exp ix
argMinimum = Expr.fst . Symb.fold1All argMin . Symb.mapWithIndex Expr.zip

_keepMinimum ::
   IO (Phys.Array (BlockDim, BitDim) Counters ->
       IO ((BlockId,CounterId),Counter))
_keepMinimum =
   Render.run $
   Render.MarshalExp . Expr.mapSnd singleBit . argMinimum . flattenCounters


argMinMasked ::
   (MultiValue.Select x, MultiValue.Select y, MultiValue.Comparison y) =>
   Exp (Bool, (x,y)) -> Exp (Bool, (x,y)) -> Exp (Bool, (x,y))
argMinMasked xy0 xy1 =
   Expr.select (Expr.fst xy1)
      (Expr.select (Expr.fst xy0)
         (Expr.zip Expr.true $ argMin (Expr.snd xy0) (Expr.snd xy1))
         xy1)
      xy0

testBlockBit :: Exp CounterId -> Exp BitId -> Exp Block -> Exp Bool
testBlockBit k j block = Expr.shr (extractBlock k block) (bitPos j) .&.* 1 /=* 0

flattenBlockBits ::
   Symb.Array BlockDim Block ->
   Symb.Array ((BlockDim,CounterDim), BitDim) Bool
flattenBlockBits =
   Symb.mapWithIndex
      (Expr.modify2 ((atom,atom),atom) atom $ \((_n,k),j) block ->
         testBlockBit k j block) .
   Slice.apply (Slice.compose extrudeCounters extrudeBits)

argMinimumMasked ::
   Symb.Array BlockDim Block ->
   Symb.Array ((BlockDim,CounterDim), BitDim) Counter ->
   Exp ((BlockId,CounterId),BitId)
argMinimumMasked free =
   Expr.fst . Expr.snd . Symb.fold1All argMinMasked .
   Symb.zip (flattenBlockBits free) . Symb.mapWithIndex Expr.zip

_keepMinimumMasked ::
   IO (Phys.Array BlockDim Block ->
       Phys.Array (BlockDim,BitDim) Counters ->
       IO ((BlockId,CounterId),Counter))
_keepMinimumMasked =
   Render.run $ \free ->
      Render.MarshalExp .
      Expr.mapSnd singleBit . argMinimumMasked free . flattenCounters


argMinVec ::
   (TypeNum.Positive n,
    MultiVector.Select x, MultiVector.Select y, MultiVector.Comparison y) =>
   Exp (LLVM.Vector n (x,y)) -> Exp (LLVM.Vector n (x,y)) ->
   Exp (LLVM.Vector n (x,y))
argMinVec xy0 xy1 =
   ExprVec.select
      (ExprVec.cmp LLVM.CmpLT (ExprVec.snd xy0) (ExprVec.snd xy1)) xy0 xy1

argMinMaskedVec ::
   (TypeNum.Positive n,
    MultiVector.Select x, MultiVector.Select y, MultiVector.Comparison y) =>
   Exp (LLVM.Vector n (Bool, (x,y))) -> Exp (LLVM.Vector n (Bool, (x,y))) ->
   Exp (LLVM.Vector n (Bool, (x,y)))
argMinMaskedVec xy0 xy1 =
   ExprVec.select (ExprVec.fst xy1)
      (ExprVec.select (ExprVec.fst xy0)
         (ExprVec.zip (ExprVec.replicate Expr.true) $
          argMinVec (ExprVec.snd xy0) (ExprVec.snd xy1))
         xy1)
      xy0

testBlockBitVec ::
   Exp BitId -> Exp Block -> Exp (LLVM.Vector NumCounters Bool)
testBlockBitVec j block =
   ExprVec.cmp LLVM.CmpNE Expr.zero $ pickBitsX86 j block

flattenBlockBitsVec ::
   Symb.Array BlockDim Block ->
   Symb.Array (BlockDim,BitDim) (LLVM.Vector NumCounters Bool)
flattenBlockBitsVec =
   Symb.mapWithIndex
      (Expr.modify2 (atom,atom) atom $ \(_n,j) block ->
         testBlockBitVec j block) .
   Slice.apply extrudeBits

argMinimumMaskedVec ::
   Symb.Array BlockDim Block ->
   Symb.Array (BlockDim, BitDim) Counters ->
   Exp (LLVM.Vector NumCounters (Bool, ((BlockId, BitId), Counter)))
argMinimumMaskedVec free =
   Symb.fold1All argMinMaskedVec .
   Symb.zipWith ExprVec.zip (flattenBlockBitsVec free) .
   Symb.mapWithIndex (ExprVec.zip . ExprVec.replicate)

counterIds :: Exp (LLVM.Vector NumCounters CounterId)
counterIds = ExprVec.cons (LLVM.vector (NonEmptyC.iterate (1+) 0))

_keepMinimumMaskedVector ::
   Exp (LLVM.Vector NumCounters (Bool, ((BlockId, BitId), Counter))) ->
   Exp ((BlockId, CounterId), BitId)
_keepMinimumMaskedVector =
   Expr.liftM
      (fmap (MultiValue.fst . MultiValue.snd) .
       foldM (Expr.unliftM2 argMinMasked)
         (MultiValue.zip (MultiValue.cons False) MultiValue.undef)
       <=< MultiValueVec.dissect)
   .
   ExprVec.mapSnd
      (ExprVec.mapFst (ExprVec.mapFst (flip ExprVec.zip counterIds)))

type
   IxVector n =
      MultiValue.T (LLVM.Vector n
         (Bool, (((BlockId, CounterId), BitId), Counter)))

argMinMaskedVecHalf ::
   (TypeNum.Positive n, TypeNum.Positive n2, (n:+:n) ~ n2,
    MultiVector.Select x, MultiVector.Select y, MultiVector.Comparison y) =>
   MultiValue.T (LLVM.Vector n2 (Bool, (x, y))) ->
   LLVM.CodeGenFunction r (MultiValue.T (LLVM.Vector n (Bool, (x, y))))
argMinMaskedVecHalf x =
   Monad.liftJoin2
      (Expr.unliftM2 argMinMaskedVec)
      (MultiValueVec.take x)
      (MultiValueVec.takeRev x)

keepMinimumMaskedCascade ::
   Exp (LLVM.Vector NumCounters (Bool, ((BlockId, BitId), Counter))) ->
   Exp ((BlockId, CounterId), BitId)
keepMinimumMaskedCascade =
   Expr.fst . Expr.snd
   .
   Expr.liftM
      (\x16 -> do
         x8 <- argMinMaskedVecHalf x16
         x4 <- argMinMaskedVecHalf x8
         x2 <- argMinMaskedVecHalf x4
         Monad.liftJoin2 (Expr.unliftM2 argMinMasked)
            (MultiValueVec.extract (LLVM.valueOf 0) (x2 :: IxVector TypeNum.D2))
            (MultiValueVec.extract (LLVM.valueOf 1) x2))
   .
   ExprVec.mapSnd
      (ExprVec.mapFst (ExprVec.mapFst (flip ExprVec.zip counterIds)))

{- |
In general this function will choose a different minimal element
than 'keepMinimumMasked'.
-}
keepMinimumMaskedVec ::
   IO (Phys.Array BlockDim Block ->
       Phys.Array (BlockDim, BitDim) Counters ->
       IO ((BlockId,CounterId),Subblock))
keepMinimumMaskedVec =
   Render.run $ \free ->
      Render.MarshalExp .
      Expr.mapSnd singleBit . keepMinimumMaskedCascade .
      argMinimumMaskedVec free


affectedRows ::
   IO (Phys.Array (SetDim,BlockDim) Block ->
       ((BlockId,CounterId),Subblock) -> IO [SetId])
affectedRows = do
   affected <-
      Render.run $ \arr ((j,k),bit) ->
         findIndices $ Symb.map (Expr.not . disjoint bit . extractBlock k) $
         Slice.apply (Slice.pickSnd j) $ Symb.fix arr
   return $ \arr bit -> Phys.toList =<< affected arr bit


minimize ::
   IO (Phys.Array BlockDim Block ->
       Phys.Array (SetDim,BlockDim) Block -> IO [SetId])
minimize = do
   smRows <- Render.run (sumRows . uninterleaveBits)
   affected <- affectedRows
   keepMin <- keepMinimumMaskedVec
   return $ \free arr -> affected arr =<< keepMin free =<< smRows arr

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
