module Math.SetCover.Exact.Knead.Symbolic (
   BitSet(..),
   Block,

   SetId, SetDim, BlockId, BlockDim, DigitId, DigitDim,
   sumBags3,
   difference,
   getRow,
   nullSet,
   disjoint,
   differenceWithRow,
   findIndices,
   filterDisjointRows,
   ) where

import qualified Math.SetCover.Exact.Block as Blocks

import Control.Monad.HT ((<=<))
import Control.Applicative (liftA2, (<$>))

import qualified Data.Array.Knead.Symbolic.Render as Render
import qualified Data.Array.Knead.Symbolic.Physical as Phys
import qualified Data.Array.Knead.Symbolic as Symb
import qualified Data.Array.Knead.Symbolic.Slice as Slice
import qualified Data.Array.Knead.Shape as Shape
import qualified LLVM.DSL.Expression as Expr
import Data.Array.Knead.Symbolic ((!))
import LLVM.DSL.Expression
         (Exp, (==*), (<*), xor, (.|.*), (.&.*), )

import qualified Data.Array.Comfort.Shape as ComfortShape
import qualified Data.Array.Comfort.Storable.Unchecked as ComfortArray
import qualified Data.Array.Comfort.Boxed as Array
import Data.Array.Comfort.Boxed (Array)

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value as MultiValue
import LLVM.Extra.Multi.Value (atom)

import qualified Data.Word as Word
import qualified Data.Int as Int
import Data.Set (Set)

import Foreign.Storable.Record.Tuple (Tuple(Tuple))

import Prelude2010
import Prelude ()



class (MultiValue.Logic block) => BitSet block where
   nullBlock :: Exp block -> Exp Bool
   blocksFromSets :: (Ord a) => [Set a] -> ([[block]], [block])
   keepMinimumBit :: Exp block -> Exp block

instance BitSet Word.Word8 where
   nullBlock block = block ==* Expr.zero
   blocksFromSets sets = Blocks.blocksFromSets sets
   keepMinimumBit = keepMinimumBitPrim

instance BitSet Word.Word16 where
   nullBlock block = block ==* Expr.zero
   blocksFromSets sets = Blocks.blocksFromSets sets
   keepMinimumBit = keepMinimumBitPrim

instance BitSet Word.Word32 where
   nullBlock block = block ==* Expr.zero
   blocksFromSets sets = Blocks.blocksFromSets sets
   keepMinimumBit = keepMinimumBitPrim

instance BitSet Word.Word64 where
   nullBlock block = block ==* Expr.zero
   blocksFromSets sets = Blocks.blocksFromSets sets
   keepMinimumBit = keepMinimumBitPrim

keepMinimumBitPrim ::
   (MultiValue.Additive a, MultiValue.Logic a) => Exp a -> Exp a
keepMinimumBitPrim =
   Expr.liftM (\x -> MultiValue.and x =<< MultiValue.neg x)



type Block = Word.Word64

-- SetId must allow negative numbers since it is used for empty plain Arrays
type SetId = Int.Int32
type BlockId = Int.Int32
type DigitId = Word.Word32

type SetDim = Shape.ZeroBased SetId
type BlockDim = Shape.ZeroBased BlockId
type DigitDim = Shape.ZeroBased DigitId


addLow, addHigh :: MultiValue.Logic a => Exp a -> Exp a -> Exp a -> Exp a
addLow a b c = a `xor` b `xor` c
addHigh a b c = c.&.*(a.|.*b) .|.* a.&.*b

add2 ::
   IO (Phys.Array ((SetDim, BlockDim), DigitDim) Block ->
       IO (Phys.Array ((SetDim, BlockDim), DigitDim) Block))
add2 =
   Render.run $ \xs ->
   Render.MapAccumLSimple
      (Expr.modify2 atom (Tuple (atom,atom)) $ \carry (Tuple (a,b)) ->
         (addHigh a b carry, addLow a b carry))
      (Symb.fill (Expr.fst (Symb.shape xs)) Expr.zero)
      (Symb.map Expr.tuple $ halfBags xs)


zbAtom :: Shape.ZeroBased (MultiValue.Atom a)
zbAtom = Shape.ZeroBased atom

halfBags ::
   Symb.Array ((SetDim, BlockDim), DigitDim) Block ->
   Symb.Array ((SetDim, BlockDim), DigitDim) (Block,Block)
halfBags xs =
   Symb.map
      (Expr.modify2 ((zbAtom, atom), zbAtom) ((atom,atom),atom)
         (\((Shape.ZeroBased numSets, _shBlocks), Shape.ZeroBased numDigits)
           ((n,j),k) ->
            elseIfThen Expr.zero (k<*numDigits) $
            Expr.zip
               (xs ! Expr.zip (Expr.zip (2*n) j) k)
               (elseIfThen Expr.zero (2*n+1<*numSets)
                  (xs ! Expr.zip (Expr.zip (2*n+1) j) k)))
         (Symb.shape xs)) $
   Symb.id
      (Expr.modify ((zbAtom, atom), zbAtom)
         (\((Shape.ZeroBased numSets, shBlocks), Shape.ZeroBased numDigits) ->
            ((Shape.ZeroBased (Expr.idiv (numSets+1) 2), shBlocks),
             Shape.ZeroBased (numDigits+1)))
         (Symb.shape xs))

elseIfThen :: MultiValue.C a => Exp a -> Exp Bool -> Exp a -> Exp a
elseIfThen y c x = Expr.ifThenElse c x y


removeDimension ::
   IO (Phys.Array ((SetDim, BlockDim), DigitDim) Block ->
       IO (Phys.Array (DigitDim, BlockDim) Block))
removeDimension =
   Render.run $
      Symb.fix .
      Slice.apply
         (Slice.first (Slice.pickFst Expr.zero)
          `Slice.compose`
          Slice.transpose)

sumLoop ::
   IO (Phys.Array ((SetDim, BlockDim), DigitDim) Block ->
       IO (Phys.Array (DigitDim, BlockDim) Block))
sumLoop = do
   runAdd2 <- add2
   remDim <- removeDimension

   let go xs =
         if (ComfortShape.zeroBasedSize $ fst $ fst $ Phys.shape xs) > 1
           then go =<< runAdd2 xs
           else remDim xs

   return go

addSingleDim :: Phys.Array sh a -> Phys.Array (sh,DigitDim) a
addSingleDim = ComfortArray.mapShape (flip (,) (Shape.ZeroBased 1))

{-
ToDo:
We could use a carry-save adder that would enable more parallelism.
Unfortunately, currently we cannot benefit from this opportunity.
-}
_sumBags ::
   IO (Phys.Array (SetDim,BlockDim) Block ->
       IO (Phys.Array (DigitDim,BlockDim) Block))
_sumBags = (.addSingleDim) <$> sumLoop


{- |
A faster first addition step.
In the first addition we do not need to propagate carry.
We use this fact for reducing the number of rows to a third.
-}
_add3 ::
   IO (Phys.Array (SetDim, BlockDim) Block ->
       IO (Phys.Array ((SetDim, BlockDim), DigitDim) Block))
_add3 =
   Render.run $ \xs ->
   Symb.mapWithIndex
      (Expr.modify2 (atom,atom) (atom,atom,atom) $ \(_,k) (a,b,c) ->
         Expr.ifThenElse (k ==* Expr.zero) (addLow a b c) (addHigh a b c))
      (Slice.apply (Slice.extrudeSnd digitDim2) $ thirdBags xs)

add3 ::
   IO (Phys.Array (SetDim, BlockDim) Block ->
       IO (Phys.Array ((SetDim, BlockDim), DigitDim) Block))
add3 =
   Render.run $
   Render.AddDimension digitDim2
      (Expr.modify2 atom (atom,atom,atom) $ \k (a,b,c) ->
         Expr.ifThenElse (k ==* Expr.zero) (addLow a b c) (addHigh a b c))
   .
   thirdBags

digitDim2 :: Exp DigitDim
digitDim2 = Expr.compose $ Shape.ZeroBased $ Expr.fromInteger' 2

thirdBags ::
   Symb.Array (SetDim, BlockDim) Block ->
   Symb.Array (SetDim, BlockDim) (Block,Block,Block)
thirdBags xs =
   Symb.map
      (Expr.modify (atom,atom)
         (\(n,j) ->
            Expr.zip3
               (xs ! Expr.zip (3*n) j)
               (condAccess xs (3*n+1) j)
               (condAccess xs (3*n+2) j))) $
   Symb.id
      (Expr.mapFst
         (Expr.modify zbAtom
            (\(Shape.ZeroBased numSets) ->
               Shape.ZeroBased $ Expr.idiv (numSets+2) 3))
         (Symb.shape xs))

condAccess ::
   Symb.Array (SetDim, BlockDim) Block -> Exp SetId -> Exp BlockId -> Exp Block
condAccess xs n j =
   Expr.ifThenElse (n <* Shape.zeroBasedSize (Expr.fst (Symb.shape xs)))
      (xs ! Expr.zip n j) Expr.zero

sumBags3 ::
   IO (Phys.Array (SetDim,BlockDim) Block ->
       IO (Phys.Array (DigitDim,BlockDim) Block))
sumBags3 = liftA2 (<=<) sumLoop add3


difference :: (MultiValue.Logic a) => Exp a -> Exp a -> Exp a
difference x y = x .&.* Expr.complement y

differenceWithRow ::
   (Shape.C k, MultiValue.Logic block) =>
   Symb.Array BlockDim block -> Exp (Shape.Index k) ->
   Symb.Array (k,BlockDim) block -> Symb.Array BlockDim block
differenceWithRow x k bag =
   Symb.zipWith difference x (getRow k bag)


disjoint :: (BitSet block) => Exp block -> Exp block -> Exp Bool
disjoint x y  =  nullBlock $ x .&.* y

getRow ::
   (Shape.C k, MultiValue.C block) =>
   Exp (Shape.Index k) ->
   Symb.Array (k, BlockDim) block -> Symb.Array BlockDim block
getRow k = Slice.apply (Slice.pickFst k)

nullSet :: (BitSet block) => Symb.Array BlockDim block -> Exp Bool
nullSet =
   Expr.maybe Expr.true (const Expr.false) . Symb.findAll (Expr.not . nullBlock)

disjointRow ::
   (BitSet block) =>
   Exp SetId -> Exp SetId -> Symb.Array (SetDim, BlockDim) block -> Exp Bool
disjointRow k0 k1 bag =
   nullSet $ Symb.zipWith (.&.*) (getRow k0 bag) (getRow k1 bag)

disjointRows ::
   (BitSet block) =>
   Exp SetId -> Symb.Array (SetDim,BlockDim) block -> Symb.Array SetDim Bool
disjointRows k0 sets =
   Symb.map
      (\k1 -> disjointRow k0 k1 sets)
      (Symb.id (Expr.fst (Symb.shape sets)))


findIndices ::
   Symb.Array SetDim Bool -> Render.MapFilter SetDim (SetId,Bool) SetId
findIndices arr =
   Render.MapFilter Expr.fst Expr.snd
      (Symb.zip (Symb.id $ Symb.shape arr) arr)

findDisjointRows ::
   (BitSet block, Storable.C block) =>
   IO (SetId ->
       Phys.Array (SetDim,BlockDim) block ->
       IO (Phys.Array SetDim SetId))
findDisjointRows =
   Render.run $ \k -> findIndices . disjointRows k

collectRows ::
   (MultiValue.C block) =>
   Symb.Array SetDim SetId ->
   Symb.Array (SetDim,BlockDim) block -> Symb.Array (SetDim,BlockDim) block
collectRows rows sets =
   Symb.backpermute
      (Expr.mapFst (const $ Symb.shape rows) (Symb.shape sets))
      (Expr.mapFst (rows!))
      sets

filterDisjointRows ::
   (BitSet block, Storable.C block) =>
   IO (SetId ->
       (Array SetDim label, Phys.Array (SetDim,BlockDim) block) ->
       IO (Array SetDim label, Phys.Array (SetDim,BlockDim) block))
filterDisjointRows = do
   disjRows <- findDisjointRows
   collect <- Render.run collectRows
   return $ \k (labels,sets) -> do
      perm <- disjRows k sets
      liftA2 (,)
         (Array.fromList (Phys.shape perm) . map (labels Array.!)
            <$> Phys.toList perm)
         (collect perm sets)
