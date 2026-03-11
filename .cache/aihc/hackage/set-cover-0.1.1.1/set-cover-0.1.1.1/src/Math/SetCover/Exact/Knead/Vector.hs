{-# LANGUAGE TypeFamilies #-}
module Math.SetCover.Exact.Knead.Vector (
   ByteVector, Block(..),
   ) where

import qualified Math.SetCover.Exact.Knead as ESC_Knead
import qualified Math.SetCover.Exact.Block as Blocks
import Math.SetCover.Exact.Knead (BitSet)

import Control.Monad ((<=<))
import Control.Applicative (liftA2)

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM
import qualified Type.Data.Num.Decimal as TypeNum

import qualified Foreign.Storable as Store
import Foreign.Storable (Storable)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr (Ptr, castPtr)
import Data.Storable.Endian (peekLE, pokeLE)

import qualified Data.NonEmpty.Mixed as NonEmptyM
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Data.Word (Word8, Word64)
import Data.Bits (shiftL, shiftR)


type ByteVector = LLVM.Vector TypeNum.D16 Word8
data Block = Block {block0, block1 :: !Word64}

blockSize :: Int
blockSize = 2


{-# INLINE getByte #-}
getByte :: Int -> Word64 -> Word8
getByte k x = fromIntegral $ shiftR x (k*8)

{-# INLINE _putByte #-}
_putByte :: Int -> Word8 -> Word64
_putByte k x = shiftL (fromIntegral x) (k*8)


instance Storable Block where
   sizeOf = (blockSize*) . Store.sizeOf . block0
   alignment = (blockSize*) . Store.alignment . block0
   poke ptr (Block x0 x1) = do
      let ptr64 = castPtr ptr
      pokeLE ptr64 x0
      pokeLE (advancePtr ptr64 1) x1
   peek ptr =
      let ptr64 = castPtr ptr
      in  liftA2 Block (peekLE ptr64) (peekLE (advancePtr ptr64 1))

instance MultiValue.C Block where
   type Repr Block = LLVM.Value ByteVector
   cons = MultiValue.consPrimitive . blockVector
   undef = MultiValue.undefPrimitive
   zero = MultiValue.zeroPrimitive
   phi = MultiValue.phiPrimitive
   addPhi = MultiValue.addPhiPrimitive

blockVector :: Block -> ByteVector
blockVector (Block x0 x1) =
   LLVM.vector $
   fmap
      (\k ->
         let split = Store.sizeOf x0
         in if k<split then getByte k x0 else getByte (k-split) x1) $
   NonEmptyC.iterate (1+) 0

instance MultiValue.Logic Block where
   and = MultiValue.liftM2 LLVM.and; or = MultiValue.liftM2 LLVM.or
   xor = MultiValue.liftM2 LLVM.xor; inv = MultiValue.liftM LLVM.inv

instance Storable.C Block where
   load = fmap MultiValue.cast . Storable.load <=< castBlockPtr
   store b = Storable.store (MultiValue.cast b) <=< castBlockPtr

castBlockPtr ::
   LLVM.Value (Ptr Block) ->
   LLVM.CodeGenFunction r (LLVM.Value (Ptr ByteVector))
castBlockPtr = LLVM.bitcast


toWord128 ::
   LLVM.Value ByteVector ->
   LLVM.CodeGenFunction r (LLVM.Value (LLVM.WordN TypeNum.D128))
toWord128 = LLVM.bitcast

fromWord128 ::
   LLVM.Value (LLVM.WordN TypeNum.D128) ->
   LLVM.CodeGenFunction r (LLVM.Value ByteVector)
fromWord128 = LLVM.bitcast

instance BitSet Block where
   nullBlock =
      Expr.liftReprM (\x ->
         A.cmp LLVM.CmpEQ (LLVM.value LLVM.zero) =<< toWord128 x)
   blocksFromSets sets =
      let (avails, free) = Blocks.blocksFromSets sets
          numBlocks = - div (- length free) blockSize
          makeBlock (NonEmpty.Cons x0 (NonEmpty.Cons x1 Empty.Cons)) =
            Block x0 x1
          sliceRow =
            take numBlocks . map makeBlock . fst .
            NonEmptyM.sliceVertical . (++ repeat 0)
      in  (map sliceRow avails, sliceRow free)
   keepMinimumBit =
      Expr.liftReprM (\x0 ->
         do x <- toWord128 x0; fromWord128 =<< A.and x =<< A.neg x)
