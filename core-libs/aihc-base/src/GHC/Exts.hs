{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Exts
  ( module GHC.Prim,
    atomicModifyMutVar#,
    IsList (..),
    Item,
    IsString (..),
    Char (..),
    Int8 (..),
    Int16 (..),
    Int32 (..),
    Int64 (..),
    FunPtr (..),
    Constraint,
    coerce,
    lazy,
    inline,
    oneShot,
    runRW#,
    build,
    augment,
    Addr#,
    ByteArray#,
    copyAddrToByteArray#,
    Int#,
    MutableByteArray#,
    RealWorld,
    StablePtr#,
    Word#,
    RuntimeRep (..),
    VecCount (..),
    VecElem (..),
    Levity (..),
    TYPE,
    UnliftedType,
    Down (..),
    Ptr (..),
    Int (..),
    Float (..),
    Double (..),
    Word (..),
    Word8 (..),
    Word16 (..),
    Word32 (..),
    Word64 (..),
    isTrue#,
    and#,
    getSizeofMutableByteArray#,
    indexAddrArray#,
    indexDoubleArray#,
    indexFloatArray#,
    indexInt8Array#,
    indexInt16Array#,
    indexInt32Array#,
    indexInt64Array#,
    indexIntArray#,
    indexStablePtrArray#,
    indexWideCharArray#,
    indexWord8Array#,
    indexWord16Array#,
    indexWord32Array#,
    indexWord64Array#,
    indexWordArray#,
    int2Word#,
    neWord#,
    newByteArray#,
    nullAddr#,
    or#,
    readAddrArray#,
    readDoubleArray#,
    readFloatArray#,
    readInt8Array#,
    readInt16Array#,
    readInt32Array#,
    readInt64Array#,
    readIntArray#,
    readStablePtrArray#,
    readWideCharArray#,
    readWord8Array#,
    readWord16Array#,
    readWord32Array#,
    readWord64Array#,
    readWordArray#,
    sameMutableByteArray#,
    setByteArray#,
    sizeofByteArray#,
    uncheckedIShiftL#,
    uncheckedIShiftRA#,
    uncheckedIShiftRL#,
    uncheckedShiftL#,
    unsafeCoerce#,
    unsafeFreezeByteArray#,
    word2Int#,
    word8ToWord#,
    writeAddrArray#,
    writeDoubleArray#,
    writeFloatArray#,
    writeInt8Array#,
    writeInt16Array#,
    writeInt32Array#,
    writeInt64Array#,
    writeIntArray#,
    writeStablePtrArray#,
    writeWideCharArray#,
    writeWord8Array#,
    writeWord16Array#,
    writeWord32Array#,
    writeWord64Array#,
    writeWordArray#,
    xor#,
  )
where

import Data.Coerce (coerce)
import Data.Ord (Down (..))
import Data.String (IsString (..))
import GHC.Base (augment, build)
import GHC.Int (Int16 (..), Int32 (..), Int64 (..), Int8 (..))
import GHC.Internal.Char (Char (..))
import GHC.IsList (IsList (..))
import GHC.Prim
import GHC.Ptr (FunPtr (..), Ptr (..))
import GHC.Types (Bool (..), Constraint, Double (..), Float (..), Int (..), Levity (..), RuntimeRep (..), TYPE, UnliftedType, VecCount (..), VecElem (..), isTrue#)
import GHC.Word (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..))

-- | The value is returned unchanged. Strictness analysis does not apply.
lazy :: a -> a
lazy value = value

-- | The value is returned unchanged. Inlining hints do not apply.
inline :: a -> a
inline value = value

-- | The function is returned unchanged. Arity hints do not apply.
--
-- The argument and the result may have any runtime representation: a
-- worker that takes an @Int#@ index is the common caller.
oneShot :: forall (q :: RuntimeRep) (r :: RuntimeRep) (a :: TYPE q) (b :: TYPE r). (a -> b) -> a -> b
oneShot function = function

-- | Replace the contents of a mutable variable with the first component of
-- what the function gives, and return the second. GHC compiles this to an
-- RTS call that installs two selector thunks; the definition here builds the
-- same two thunks with @let@, which is why the pair is never forced. The
-- variable never yields between the read and the write, so a single-threaded
-- run cannot observe the intermediate state.
--
-- The type is GHC\'s: the pair the function gives is not visible in it, so
-- the coercion below is where the pair comes back.
atomicModifyMutVar# :: MutVar# d a -> (a -> b) -> State# d -> (# State# d, c #)
atomicModifyMutVar# reference modify state =
  case readMutVar# reference state of
    (# readState, current #) ->
      let pair = unsafeCoerce# (modify current)
          replacement = case pair of (next, _) -> next
          extra = case pair of (_, second) -> second
       in case writeMutVar# reference replacement readState of
            writtenState -> (# writtenState, extra #)
