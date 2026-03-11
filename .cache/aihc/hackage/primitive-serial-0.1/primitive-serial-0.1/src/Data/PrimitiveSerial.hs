module Data.PrimitiveSerial
    ( BSRead(..)
    , runWholeBSRead
    , bsRead
    , bsReadN
    , bsReadEverything
    , bsReadCount
    , decodeNative
    , encodeNative
    , FixedNumeric(..)
    , decodeLittleEndian
    , encodeLittleEndian
    , decodeBigEndian
    , encodeBigEndian
    ) where

import Control.Applicative
import Control.Monad
import Data.ByteString
import Data.ByteString.Internal
import Data.Int
import Data.Word
import Foreign
import Prelude hiding (drop, length, null, take)
import System.Endian
import System.IO.Unsafe

newtype BSRead a = MkBSRead
    { runBSRead :: StrictByteString -> Maybe (StrictByteString, a)
    }

instance Functor BSRead where
    fmap ab (MkBSRead f) = MkBSRead $ \bs -> fmap (fmap ab) $ f bs

instance Applicative BSRead where
    pure a = MkBSRead $ \bs -> Just (bs, a)
    MkBSRead fab <*> MkBSRead fa =
        MkBSRead $ \bs -> do
            (bs', ab) <- fab bs
            (bs'', a) <- fa bs'
            return (bs'', ab a)

instance Alternative BSRead where
    MkBSRead fa <|> MkBSRead fb = MkBSRead $ \bs -> fa bs <|> fb bs
    empty = MkBSRead $ \_ -> Nothing

instance Monad BSRead where
    MkBSRead f >>= q =
        MkBSRead $ \bs -> do
            (bs', a) <- f bs
            runBSRead (q a) bs'

instance MonadPlus BSRead

runWholeBSRead :: BSRead a -> StrictByteString -> Maybe a
runWholeBSRead ra bs = do
    (bs', a) <- runBSRead ra bs
    if null bs'
        then return a
        else Nothing

bsRead :: BSRead Word8
bsRead =
    MkBSRead $ \bs -> do
        w <- indexMaybe bs 0
        return (drop 1 bs, w)

bsReadN :: Int -> BSRead StrictByteString
bsReadN n =
    MkBSRead $ \bs -> do
        if n > length bs
            then Nothing
            else return (drop n bs, take n bs)

bsReadEverything :: BSRead StrictByteString
bsReadEverything = MkBSRead $ \bs -> Just (mempty, bs)

bsReadCount :: BSRead Int
bsReadCount = MkBSRead $ \bs -> Just (bs, length bs)

decodeNative ::
       forall a. Storable a
    => BSRead a
decodeNative = let
    typeSize :: Int
    typeSize = sizeOf (undefined :: a)
    in MkBSRead $ \bs@(BS fptr len) ->
           if typeSize <= len
               then Just $ (drop typeSize bs, unsafePerformIO $ withForeignPtr fptr $ \ptr -> peek $ castPtr ptr)
               else Nothing

encodeNative ::
       forall a. Storable a
    => a
    -> StrictByteString
encodeNative a = let
    typeSize :: Int
    typeSize = sizeOf (undefined :: a)
    in unsafePerformIO $ create typeSize $ \ptr -> poke (castPtr ptr) a

unsafeStorableCoerce ::
       forall a b. (Storable a, Storable b)
    => a
    -> b
unsafeStorableCoerce a = unsafePerformIO $ with a $ \ptr -> peek $ castPtr ptr

class Storable a => FixedNumeric a where
    nativeToLittleEndian :: a -> a
    nativeToBigEndian :: a -> a

instance FixedNumeric Word8 where
    nativeToLittleEndian = id
    nativeToBigEndian = id

instance FixedNumeric Word16 where
    nativeToLittleEndian = fromLE16
    nativeToBigEndian = fromBE16

instance FixedNumeric Word32 where
    nativeToLittleEndian = fromLE32
    nativeToBigEndian = fromBE32

instance FixedNumeric Word64 where
    nativeToLittleEndian = fromLE64
    nativeToBigEndian = fromBE64

instance FixedNumeric Int8 where
    nativeToLittleEndian = id
    nativeToBigEndian = id

instance FixedNumeric Int16 where
    nativeToLittleEndian = unsafeStorableCoerce . nativeToLittleEndian @Word16 . unsafeStorableCoerce
    nativeToBigEndian = unsafeStorableCoerce . nativeToBigEndian @Word16 . unsafeStorableCoerce

instance FixedNumeric Int32 where
    nativeToLittleEndian = unsafeStorableCoerce . nativeToLittleEndian @Word32 . unsafeStorableCoerce
    nativeToBigEndian = unsafeStorableCoerce . nativeToBigEndian @Word32 . unsafeStorableCoerce

instance FixedNumeric Int64 where
    nativeToLittleEndian = unsafeStorableCoerce . nativeToLittleEndian @Word64 . unsafeStorableCoerce
    nativeToBigEndian = unsafeStorableCoerce . nativeToBigEndian @Word64 . unsafeStorableCoerce

instance FixedNumeric Float where
    nativeToLittleEndian = unsafeStorableCoerce . nativeToLittleEndian @Word32 . unsafeStorableCoerce
    nativeToBigEndian = unsafeStorableCoerce . nativeToBigEndian @Word32 . unsafeStorableCoerce

instance FixedNumeric Double where
    nativeToLittleEndian = unsafeStorableCoerce . nativeToLittleEndian @Word64 . unsafeStorableCoerce
    nativeToBigEndian = unsafeStorableCoerce . nativeToBigEndian @Word64 . unsafeStorableCoerce

decodeLittleEndian ::
       forall a. FixedNumeric a
    => BSRead a
decodeLittleEndian = fmap nativeToLittleEndian $ decodeNative

encodeLittleEndian ::
       forall a. FixedNumeric a
    => a
    -> StrictByteString
encodeLittleEndian = encodeNative . nativeToLittleEndian

decodeBigEndian ::
       forall a. FixedNumeric a
    => BSRead a
decodeBigEndian = fmap nativeToBigEndian $ decodeNative

encodeBigEndian ::
       forall a. FixedNumeric a
    => a
    -> StrictByteString
encodeBigEndian = encodeNative . nativeToBigEndian
