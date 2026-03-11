module Sound.MED.Basic.AmigaPrivate where

import qualified Sound.MED.Basic.Storable as MedStore
import qualified Sound.MED.Basic.ByteString as MedBytes
import Sound.MED.Basic.Storable (MEM)
import Data.ByteString (ByteString)
import Sound.MED.Basic.Utility (PTR, LONG, ULONG, WORD, UWORD, BYTE, UBYTE)

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.Reader as MR


type Peek m a = PTR -> m a

class (MonadFail m) => Reader m where
  peekLONG  :: Peek m LONG
  peekULONG :: Peek m ULONG
  peekWORD  :: Peek m WORD
  peekUWORD :: Peek m UWORD
  peekBYTE  :: Peek m BYTE
  peekUBYTE :: Peek m UBYTE

{-# INLINE peekPTR #-}
peekPTR :: (Reader m) => Peek m PTR
peekPTR = peekULONG


newtype StorableReader a = StorableReader (MR.ReaderT MEM IO a)

instance Functor StorableReader where
  fmap f (StorableReader act) = StorableReader $ fmap f act

instance Applicative StorableReader where
  pure = StorableReader . pure
  StorableReader f <*> StorableReader m = StorableReader (f <*> m)

instance Monad StorableReader where
  return = pure
  StorableReader x >>= f  =
    StorableReader  $  x >>= \a -> case f a of StorableReader y -> y

instance MonadFail StorableReader where
  fail = StorableReader . MT.lift . fail

runStorable :: StorableReader a -> MEM -> IO a
runStorable (StorableReader rd) = MR.runReaderT rd

{-# INLINE liftStorable #-}
liftStorable :: MedStore.Peek a -> PTR -> StorableReader a
liftStorable peek ptr = StorableReader (MR.ReaderT $ \mem -> peek mem ptr)

instance Reader StorableReader where
  peekLONG  = liftStorable MedStore.peekBig
  peekULONG = liftStorable MedStore.peekBig
  peekWORD  = liftStorable MedStore.peekBig
  peekUWORD = liftStorable MedStore.peekBig
  peekBYTE  = liftStorable MedStore.peekOffset
  peekUBYTE = liftStorable MedStore.peekOffset


newtype
  ByteStringReader a =
    ByteStringReader (MR.ReaderT ByteString (Either String) a)

instance Functor ByteStringReader where
  fmap f (ByteStringReader act) = ByteStringReader $ fmap f act

instance Applicative ByteStringReader where
  pure = ByteStringReader . pure
  ByteStringReader f <*> ByteStringReader m = ByteStringReader (f <*> m)

instance Monad ByteStringReader where
  return = pure
  ByteStringReader x >>= f  =
    ByteStringReader  $  x >>= \a -> case f a of ByteStringReader y -> y

instance MonadFail ByteStringReader where
  fail = ByteStringReader . MT.lift . Left

runByteString :: ByteStringReader a -> ByteString -> Either String a
runByteString (ByteStringReader rd) = MR.runReaderT rd

{-# INLINE liftByteString #-}
liftByteString :: MedBytes.Peek a -> PTR -> ByteStringReader a
liftByteString peek ptr =
  ByteStringReader (MR.ReaderT $ \mem -> Right $ peek mem ptr)

instance Reader ByteStringReader where
  peekLONG  = liftByteString MedBytes.peekInt32
  peekULONG = liftByteString MedBytes.peekWord32
  peekWORD  = liftByteString MedBytes.peekInt16
  peekUWORD = liftByteString MedBytes.peekWord16
  peekBYTE  = liftByteString MedBytes.peekInt8
  peekUBYTE = liftByteString MedBytes.peekWord8
