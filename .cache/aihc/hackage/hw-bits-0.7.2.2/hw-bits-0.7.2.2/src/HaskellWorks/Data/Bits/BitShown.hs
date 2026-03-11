{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.Bits.BitShown
  ( BitShown(..)
  , bitShown
  ) where

import Control.DeepSeq
import Data.Maybe
import Data.String
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FromByteString

import qualified Data.ByteString as BS

-- | Tag for a value describe the value as being able to be shown as a bit string
newtype BitShown a = BitShown { unBitShown :: a } deriving (Eq, BitRead, BitShow, Generic, NFData)

deriving instance Functor BitShown

instance BitRead a => IsString (BitShown a) where
  fromString = fromJust . bitRead

instance BitShow a => Show (BitShown a) where
  show a = bitShows a ""

-- | Show the value as a bit string
bitShown :: BitShown a -> a
bitShown (BitShown a) = a

deriving instance TestBit a => TestBit (BitShown a)

instance FromByteString (BitShown [Bool]) where
  fromByteString = BitShown . BS.foldr gen []
    where gen :: Word8 -> [Bool] -> [Bool]
          gen w bs =
            (w .&. 0x01 /= 0) :
            (w .&. 0x02 /= 0) :
            (w .&. 0x04 /= 0) :
            (w .&. 0x08 /= 0) :
            (w .&. 0x10 /= 0) :
            (w .&. 0x20 /= 0) :
            (w .&. 0x40 /= 0) :
            (w .&. 0x80 /= 0) : bs
