module Data.Store.Streaming.Internal
  ( messageMagic
  , magicLength
  , sizeTagLength
  , headerLength
  , SizeTag
  ) where

import           Data.Word (Word64)
import qualified Foreign.Storable as Storable

-- | Type used to store the length of a 'Message'.
type SizeTag = Int

-- | Some fixed arbitrary magic number that precedes every 'Message'.
messageMagic :: Word64
messageMagic = 18205256374652458875

magicLength :: Int
magicLength = Storable.sizeOf messageMagic

sizeTagLength :: Int
sizeTagLength = Storable.sizeOf (undefined :: SizeTag)

headerLength :: Int
headerLength = sizeTagLength + magicLength