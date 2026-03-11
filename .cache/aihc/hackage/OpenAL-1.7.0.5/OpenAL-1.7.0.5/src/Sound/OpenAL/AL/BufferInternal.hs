{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.BufferInternal
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Buffer.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.BufferInternal (
   Buffer(..), marshalBuffer, unmarshalBuffer
) where

#if __GLASGOW_HASKELL__ >= 704
-- Make the foreign imports happy.
import Foreign.C.Types
#endif

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.ObjectName ( ObjectName(..), GeneratableObjectName(..) )
import Foreign.Marshal.Array ( withArrayLen, peekArray, allocaArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( Storable(..) )

import Sound.OpenAL.AL.ALboolean
import Sound.OpenAL.AL.BasicTypes
import Sound.OpenAL.AL.PeekPoke

--------------------------------------------------------------------------------

-- | The abstract buffer type.

newtype Buffer = Buffer { bufferID :: ALuint }
   deriving ( Eq, Ord, Show )
 
-- | A dummy buffer.

nullBuffer :: Buffer
nullBuffer = Buffer 0

marshalBuffer :: Maybe Buffer -> ALuint
marshalBuffer = bufferID . maybe nullBuffer id

unmarshalBuffer :: ALuint -> Maybe Buffer
unmarshalBuffer b =
   if b == bufferID nullBuffer then Nothing else Just (Buffer b)

instance Storable Buffer where
   sizeOf    ~(Buffer b) = sizeOf b
   alignment ~(Buffer b) = alignment b
   peek                  = peek1 Buffer . castPtr
   poke ptr   (Buffer b) = poke1 (castPtr ptr) b

--------------------------------------------------------------------------------
-- This should really be in Sound.OpenAL.AL.Buffer, but we have it here to
-- avoid an orphan module.

instance ObjectName Buffer where
   deleteObjectNames buffers =
      liftIO $ withArrayLen buffers $ alDeleteBuffers . fromIntegral

   isObjectName = liftIO . fmap unmarshalALboolean . alIsBuffer

instance GeneratableObjectName Buffer where
   genObjectNames n = liftIO $
      allocaArray n $ \buf -> do
        alGenBuffers (fromIntegral n) buf
        peekArray n buf

foreign import ccall unsafe "alGenBuffers"
   alGenBuffers :: ALsizei -> Ptr Buffer -> IO ()

foreign import ccall unsafe "alDeleteBuffers"
   alDeleteBuffers :: ALsizei -> Ptr Buffer -> IO ()

foreign import ccall unsafe "alIsBuffer"
   alIsBuffer :: Buffer -> IO ALboolean
