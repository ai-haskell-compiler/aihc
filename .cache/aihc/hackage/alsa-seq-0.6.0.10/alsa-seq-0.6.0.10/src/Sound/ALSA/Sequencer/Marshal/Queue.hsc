--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Marshal.Queue
-- Copyright : (c) Henning Thielemann, 2010
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- PRIVATE MODULE.
--
-- Here we have the various types used by the library,
-- and how they are imported\/exported to C.
--
-- We use Hsc for expanding C types to Haskell types like Word32.
-- However if a C type is translated to Word32
-- you should not assume that it is translated to Word32 on every platform.
-- On a 64bit machine it may well be Word64.
-- Thus you should use our wrapper types whereever possible.
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.ALSA.Sequencer.Marshal.Queue where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Marshal/Template.h>

import qualified Sound.ALSA.Sequencer.Utility as U

import qualified Foreign.C.Types as C
import Foreign.Storable (Storable, sizeOf, alignment, peek, peekByteOff, poke, pokeByteOff, )

import Data.Array (Ix, )

import qualified Data.Word as Word




-- | The type of queue identifiers.
newtype T =
   Cons #{intfieldtype snd_seq_event_t, queue}
      deriving (Eq, Ord, Storable)

instance Show T where
   showsPrec prec (Cons x) =
      U.showsRecord prec "Queue" [U.showsField x]

imp :: C.CInt -> T
imp x = Cons (fromIntegral x)

exp :: T -> C.CInt
exp (Cons x) = fromIntegral x

#{enum T, Cons
 , direct = SND_SEQ_QUEUE_DIRECT
 }


data Skew =
   Skew {
      skewValue :: !(#{intfieldtype snd_seq_queue_skew_t, value}),
      skewBase  :: !(#{intfieldtype snd_seq_queue_skew_t, base})
   } deriving (Show, Eq)

instance Storable Skew where
  sizeOf _    = #{size snd_seq_queue_skew_t}
  alignment _ = #{alignment snd_seq_queue_skew_t}
  peek p      = do v <- #{peek snd_seq_queue_skew_t, value} p
                   b <- #{peek snd_seq_queue_skew_t, base}  p
                   return Skew { skewValue = v
                               , skewBase  = b
                               }
  poke p v    = #{poke snd_seq_queue_skew_t, value} p (skewValue v)
             >> #{poke snd_seq_queue_skew_t, base}  p (skewBase  v)


#{newintfieldtype "Position", snd_seq_ev_queue_control_t, param.position}
