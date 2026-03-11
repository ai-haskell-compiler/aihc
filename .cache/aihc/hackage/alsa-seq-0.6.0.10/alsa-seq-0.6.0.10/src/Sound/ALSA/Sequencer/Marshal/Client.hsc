--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Marshal
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
module Sound.ALSA.Sequencer.Marshal.Client where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Marshal/Template.h>

import qualified Sound.ALSA.Sequencer.Utility as U
import qualified Foreign.C.Types as C
import qualified Data.Word as Word
import Foreign.Storable (Storable, )


-- | The type of client identifiers.
newtype T =
   Cons #{intfieldtype snd_seq_addr_t, client}
      deriving (Eq, Ord, Storable)

instance Show T where
   showsPrec prec (Cons x) =
      U.showsRecord prec "Client" [U.showsField x]


#{enum T, Cons
 , system      = SND_SEQ_CLIENT_SYSTEM
 , subscribers = SND_SEQ_ADDRESS_SUBSCRIBERS
 , broadcast   = SND_SEQ_ADDRESS_BROADCAST
 , unknown     = SND_SEQ_ADDRESS_UNKNOWN
 }



exp :: T -> C.CInt
exp (Cons c) = fromIntegral c

imp :: C.CInt -> T
imp p = Cons (fromIntegral p)

-- | The different types of clients.
data Type = User | Kernel

impType :: C.CInt -> Type
impType x =
   if x == #{const SND_SEQ_USER_CLIENT}
     then User
     else Kernel
