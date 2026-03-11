module Sound.MED.Raw.InstrExt where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

import Foreign.Storable (Storable, sizeOf)

data InstrExt = InstrExt
  { hold               :: Maybe UBYTE
  , decay              :: Maybe UBYTE
  , suppress_midi_off  :: Maybe UBYTE
  , finetune           :: Maybe BYTE
  , default_pitch      :: Maybe UBYTE
  , instr_flags        :: Maybe UBYTE
  , long_midi_preset   :: Maybe UWORD
  , output_device      :: Maybe UBYTE
  , reserved           :: Maybe UBYTE
  , long_repeat        :: Maybe ULONG
  , long_replen        :: Maybe ULONG
  }
  deriving (Show)

{-# SPECIALISE peek :: UWORD -> PTR -> StorableReader InstrExt #-}
{-# SPECIALISE peek :: UWORD -> PTR -> ByteStringReader InstrExt #-}
peek :: (Reader m) => UWORD -> PTR -> m InstrExt
peek size p = do
  let peekHelp ::
        (Reader m, Storable a) => a -> Peek m a -> UWORD -> m (Maybe a)
      peekHelp elm peeker offset =
        skipIf (offset + fromIntegral (sizeOf elm) > size) $
        peeker (p + fromIntegral offset)
      peeker $?? offset = peekHelp undefined peeker offset
  hold' <- peekUBYTE $?? 0
  decay' <- peekUBYTE $?? 1
  suppress_midi_off' <- peekUBYTE $?? 2
  finetune' <- peekBYTE $?? 3
  default_pitch' <- peekUBYTE $?? 4
  instr_flags' <- peekUBYTE $?? 5
  long_midi_preset' <- peekUWORD $?? 6
  output_device' <- peekUBYTE $?? 8
  reserved' <- peekUBYTE $?? 9
  long_repeat' <- peekULONG $?? 10
  long_replen' <- peekULONG $?? 14
  return $ InstrExt
    hold' decay' suppress_midi_off' finetune' default_pitch'
    instr_flags' long_midi_preset' output_device' reserved'
    long_repeat' long_replen'
