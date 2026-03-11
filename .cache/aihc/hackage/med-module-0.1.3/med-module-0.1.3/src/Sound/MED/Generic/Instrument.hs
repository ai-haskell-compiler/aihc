module Sound.MED.Generic.Instrument where

import qualified Sound.MED.Raw.MMD0Sample as MMD0Sample
import qualified Sound.MED.Raw.InstrHdr as InstrHdr
import qualified Sound.MED.Raw.MMDInstrInfo as MMDInstrInfo
import qualified Sound.MED.Raw.InstrExt as InstrExt

import Sound.MED.Basic.Human(Human(human))
import Sound.MED.Basic.Utility(stringFromBytes)

import qualified Data.List as List

data MEDInstrument = MEDInstrument
  { rep                :: Maybe Int
  , replen             :: Maybe Int
  , midich             :: Maybe Int
  , midipreset         :: Maybe Int
  , svol               :: Maybe Int
  , strans             :: Maybe Int
  , hold               :: Maybe Int
  , decay              :: Maybe Int
  , suppress_midi_off  :: Maybe Int
  , finetune           :: Maybe Int
  , default_pitch      :: Maybe Int
  , instr_flags        :: Maybe Int
  , long_midi_preset   :: Maybe Int
  , output_device      :: Maybe Int
  , long_repeat        :: Maybe Int
  , long_replen        :: Maybe Int
  , name               :: Maybe String
  }
  deriving (Show)

medinstruments ::
  [Maybe InstrHdr.InstrHdr] -> [MMD0Sample.MMD0Sample] ->
  [MMDInstrInfo.MMDInstrInfo] -> [InstrExt.InstrExt] ->
  [MEDInstrument]
medinstruments hdrs samples infos exts =
  let pad xs = map Just xs ++ repeat Nothing
  in  take (maximum [length hdrs, length samples, length infos, length exts]) $
      List.zipWith4 medinstrument
        (hdrs ++ repeat Nothing) (pad samples) (pad infos) (pad exts)

medinstrument ::
  Maybe InstrHdr.InstrHdr -> Maybe MMD0Sample.MMD0Sample ->
  Maybe MMDInstrInfo.MMDInstrInfo -> Maybe InstrExt.InstrExt -> MEDInstrument
medinstrument _h s i e =
  let rep' = fmap (fromIntegral . MMD0Sample.rep) s
      replen' = fmap (fromIntegral . MMD0Sample.replen) s
      midich' = fmap (fromIntegral . MMD0Sample.midich) s
      midipreset' = fmap (fromIntegral . MMD0Sample.midipreset) s
      svol' = fmap (fromIntegral . MMD0Sample.svol) s
      strans' = fmap (fromIntegral . MMD0Sample.strans) s
      hold' = fmap fromIntegral . InstrExt.hold =<< e
      decay' = fmap fromIntegral . InstrExt.decay =<< e
      suppress_midi_off' = fmap fromIntegral . InstrExt.suppress_midi_off =<< e
      finetune' = fmap fromIntegral . InstrExt.finetune =<< e
      default_pitch' = fmap fromIntegral . InstrExt.default_pitch =<< e
      instr_flags' = fmap fromIntegral . InstrExt.instr_flags =<< e
      long_midi_preset' = fmap fromIntegral . InstrExt.long_midi_preset =<< e
      output_device' = fmap fromIntegral . InstrExt.output_device =<< e
      long_repeat' = fmap fromIntegral . InstrExt.long_repeat =<< e
      long_replen' = fmap fromIntegral . InstrExt.long_replen =<< e
      name' = fmap stringFromBytes . MMDInstrInfo.name =<< i
  in MEDInstrument
    rep' replen' midich' midipreset' svol' strans' hold' decay'
    suppress_midi_off' finetune' default_pitch' instr_flags'
    long_midi_preset' output_device' long_repeat' long_replen' name'

instance Human MEDInstrument where
  human i = show i ++ "\n"
