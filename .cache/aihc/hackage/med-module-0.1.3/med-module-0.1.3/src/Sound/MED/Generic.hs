module Sound.MED.Generic where

import qualified Sound.MED.Raw.MMD0 as MMD0
import qualified Sound.MED.Raw.MMD1 as MMD1
import qualified Sound.MED.Raw.MMD2 as MMD2
import qualified Sound.MED.Raw.MMD3 as MMD3

import qualified Sound.MED.Raw.MMD0exp as MMD0exp
import qualified Sound.MED.Raw.MMD0Song as MMD0Song
import qualified Sound.MED.Raw.MMD2Song as MMD2Song

import qualified Sound.MED.Generic.Block as MEDBlock
import qualified Sound.MED.Generic.Instrument as MEDInstrument
import qualified Sound.MED.Generic.PlaySeq as MEDPlaySeq
import qualified Sound.MED.Generic.Tempo as MEDTempo
import Sound.MED.Generic.Instrument(medinstruments)

import Sound.MED.Basic.Human(Human(human))
import Sound.MED.Basic.Amiga

import Control.Exception (bracket)
import Data.Foldable (foldMap)
import Text.Printf (printf)


data MED = MED
  { instrs :: [MEDInstrument.MEDInstrument]
  , blocks :: [MEDBlock.MEDBlock]
  , playseqs :: [MEDPlaySeq.MEDPlaySeq]
  , tempo :: MEDTempo.MEDTempo
  }

peek :: (Reader m) => m MED
peek = do
  ident <- peekULONG 0
  let samples0 song =
        take (fromIntegral (MMD0Song.numsamples song)) $ MMD0Song.sample song
  let samples2 song =
        take (fromIntegral (MMD2Song.numsamples song)) $ MMD2Song.sample song
  let instrs_ smplarr samples expdata med =
        medinstruments (smplarr med) (samples med)
          (foldMap MMD0exp.iinfo $ expdata med)
          (foldMap MMD0exp.exp_smp $ expdata med)
  let mmd0PlaySeq song = [MEDPlaySeq.playSeq0 song]
  let mmd2PlaySeqs = map MEDPlaySeq.playSeq2 . MMD2Song.playseqtable
  case ident of
    0x4D4D4430 -> do
      med <- MMD0.peek 0
      return $ MED
        { instrs = instrs_ MMD0.smplarr (samples0 . MMD0.song) MMD0.expdata med
        , blocks = map MEDBlock.medblock0 $ MMD0.blockarr med
        , playseqs = mmd0PlaySeq $ MMD0.song med
        , tempo = MEDTempo.song0Tempo $ MMD0.song med
        }
    0x4D4D4431 -> do
      med <- MMD1.peek 0
      return $ MED
        { instrs = instrs_ MMD1.smplarr (samples0 . MMD1.song) MMD1.expdata med
        , blocks = map MEDBlock.medblock1 $ MMD1.blockarr med
        , playseqs = mmd0PlaySeq $ MMD1.song med
        , tempo = MEDTempo.song0Tempo $ MMD1.song med
        }
    0x4D4D4432 -> do
      med <- MMD2.peek 0
      return $ MED
        { instrs = instrs_ MMD2.smplarr (samples2 . MMD2.song) MMD2.expdata med
        , blocks = map MEDBlock.medblock1 $ MMD2.blockarr med
        , playseqs = mmd2PlaySeqs $ MMD2.song med
        , tempo = MEDTempo.song2Tempo $ MMD2.song med
        }
    0x4D4D4433 -> do
      med <- MMD3.peek 0
      return $ MED
        { instrs = instrs_ MMD3.smplarr (samples2 . MMD3.song) MMD3.expdata med
        , blocks = map MEDBlock.medblock1 $ MMD3.blockarr med
        , playseqs = mmd2PlaySeqs $ MMD3.song med
        , tempo = MEDTempo.song2Tempo $ MMD3.song med
        }
    _ -> fail $ printf "unknown format: %08x" $ toInteger ident

instance Human MED where
  human m =
    concatMap human (instrs m) ++
    concatMap human (blocks m) ++
    unlines (map human (playseqs m))


load :: FilePath -> IO MED
load path = bracket (loadMEM path) freeMEM (runStorable peek)
