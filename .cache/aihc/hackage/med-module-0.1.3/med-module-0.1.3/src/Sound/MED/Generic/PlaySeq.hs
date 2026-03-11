module Sound.MED.Generic.PlaySeq where

import qualified Sound.MED.Raw.MMD0Song as MMD0Song
import Sound.MED.Raw.MMD0Song(MMD0Song)

import qualified Sound.MED.Raw.PlaySeq as PlaySeq
import Sound.MED.Raw.PlaySeq(PlaySeq)

import Sound.MED.Basic.Human(Human(human))
import Sound.MED.Basic.Utility(stringFromBytes)

import qualified Data.List as List

data MEDPlaySeq = MEDPlaySeq
  { name :: String
  , indices :: [Int] -- ^ block indices
  }

playSeq0 :: MMD0Song -> MEDPlaySeq
playSeq0 song =
  MEDPlaySeq "" $ take (fromIntegral $ MMD0Song.songlen song) $
  map fromIntegral $ MMD0Song.playseq song

playSeq2 :: PlaySeq -> MEDPlaySeq
playSeq2 pseq =
  MEDPlaySeq
    { name = stringFromBytes (PlaySeq.name pseq)
    , indices = map fromIntegral (PlaySeq.seq pseq)
    }

instance Human MEDPlaySeq where
  human (MEDPlaySeq name' indices') =
    (if null name' then "playseq" else name') ++ ": " ++
    List.intercalate ", " (map show indices')
