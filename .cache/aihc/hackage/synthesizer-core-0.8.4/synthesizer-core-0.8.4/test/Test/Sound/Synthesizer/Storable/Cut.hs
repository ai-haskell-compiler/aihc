module Test.Sound.Synthesizer.Storable.Cut (tests) where

import qualified Synthesizer.Storable.Cut as CutSt
import qualified Synthesizer.Storable.Signal as SigSt

import qualified Synthesizer.Plain.Cut as Cut
import qualified Synthesizer.Plain.Signal as Sig

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.List.HT as ListHT

import qualified Number.NonNegative as NonNeg

import qualified Test.QuickCheck as QC
import Test.QuickCheck (quickCheck, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


genEventList :: QC.Gen (EventList.T NonNeg.Int (Sig.T Int))
genEventList = fmap (EventList.mapTime (flip mod 1000)) QC.arbitrary

arrange :: SigSt.ChunkSize -> QC.Property
arrange chunkSize =
   QC.forAll genEventList $ \evs ->
   let sevs = EventList.mapBody (SigSt.fromList chunkSize) evs
   in  ListHT.allEqual $
       SigSt.fromList chunkSize (Cut.arrange evs) :
       CutSt.arrangeAdaptive chunkSize sevs :
       CutSt.arrangeList chunkSize sevs :
       CutSt.arrangeEquidist chunkSize sevs :
       []


tests :: [(String, IO ())]
tests =
   ("arrange", quickCheck arrange) :
   []
