module Reactive.Banana.MIDI.Pattern where

import qualified Reactive.Banana.MIDI.Note as Note
import qualified Reactive.Banana.MIDI.KeySet as KeySet
import qualified Reactive.Banana.MIDI.DeBruijn as DeBruijn
import qualified Reactive.Banana.MIDI.Pitch as Pitch

import Reactive.Banana.MIDI.Common (splitFraction, )

import qualified Reactive.Banana.MIDI.Utility as RBU
import qualified Reactive.Banana.Bunch.Combinators as RB
import Reactive.Banana.Bunch.Combinators ((<@>), )

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import Sound.MIDI.Message.Channel.Voice (Velocity, )

import qualified Data.EventList.Absolute.TimeBody as AbsEventList
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import Data.EventList.Relative.MixedBody ((/.), (./), )
import qualified Numeric.NonNegative.Wrapper as NonNegW

import qualified Data.List.HT as ListHT
import qualified Data.List as List

import qualified System.Random as Rnd

import qualified Control.Monad.Trans.State as MS

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import Control.Monad (guard, liftM, )
import Control.Applicative (pure, (<*>), )
import Data.Maybe (mapMaybe, maybeToList, )
import Data.Bool.HT (if', )
import Data.Ord.HT (comparing, )

import Prelude hiding (init, filter, reverse, )



-- * reactive patterns

type T m time set key value =
   RB.Behavior (set key value) ->
   RB.Event time ->
   m (RB.Event [Note.Boundary key value])

mono ::
   (RB.MonadMoment m) =>
   Selector set key Velocity i ->
   RB.Behavior (set key Velocity) ->
   RB.Event i ->
   m (RB.Event [Note.Boundary key Velocity])
mono select pressed pattern =
   liftM fst $ RBU.sequence [] $
   pure
      (\set i -> do
         off <- MS.get
         let mnote = select i set
             on =
                fmap
                   (\(key, vel) -> Note.Boundary key vel True)
                   mnote
         MS.put $ fmap
            (\(key, _vel) -> Note.Boundary key VoiceMsg.normalVelocity False)
            mnote
         return $ off ++ on)
      <*> pressed
      <@> pattern


poly ::
   (RB.MonadMoment m) =>
   Selector set key Velocity i ->
   RB.Behavior (set key Velocity) ->
   RB.Event [IndexNote i] ->
   m (RB.Event [Note.Boundary key Velocity])
poly select pressed pattern =
   liftM fst $ RBU.sequence EventList.empty $
   pure
      (\set is -> do
         off <- MS.get
         let (nowOff, laterOff) = EventListTM.splitAtTime 1 off
             sel = concatMap (Trav.traverse (flip select set)) is
             on =
                fmap
                   (\(IndexNote _ (key, vel)) ->
                      Note.Boundary key vel True)
                   sel
         MS.put $
            EventList.mergeBy (\ _ _ -> False) laterOff $
            EventList.fromAbsoluteEventList $
            AbsEventList.fromPairList $
            List.sortBy (comparing fst) $
            map
               (\(IndexNote dur (key, _vel)) ->
                  (dur, Note.Boundary key VoiceMsg.normalVelocity False))
            sel
         return $ Fold.toList nowOff ++ on)
      <*> pressed
      <@> pattern



-- * selectors

type Selector set key value i =
        i -> set key value -> [(key, value)]


data IndexNote i = IndexNote NonNegW.Int i
   deriving (Show, Eq, Ord)

instance Functor IndexNote where
   fmap f (IndexNote d i) = IndexNote d $ f i

instance Fold.Foldable IndexNote where
   foldMap = Trav.foldMapDefault

instance Trav.Traversable IndexNote where
   sequenceA (IndexNote d i) = fmap (IndexNote d) i


item :: i -> Int -> IndexNote i
item i n = IndexNote (NonNegW.fromNumberMsg "Pattern.item" n) i

data
   Poly set key value i =
      Poly (Selector set key value i) (EventList.T Int [IndexNote i])


{- |
Generate notes according to the key set,
where notes for negative and too large indices
are padded with keys that are transposed by octaves.
-}
selectFromOctaveChord ::
   (KeySet.C set, Ord pitch, Pitch.C pitch) =>
   Selector set pitch value Int
selectFromOctaveChord d chord =
   maybeToList $ do
      let size = KeySet.size chord
      guard (size>0)
      let (q,r) = divMod d size
      (pc, vel) <- KeySet.index r chord
      pcTrans <- Pitch.increase (12*q) pc
      return (pcTrans, vel)

selectFromChord ::
   (KeySet.C set, Ord key) =>
   Selector set key value Int
selectFromChord n chord =
   maybeToList $ KeySet.index n chord

selectFromChordRatio ::
   (KeySet.C set, Ord key) =>
   Selector set key value Double
selectFromChordRatio d chord =
   selectFromChord (floor $ d * fromIntegral (KeySet.size chord)) chord


selectInversion ::
   (KeySet.C set, Pitch.C pitch) =>
   Selector set pitch value Double
selectInversion d chord =
   let makeNote octave (pc, vel) =
          fmap
             (\pcTrans -> (pcTrans, vel))
             (Pitch.increase (octave*12) pc)
       (oct,p) = splitFraction d
       pivot = floor (p * fromIntegral (KeySet.size chord))
       (low,high) = splitAt pivot $ KeySet.toList chord
   in  mapMaybe (makeNote oct) high ++
       mapMaybe (makeNote (oct+1)) low



-- * patterns

{- |
See Haskore/FlipSong

  flipSeq m !! n = cross sum of the m-ary representation of n modulo m.

  For m=2 this yields
  http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=A010060
-}
flipSeq :: Int -> [Int]
flipSeq n =
   let incList m = map (\x -> mod (x+m) n)
       recourse y =
          let z = concatMap (flip incList y) [1 .. n-1]
          in  z ++ recourse (y++z)
   in  [0] ++ recourse [0]


cycleUpIndex, cycleDownIndex, pingPongIndex ::
   (RB.MonadMoment m) =>
   RB.Behavior Int ->
   RB.Event time ->
   m (RB.Event Int)
cycleUpIndex numbers times =
   liftM fst $ RB.mapAccum 0 $
   pure
      (\number _time i -> (i, mod (succ i) (max 1 number)))
      <*> numbers
      <@> times

cycleDownIndex numbers times =
   RB.accumE 0 $
   pure
      (\number _time i -> mod (pred i) (max 1 number))
      <*> numbers
      <@> times

pingPongIndex numbers times =
   liftM fst $ RB.mapAccum (0,1) $
   pure
      (\number _time (i,d0) ->
         (i, let j = i+d0
                 d1 =
                    if' (j>=number) (-1) $
                    if' (j<0) 1 d0
             in  (i+d1, d1)))
      <*> numbers
      <@> times

crossSumIndex ::
   (RB.MonadMoment m) =>
   RB.Behavior Int ->
   RB.Event time ->
   m (RB.Event Int)
crossSumIndex numbers times =
   flip liftM (fromList [0..] times) $ \ts ->
   pure
      (\number i ->
         let m = fromIntegral number
         in  if m <= 1
               then 0
               else fromInteger $ flip mod m $ sum $ decomposePositional m i)
      <*> numbers
      <@> ts


crossSumStaticIndex ::
   (RB.MonadMoment m) =>
   Int ->
   RB.Event time ->
   m (RB.Event Int)
crossSumStaticIndex number =
   fromList (flipSeq number)

fromList ::
   (RB.MonadMoment m) =>
   [a] -> RB.Event time -> m (RB.Event a)
fromList xs times =
   liftM (RB.filterJust . fst) $ RB.mapAccum xs $
   fmap
      (\_time xs0 ->
         case xs0 of
            [] -> (Nothing, [])
            x:xs1 -> (Just x, xs1))
      times


cycleUp, cycleDown, pingPong, crossSum ::
   (RB.MonadMoment m, KeySet.C set, Ord key) =>
   RB.Behavior Int -> T m time set key Velocity
cycleUp   numbers sets times =
   mono selectFromChord sets =<< cycleUpIndex numbers times
cycleDown numbers sets times =
   mono selectFromChord sets =<< cycleDownIndex numbers times
pingPong  numbers sets times =
   mono selectFromChord sets =<< pingPongIndex numbers times
crossSum  numbers sets times =
   mono selectFromChord sets =<< crossSumIndex numbers times

bruijn ::
   (RB.MonadMoment m, KeySet.C set, Ord key) =>
   Int -> Int -> T m time set key Velocity
bruijn n k sets times =
   mono selectFromChord sets =<<
   fromList (cycle $ DeBruijn.lexLeast n k) times


binaryStaccato, binaryLegato, binaryAccident ::
   (RB.MonadMoment m, KeySet.C set, Ord key) => T m time set key Velocity
{-
binary number Pattern.T:
   0
   1
   0 1
   2
   0 2
   1 2
   0 1 2
   3
-}
binaryStaccato sets times =
   poly selectFromChord sets =<<
      (flip fromList times $
       map
          (map (IndexNote 1 . fst) .
           List.filter ((/=0) . snd) .
           zip [0..] .
           decomposePositional 2)
          [0..])

binaryLegato sets times =
   poly selectFromChord sets =<<
      (flip fromList times $
       map
          (\m ->
             map (uncurry IndexNote) $
             List.filter (\(p,_i) -> mod m p == 0) $
             takeWhile ((<=m) . fst) $
             zip (iterate (2*) 1) [0..])
          [0..])

{-
This was my first try to implement binaryLegato.
It was not what I wanted, but it sounded nice.
-}
binaryAccident sets times =
   poly selectFromChord sets =<<
      (flip fromList times $
       map
          (zipWith IndexNote (iterate (2*) 1) .
           map fst .
           List.filter ((/=0) . snd) .
           zip [0..] .
           decomposePositional 2)
          [0..])


-- cf. htam:NumberTheory
decomposePositional :: Integer -> Integer -> [Integer]
decomposePositional b =
   let recourse 0 = []
       recourse x =
          let (q,r) = divMod x b
          in  r : recourse q
   in  recourse

cycleUpOctave ::
   (RB.MonadMoment m, KeySet.C set, Ord pitch, Pitch.C pitch) =>
   RB.Behavior Int -> T m time set pitch Velocity
cycleUpOctave numbers sets times =
   mono selectFromOctaveChord sets =<< cycleUpIndex numbers times


random ::
   (RB.MonadMoment m, KeySet.C set, Ord key) =>
   T m time set key Velocity
random sets times =
   (mono selectFromChordRatio sets =<<) $
   liftM fst $ RB.mapAccum (Rnd.mkStdGen 42) $
   fmap (const $ Rnd.randomR (0,1)) times

randomInversions ::
   (RB.MonadMoment m, KeySet.C set, Pitch.C pitch) =>
   T m time set pitch Velocity
randomInversions =
   inversions $
   map sum $
   ListHT.sliceVertical 3 $
   Rnd.randomRs (-1,1) $
   Rnd.mkStdGen 42

cycleUpInversions ::
   (RB.MonadMoment m, KeySet.C set, Pitch.C pitch) =>
   Int -> T m time set pitch Velocity
cycleUpInversions n =
   inversions $ cycle $ take n $
   map (\i -> fromInteger i / fromIntegral n) [0..]

inversions ::
   (RB.MonadMoment m, KeySet.C set, Pitch.C pitch) =>
   [Double] -> T m time set pitch Velocity
inversions rs sets times =
   mono selectInversion sets =<< fromList rs times



-- * tests

{-
We cannot use cycle function here, because we need to cycle a Body-Time list
which is incompatible to a Body-Body list,
even if the end is never reached.
-}
examplePolyTempo0 ::
   EventList.T Int [IndexNote Int]
examplePolyTempo0 =
   let pat =
          [item 0 1] ./ 1 /. [item 1 1, item 2 1] ./ 2 /.
          [item 1 1, item 2 1] ./ 1 /. [item 0 1] ./ 2 /.
          pat
   in  0 /. pat

examplePolyTempo1 ::
   EventList.T Int [IndexNote Int]
examplePolyTempo1 =
   let pat =
          [item 0 1] ./ 1 /.
          [item 2 1, item 3 1, item 4 1] ./ 1 /.
          [item 2 1, item 3 1, item 4 1] ./ 1 /.
          [item 1 1] ./ 1 /.
          [item 2 1, item 3 1, item 4 1] ./ 1 /.
          [item 2 1, item 3 1, item 4 1] ./ 1 /.
          pat
   in  0 /. pat
