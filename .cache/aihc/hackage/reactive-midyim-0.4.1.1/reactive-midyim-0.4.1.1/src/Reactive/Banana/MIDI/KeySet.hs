module Reactive.Banana.MIDI.KeySet where

import qualified Reactive.Banana.MIDI.Note as Note

import qualified Data.Traversable as Trav

import qualified Data.Accessor.Monad.Trans.State as AccState
import qualified Data.Accessor.Basic as Acc

import qualified Control.Monad.Trans.State as MS

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Maybe.HT (toMaybe, )
import Data.Maybe (maybeToList, listToMaybe, )


{-
class C set where
   press :: Channel -> (Velocity, Pitch) -> set -> set
   release :: Channel -> (Velocity, Pitch) -> set -> set
   reset :: set -> set

change :: C set => Channel -> (Velocity, Pitch, Bool) -> set -> set
change chan (vel, pitch, True)  = press   chan (vel, pitch)
change chan (vel, pitch, False) = release chan (vel, pitch)
-}

class C set where
   reset :: MS.State (set key value) [Note.Boundary key value]
   {- |
   It must hold @reset == resetSome (const True)@.
   -}
   resetSome :: Ord key => (key -> Bool) -> MS.State (set key value) [Note.Boundary key value]
   size :: set key value -> Int
   toList :: set key value -> [(key, value)]
   index :: Ord key => Int -> set key value -> Maybe (key, value)
   change :: Ord key => Note.Boundary key value -> MS.State (set key value) [Note.Boundary key value]

changeExt ::
   (Ord key, C set) =>
   Note.BoundaryExt key value ->
   MS.State (set key value) [Note.Boundary key value]
changeExt e =
   case e of
      Note.BoundaryExt bnd -> change bnd
      Note.AllOff p -> resetSome p

class Map set where
   accessMap :: Acc.T (set key value) (Map.Map key value)


newtype Pressed key value = Pressed {deconsPressed :: Map.Map key value}
   deriving (Show)

pressed :: Pressed key value
pressed = Pressed Map.empty

instance Map Pressed where
   accessMap = Acc.fromWrapper Pressed deconsPressed

instance C Pressed where
   reset = releasePlayedKeys
   resetSome = releaseSomeKeys
   size = sizeGen
   toList = toListGen
   index = indexGen
   change bnd@(Note.Boundary key vel on) = do
      AccState.modify accessMap $
         if on
           then Map.insert key vel
           else Map.delete key
      return [bnd]



newtype Latch key value = Latch {deconsLatch :: Map.Map key value}
   deriving (Show)

latch :: Latch key value
latch = Latch Map.empty

instance Map Latch where
   accessMap = Acc.fromWrapper Latch deconsLatch

latchChange ::
   Ord key =>
   Note.Boundary key value ->
   MS.State (Latch key value) (Maybe (Note.Boundary key value))
latchChange (Note.Boundary key vel on) =
   Trav.sequence $ toMaybe on $ do
      isPressed <- MS.gets (Map.member key . deconsLatch)
      if isPressed
        then
           AccState.modify accessMap (Map.delete key) >>
           return (Note.Boundary key vel False)
        else
           AccState.modify accessMap (Map.insert key vel) >>
           return (Note.Boundary key vel True)

instance C Latch where
   reset = releasePlayedKeys
   resetSome = releaseSomeKeys
   size = sizeGen
   toList = toListGen
   index = indexGen
   change = fmap maybeToList . latchChange



data GroupLatch key value =
   GroupLatch {
      groupLatchPressed_ {- input -} :: Set.Set key,
      groupLatchPlayed_ {- output -} :: Map.Map key value
   } deriving (Show)

groupLatch :: GroupLatch key value
groupLatch = GroupLatch Set.empty Map.empty

groupLatchPressed :: Acc.T (GroupLatch key value) (Set.Set key)
groupLatchPressed =
   Acc.fromSetGet
      (\mp grp -> grp{groupLatchPressed_ = mp})
      groupLatchPressed_

groupLatchPlayed :: Acc.T (GroupLatch key value) (Map.Map key value)
groupLatchPlayed =
   Acc.fromSetGet
      (\mp grp -> grp{groupLatchPlayed_ = mp})
      groupLatchPlayed_

instance Map GroupLatch where
   accessMap = groupLatchPlayed

{- |
All pressed keys are latched until a key is pressed after a pause
(i.e. all keys released).
For aborting the pattern you have to send
a 'ModeMsg.AllNotesOff' or 'ModeMsg.AllSoundOff' message.
-}
instance C GroupLatch where
   reset = releasePlayedKeys
   resetSome = releaseSomeKeys
   size = sizeGen
   toList = toListGen
   index = indexGen
   change (Note.Boundary key vel on) =
      if on
        then do
           pressd <- AccState.get groupLatchPressed
           noteOffs <-
              if Set.null pressd
                then releasePlayedKeys
                else return []
           AccState.modify groupLatchPressed (Set.insert key)
           played <- AccState.get groupLatchPlayed
           noteOn <-
              if Map.member key played
                then
                   return []
                else do
                   AccState.modify groupLatchPlayed (Map.insert key vel)
                   return [Note.Boundary key vel True]
           return $
              noteOffs ++ noteOn
        else
           AccState.modify groupLatchPressed (Set.delete key) >>
           return []



data SerialLatch key value =
   SerialLatch {
      serialLatchSize_ :: Int,
      serialLatchCursor_ :: Int,
      serialLatchPlayed_ :: Map.Map Int (key, value)
   } deriving (Show)

serialLatch :: Int -> SerialLatch key value
serialLatch num = SerialLatch num 0 Map.empty

serialLatchCursor :: Acc.T (SerialLatch key value) Int
serialLatchCursor =
   Acc.fromSetGet
      (\mp grp -> grp{serialLatchCursor_ = mp})
      serialLatchCursor_

serialLatchPlayed :: Acc.T (SerialLatch key value) (Map.Map Int (key, value))
serialLatchPlayed =
   Acc.fromSetGet
      (\mp grp -> grp{serialLatchPlayed_ = mp})
      serialLatchPlayed_



{- |
A key is hold until @n@ times further keys are pressed.
The @n@-th pressed key replaces the current one.
-}
instance C SerialLatch where
--   reset = AccState.lift serialLatchPlayed releasePlayedKeys
--      (0, Map.empty)
   reset =
      fmap (map (uncurry releaseKey) . Map.elems) $
      AccState.getAndModify serialLatchPlayed (const Map.empty)
   resetSome p =
      fmap (map (uncurry releaseKey) . Map.elems) $
      AccState.lift serialLatchPlayed $
      MS.state $ Map.partition (p . fst)
   size = serialLatchSize_
   toList = Map.elems . serialLatchPlayed_
   index k = Map.lookup k . serialLatchPlayed_
   change bnd@(Note.Boundary key vel on) =
      if on
        then do
           n <- MS.gets serialLatchSize_
           k <- AccState.getAndModify serialLatchCursor (flip mod n . (1+))
           oldKey <- fmap (Map.lookup k) $ AccState.get serialLatchPlayed
           AccState.modify serialLatchPlayed (Map.insert k (key, vel))
           return $ maybeToList (fmap (uncurry releaseKey) oldKey)
                     ++ [bnd]
        else return []

sizeGen :: (Map set) => set key value -> Int
sizeGen = Map.size . Acc.get accessMap

toListGen :: (Map set) => set key value -> [(key, value)]
toListGen = Map.toAscList . Acc.get accessMap

indexGen ::
   (Ord key, Map set) =>
   Int -> set key value -> Maybe (key, value)
indexGen k =
   listToMaybe . drop k . Map.toAscList . Acc.get accessMap

releasePlayedKeys ::
   (Map set) =>
   MS.State
      (set key value)
      [Note.Boundary key value]
releasePlayedKeys =
   fmap (map (uncurry releaseKey) . Map.toList) $
   AccState.getAndModify accessMap $ const Map.empty

releaseSomeKeys ::
   (Ord key, Map set) =>
   (key -> Bool) ->
   MS.State
      (set key value)
      [Note.Boundary key value]
releaseSomeKeys p =
   fmap (map (uncurry releaseKey) . Map.toList) $
   AccState.lift accessMap $ MS.state $
   Map.partitionWithKey (const . p)

releaseKey ::
   key -> value -> Note.Boundary key value
releaseKey key vel =
   Note.Boundary key vel False
