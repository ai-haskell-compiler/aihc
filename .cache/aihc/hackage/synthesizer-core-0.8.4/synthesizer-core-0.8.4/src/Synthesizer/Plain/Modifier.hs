{- |
Support for stateful modifiers like controlled filters.
This is similar to "Synthesizer.Causal.Process"
but we cannot replace the Modifier structure by the Causal structure
because the Modifier structure exhibits the state
which allows stacking of modifiers
using an efficient storage for the stacked state.
More precisely, because Modifiers exhibits the type of the state,
we can ensure that the state type of several modifiers is equal
and thus the individual states can be stored in an array or a StorableVector.
-}
module Synthesizer.Plain.Modifier where

import Control.Monad.Trans.State (State, state, runState, evalState, )
import Control.Monad (zipWithM, )

import qualified Data.StorableVector as SV
import Foreign.Storable (Storable(..))

import qualified Data.List as List

import Prelude hiding (init)


-- Signal.T, re-defined here in order to avoid module cycle
type T a = [a]


data Simple s ctrl a b =
   Simple {
      init :: s,
      step :: ctrl -> a -> State s b
   }

{-|
modif is a process controlled by values of type c
with an internal state of type s,
it converts an input value of type a into an output value of type b
while turning into a new state

ToDo:
Shall finite signals be padded with zeros?
-}
static ::
   Simple s ctrl a b -> ctrl -> T a -> T b
static modif control x =
   evalState (mapM (step modif control) x) (init modif)

{-| Here the control may vary over the time. -}
modulated ::
   Simple s ctrl a b -> T ctrl -> T a -> T b
modulated modif control x =
   evalState (zipWithM (step modif) control x) (init modif)


data Initialized s init ctrl a b =
   Initialized {
      initInit :: init -> s,
      initStep :: ctrl -> a -> State s b
   }


initialize ::
   Initialized s init ctrl a b -> init -> Simple s ctrl a b
initialize modif stateInit =
   Simple (initInit modif stateInit) (initStep modif)

staticInit ::
   Initialized s init ctrl a b -> init -> ctrl -> T a -> T b
staticInit modif state_ =
   static (initialize modif state_)

{-| Here the control may vary over the time. -}
modulatedInit ::
   Initialized s init ctrl a b -> init -> T ctrl -> T a -> T b
modulatedInit modif state_ =
   modulated (initialize modif state_)



{- |
The number of stacked state monads
depends on the size of the list of state values.
This is like a dynamically nested StateT.
-}
stackStatesR :: (a -> State s a) -> (a -> State [s] a)
stackStatesR m =
   state . List.mapAccumR (runState . m)

stackStatesL :: (a -> State s a) -> (a -> State [s] a)
stackStatesL m =
   state . List.mapAccumL (runState . m)


{-# INLINE stackStatesStorableR #-}
stackStatesStorableR :: (Storable s) =>
   (a -> State s a) -> (a -> State (SV.Vector s) a)
stackStatesStorableR m =
   state . SV.mapAccumR (runState . m)

{-# INLINE stackStatesStorableL #-}
stackStatesStorableL :: (Storable s) =>
   (a -> State s a) -> (a -> State (SV.Vector s) a)
stackStatesStorableL m =
   state . SV.mapAccumL (runState . m)


{-
{-# INLINE stackStatesStorableVaryR #-}
stackStatesStorableVaryR :: (Storable s, Storable c) =>
   (c -> a -> State s a) -> (SV.Vector c -> a -> State (SV.Vector s) a)
stackStatesStorableVaryR m cv a =
   State . SV.mapAccumL (runState . m)
-}

{-# INLINE stackStatesStorableVaryL #-}
stackStatesStorableVaryL :: (Storable s, Storable c) =>
   (c -> a -> State s a) -> (SV.Vector c -> a -> State (SV.Vector s) a)
stackStatesStorableVaryL m cv a = state $ \sv ->
   -- emulate SV.zipWith with minimal use of Storable functionality
   let (svFinal, mcsa) =
          SV.unfoldrN (SV.length sv)
             (\(cv0,sv0,a0) ->
                  do (c,cv1) <- SV.viewL cv0
                     (s,sv1) <- SV.viewL sv0
                     let (a1,sNew) = runState (m c a0) s
                     return (sNew,(cv1,sv1,a1)))
             (cv,sv,a)
   in  (case mcsa of
           Just (_, _, aFinal) -> aFinal
           _ -> error $ "Modifier: control vector too short - "
                   ++ "status size " ++ show (SV.length sv) ++ " vs. "
                   ++ "control size " ++ show (SV.length cv),
        svFinal)
