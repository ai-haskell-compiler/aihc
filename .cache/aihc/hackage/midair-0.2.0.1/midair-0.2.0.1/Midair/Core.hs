{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module Midair.Core (

   -- * Fundamental units
     SFlow
   -- , (-->)

   , sMap
   , sFold
   , sFoldNoDefault
   , sFoldAccum
   , sCompose
   , sZip
   , sFilter

   -- * Hot-swapping
   , mkNodeRef
   , SFNodeRef
   , nRef
   , hotSwap
   , hotSwapSTM

   -- * Graph firing
   , fireGraph
   , fireGraphIO
   ) where

import Control.Arrow
import qualified Control.Category as Cat
-- import Control.Concurrent.Async (race)
import Control.Concurrent.STM
-- import Data.Proxy
-- import Data.Type.Equality
-- import Data.Unique

-- type (-->) a b = SFlow a b

-- | Signal flow
-- 
--   A little like a function:
--   takes a value of type 'a' and returns one of type 'c'
data SFlow a c where
   -- Transformation:
   SF_Map :: (a -> c) -> SFlow a c
   -- Composition:
   SF_Compose :: forall a b c. SFlow b c -> SFlow a b -> SFlow a c
   -- Accumulation:
   SF_FoldP :: c -> (a -> c -> c) -> SFlow a c
      -- Could we written in terms of other combinators:
   SF_FoldAccum :: Maybe c -> (a -> Maybe c -> c) -> SFlow a c
   -- SF_FoldAccum :: forall a b c d
   -- Recombination:
   SF_Zip :: forall a b c. SFlow a b -> SFlow a c -> SFlow a (b, c)
   -- Filtration/selection:
   SF_Filter :: Maybe a -> (a -> Bool) -> SFlow a (Maybe a)
   -- Reference to a part of the graph:
   SF_NodeRef :: TVar (Maybe c) -> TVar (SFlow a c) -> SFlow a c

data SFNodeRef a c
   = SFNodeRef_Internal (TVar (Maybe c)) (TVar (SFlow a c))

-- | Turn the result of 'mkNodeRef' into something you can use in
--   an 'SFlow' graph
nRef :: SFNodeRef a c -> SFlow a c
nRef (SFNodeRef_Internal x y) = SF_NodeRef x y

mkNodeRefSTM :: SFlow i o -> STM (SFNodeRef i o)
mkNodeRefSTM sigNode = do
   sigNodeTVar <- newTVar sigNode
   foo <- newTVar Nothing
   return $ SFNodeRef_Internal foo sigNodeTVar

-- | Pass in a signal flow graph and get back a reference you can
--   use to hot-swap with
mkNodeRef :: SFlow i o -> IO (SFNodeRef i o)
mkNodeRef g = atomically $ mkNodeRefSTM g

-- | Given a node in the graph, and an input to that node, return the output of
--   that node and the \"new node\" with updated state
fireGraph :: TVar (SFlow a c) -> a -> STM c
fireGraph graphTVar inVal = do
   (newGraph, retVal) <- (flip fireGraph') inVal =<< readTVar graphTVar
   writeTVar graphTVar newGraph
   return retVal
  

-- The only action we do in STM here is to call fireGraph (no prime)
-- whenever we encounter a 'SF_NodeRef':
fireGraph' :: SFlow a c -> a -> STM (SFlow a c, c)
fireGraph' m@(SF_Map f) a =
   return (m, f a)
fireGraph' (SF_Compose bToCOld aToBOld) a = do
   (aToBNew, rightVal) <- fireGraph' aToBOld a
   (bToCNew, returnVal) <- fireGraph' bToCOld rightVal
   return (SF_Compose bToCNew aToBNew, returnVal)
fireGraph' (SF_FoldP theLastVal f) a =
   let newVal = f a theLastVal
   in return (SF_FoldP newVal f, newVal)
fireGraph' (SF_FoldAccum theLastVal f) a =
   let newVal = f a theLastVal
   in return (SF_FoldAccum (Just newVal) f, newVal)
fireGraph' (SF_Zip aToBOld aToCOld) a = do
   (aToBNew, b) <- fireGraph' aToBOld a
   (aToCNew, c) <- fireGraph' aToCOld a
   return (SF_Zip aToBNew aToCNew, (b, c))
fireGraph' (SF_Filter previousVal filterF) newVal = return $
   if filterF newVal
      then (SF_Filter (Just newVal) filterF, Just newVal)
      else (SF_Filter previousVal filterF, previousVal)
fireGraph' (SF_NodeRef prevOutVar graphRef) newIn = do
   newOut <- fireGraph graphRef newIn
   writeTVar prevOutVar $ Just newOut
   return (SF_NodeRef prevOutVar graphRef, newOut)

fireGraphIO :: TVar (SFlow a c) -> a -> IO c
fireGraphIO graphTVar inVal =
   atomically $ fireGraph graphTVar inVal

-- | Apply a function to the input signal
sMap :: (a -> c) -> SFlow a c
sMap = SF_Map

-- | Compose two signal flows into one. The equivalent of '(.)'
sCompose :: SFlow b c -> SFlow a b -> SFlow a c
sCompose = SF_Compose

-- | Accumulate a value. \"Folding over the past\".
sFold :: c -> (a -> c -> c) -> SFlow a c
sFold = SF_FoldP

-- | Like 'sFold' but with no default. Useful for functions like
--   @min@ which don't have a semantics of a result from one
--   argument
sFoldNoDefault :: (a -> Maybe c -> c) -> SFlow a c
sFoldNoDefault f = SF_FoldAccum Nothing f

-- | This name may change
sFoldAccum :: Maybe c -> (a -> Maybe c -> c) -> SFlow a c
sFoldAccum maybeV f = SF_FoldAccum maybeV f

-- | Zip two signal flows together into one which returns a
--   signal of two-tuples
sZip :: SFlow a b -> SFlow a c -> SFlow a (b, c)
sZip = SF_Zip

-- | Filter out the incoming signal by a predicate. Also check out
--   'Midair.Handy.sFilterWDefault'
sFilter :: (b -> Bool) -> SFlow b (Maybe b)
sFilter f = SF_Filter Nothing f

instance Functor (SFlow a) where
   fmap f0 (SF_Map f1) = SF_Map (f0 . f1)
   fmap f0 sf@(SF_FoldP _ _) = SF_Compose (SF_Map f0) sf
   fmap f0 sf@(SF_FoldAccum _ _) = SF_Compose (SF_Map f0) sf
   fmap f0 sf@(SF_Compose _ _) = SF_Compose (SF_Map f0) sf
   fmap f0 sf@(SF_Zip _ _) = SF_Compose (SF_Map f0) sf
   fmap f0 sf@(SF_Filter _ _) = SF_Compose (SF_Map f0) sf
   fmap f0 sf@(SF_NodeRef _ _) = SF_Compose (SF_Map f0) sf

instance Applicative (SFlow a) where
   pure x = SF_Map $ \_ -> x
   (<*>) a b = SF_Compose (SF_Map (\(f, x) -> f x)) (SF_Zip a b)

instance Cat.Category SFlow where
   id = SF_Map $ \x -> x
   (.) a b = SF_Compose a b

instance Arrow SFlow where
   arr = SF_Map
   first :: SFlow b c -> SFlow (b, d) (c, d)
   first bToC =
      sZip (bToC `sCompose` arr fst) (arr snd)

{-
-- | Rationale: why do we do this instead of recomputing from the beginning?
-- 
--   1) Practicality: it would be expensive to save all input values
--   2) Meaning: if you e.g. have a video game that's depermining a player's
--      position based on their keypresses, if you were to e.g. recompute all steps
--      (let's say the keypresses move 3 pixels now instead of 2), your player
--      would end up in a totally new position
-}


hotSwapSTM :: SFNodeRef a c -> (Maybe c -> SFlow a c) -> STM ()
hotSwapSTM (SFNodeRef_Internal oldValMaybeTVar graphTVar) newGraphFromVal = do
   oldValMaybe <- readTVar oldValMaybeTVar
   writeTVar graphTVar $ newGraphFromVal oldValMaybe

-- | Swap out part or whole of a signal flow graph with a new one
--   of the same type.
hotSwap :: SFNodeRef a c -> (Maybe c -> SFlow a c) -> IO ()
hotSwap a b = atomically $ hotSwapSTM a b
