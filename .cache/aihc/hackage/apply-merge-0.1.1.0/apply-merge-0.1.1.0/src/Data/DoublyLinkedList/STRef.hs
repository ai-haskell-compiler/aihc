-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.DoublyLinkedList.STRef
  ( -- * Types
    DoublyLinkedList,
    DoublyLinkedNode,

    -- * Construction
    empty,
    fromList,

    -- * Traversal
    head,
    last,
    next,
    prev,

    -- * Query
    null,
    value,

    -- * Insertion
    cons,
    snoc,
    insertBefore,
    insertAfter,

    -- * Deletion
    delete,

    -- * List conversion
    toList,
  )
where

import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Prelude hiding (head, last, null)

data DoublyLinkedList s a = DoublyLinkedList
  { firstNode :: FirstNode s a,
    lastNode :: LastNode s a
  }

-- | The sentinel node at the beginning of the list.
newtype FirstNode s a = FirstNode {nextRef :: STRef s (NextNode s a)}

data DoublyLinkedNode s a = DoublyLinkedNode
  { _value :: a,
    nextRef :: STRef s (NextNode s a),
    prevRef :: STRef s (PrevNode s a)
  }

-- | The sentinel node at the end of the list.
newtype LastNode s a = LastNode
  { prevRef :: STRef s (PrevNode s a)
  }

-- | Type for nodes that are after other nodes.
newtype NextNode s a = NextNode
  { unNextNode :: Either (DoublyLinkedNode s a) (LastNode s a)
  }

nextNodeToDoublyLinkedNode :: NextNode s a -> Maybe (DoublyLinkedNode s a)
nextNodeToDoublyLinkedNode (NextNode (Left v)) = Just v
nextNodeToDoublyLinkedNode _ = Nothing

-- | Type for nodes that are before other nodes.
newtype PrevNode s a = PrevNode
  { unPrevNode :: Either (FirstNode s a) (DoublyLinkedNode s a)
  }

class HasNext node where
  next1 :: node s a -> ST s (NextNode s a)
  next1 = let x = x in x

  setNext1 :: (HasPrev next_node) => node s a -> next_node s a -> ST s ()
  setNext1 = let x = x in x

  toPrev1 :: node s a -> PrevNode s a
  toPrev1 = let x = x in x

  -- Abuse the MINIMAL pragma to not include hidden functions in Haddocks
  {-# MINIMAL #-}

instance HasNext FirstNode where
  next1 (FirstNode nextRef) = readSTRef nextRef
  {-# INLINE next1 #-}
  setNext1 (FirstNode nextRef) nextNode = writeSTRef nextRef (toNext1 nextNode)
  {-# INLINE setNext1 #-}
  toPrev1 = PrevNode . Left
  {-# INLINE toPrev1 #-}

instance HasNext DoublyLinkedNode where
  next1 valueNode = readSTRef valueNode.nextRef
  {-# INLINE next1 #-}
  setNext1 valueNode nextNode = writeSTRef valueNode.nextRef (toNext1 nextNode)
  {-# INLINE setNext1 #-}
  toPrev1 = PrevNode . Right
  {-# INLINE toPrev1 #-}

instance HasNext PrevNode where
  next1 = either next1 next1 . unPrevNode
  {-# INLINE next1 #-}
  setNext1 prevNode nextNode =
    either (`setNext1` nextNode) (`setNext1` nextNode) (unPrevNode prevNode)
  {-# INLINE setNext1 #-}
  toPrev1 = id
  {-# INLINE toPrev1 #-}

class HasPrev node where
  prev1 :: node s a -> ST s (PrevNode s a)
  prev1 = let x = x in x

  setPrev1 :: (HasNext prev_node) => node s a -> prev_node s a -> ST s ()
  setPrev1 = let x = x in x

  toNext1 :: node s a -> NextNode s a
  toNext1 = let x = x in x

  -- Abuse the MINIMAL pragma to not include hidden functions in Haddocks
  {-# MINIMAL #-}

instance HasPrev DoublyLinkedNode where
  prev1 valueNode = readSTRef valueNode.prevRef
  {-# INLINE prev1 #-}
  setPrev1 valueNode prevNode = writeSTRef valueNode.prevRef (toPrev1 prevNode)
  {-# INLINE setPrev1 #-}
  toNext1 = NextNode . Left
  {-# INLINE toNext1 #-}

instance HasPrev LastNode where
  prev1 (LastNode prevRef) = readSTRef prevRef
  {-# INLINE prev1 #-}
  setPrev1 (LastNode prevRef) prevNode = writeSTRef prevRef (toPrev1 prevNode)
  {-# INLINE setPrev1 #-}
  toNext1 = NextNode . Right
  {-# INLINE toNext1 #-}

instance HasPrev NextNode where
  prev1 = either prev1 prev1 . unNextNode
  {-# INLINE prev1 #-}
  setPrev1 nextNode prevNode =
    either (`setPrev1` prevNode) (`setPrev1` prevNode) (unNextNode nextNode)
  {-# INLINE setPrev1 #-}
  toNext1 = id
  {-# INLINE toNext1 #-}

null :: DoublyLinkedList s a -> ST s Bool
null list = do
  nextNode <- readSTRef list.firstNode.nextRef
  case nextNode of
    NextNode (Left _) -> pure False
    _ -> pure True
{-# INLINE null #-}

value :: DoublyLinkedNode s a -> a
value = _value
{-# INLINE value #-}

head :: DoublyLinkedList s a -> ST s (Maybe (DoublyLinkedNode s a))
head list =
  readSTRef list.firstNode.nextRef >>= \case
    NextNode (Left doublyLinkedNode) -> pure (Just doublyLinkedNode)
    _ -> pure Nothing

last :: DoublyLinkedList s a -> ST s (Maybe (DoublyLinkedNode s a))
last list =
  readSTRef list.lastNode.prevRef >>= \case
    PrevNode (Right doublyLinkedNode) -> pure (Just doublyLinkedNode)
    _ -> pure Nothing

next :: DoublyLinkedNode s a -> ST s (Maybe (DoublyLinkedNode s a))
next valueNode = do
  nextNode <- readSTRef valueNode.nextRef
  pure $ case nextNode of
    NextNode (Left valueNode') -> Just valueNode'
    _ -> Nothing

prev :: DoublyLinkedNode s a -> ST s (Maybe (DoublyLinkedNode s a))
prev valueNode = do
  prevNode <- readSTRef valueNode.prevRef
  pure $ case prevNode of
    PrevNode (Right valueNode') -> Just valueNode'
    _ -> Nothing

empty :: ST s (DoublyLinkedList s a)
empty = do
  firstNode <- FirstNode <$> newSTRef undefined
  lastNode <- LastNode <$> newSTRef undefined
  writeSTRef firstNode.nextRef (NextNode (Right lastNode))
  writeSTRef lastNode.prevRef (PrevNode (Left firstNode))
  pure (DoublyLinkedList firstNode lastNode)

insertAfter1 :: (HasNext node) => node s a -> a -> ST s (DoublyLinkedNode s a)
insertAfter1 nodeA x = do
  nodeC <- next1 nodeA
  nodeB <- DoublyLinkedNode x <$> newSTRef nodeC <*> newSTRef (toPrev1 nodeA)
  setNext1 nodeA nodeB
  setPrev1 nodeC nodeB
  pure nodeB

insertBefore1 :: (HasPrev node) => node s a -> a -> ST s (DoublyLinkedNode s a)
insertBefore1 nodeC x = do
  nodeA <- prev1 nodeC
  nodeB <- DoublyLinkedNode x <$> newSTRef (toNext1 nodeC) <*> newSTRef nodeA
  setNext1 nodeA nodeB
  setPrev1 nodeC nodeB
  pure nodeB

insertBefore :: DoublyLinkedNode s a -> a -> ST s (DoublyLinkedNode s a)
insertBefore = insertBefore1

insertAfter :: DoublyLinkedNode s a -> a -> ST s (DoublyLinkedNode s a)
insertAfter = insertAfter1

cons :: DoublyLinkedList s a -> a -> ST s (DoublyLinkedNode s a)
cons list = insertAfter1 list.firstNode
{-# INLINE cons #-}

snoc :: DoublyLinkedList s a -> a -> ST s (DoublyLinkedNode s a)
snoc list = insertBefore1 list.lastNode
{-# INLINE snoc #-}

delete :: DoublyLinkedNode s a -> ST s ()
delete nodeB = do
  nodeA <- prev1 nodeB
  nodeC <- next1 nodeB
  setNext1 nodeA nodeC
  setPrev1 nodeC nodeA

fromList :: [a] -> ST s (DoublyLinkedList s a)
fromList xs = do
  list <- empty
  forM_ xs $ \x ->
    snoc list x
  pure list

toList :: DoublyLinkedList s a -> ST s [a]
toList list = next1 list.firstNode >>= go
  where
    go :: NextNode s a -> ST s [a]
    go =
      nextNodeToDoublyLinkedNode >>> \case
        Nothing -> pure []
        Just valueNode -> (valueNode._value :) <$> (next1 valueNode >>= go)
