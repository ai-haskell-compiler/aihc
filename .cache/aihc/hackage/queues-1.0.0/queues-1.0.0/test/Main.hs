module Main (main) where

import Data.Bifunctor (second)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Word (Word8)
import Hedgehog
  ( Gen,
    Group (Group),
    Property,
    PropertyName,
    PropertyT,
    checkParallel,
    forAll,
    property,
    withTests,
    (===), annotateShow,
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main qualified as Hedgehog (defaultMain)
import Hedgehog.Range qualified as Range
import Queue qualified
import Queue.Ephemeral (EphemeralQueue)
import Queue.Ephemeral qualified

main :: IO ()
main = do
  Hedgehog.defaultMain
    [ checkParallel (Group "tests" tests)
    ]

tests :: [(PropertyName, Property)]
tests =
  [ ( "toList . fromList = id",
      (withTests 200 . property) do
        let test :: (Eq a, Show a) => Iface a -> [a] -> PropertyT IO ()
            test Iface {fromList, toList} list =
              toList (fromList list) === list
        list <- forAll generateList
        test realTimeQueueIface list
        test ephemeralQueueIface list
    ),
    ( "fromList (xs ++ ys) = fromList xs <> fromList ys",
      (withTests 200 . property) do
        let test :: (Eq a) => Iface a -> [a] -> [a] -> PropertyT IO ()
            test Iface {fromList} xs ys =
              fromList (xs ++ ys) === fromList xs <> fromList ys
        xs <- forAll generateList
        ys <- forAll generateList
        test realTimeQueueIface xs ys
        test ephemeralQueueIface xs ys
    ),
    ( "toList (xs <> ys) = toList xs <> toList ys",
      (withTests 200 . property) do
        let test :: (Eq a, Show a) => Iface a -> [a] -> [a] -> PropertyT IO ()
            test Iface {fromList, toList} xs ys =
              toList (fromList xs <> fromList ys) === (xs ++ ys)
        xs <- forAll generateList
        ys <- forAll generateList
        test realTimeQueueIface xs ys
        test ephemeralQueueIface xs ys
    ),
    ( "isEmpty empty = True",
      (withTests 1 . property) do
        let test :: Iface () -> PropertyT IO ()
            test Iface {isEmpty, empty} =
              isEmpty empty === True
        test realTimeQueueIface
        test ephemeralQueueIface
    ),
    ( "isEmpty (singleton ()) = False",
      (withTests 1 . property) do
        let test :: Iface () -> PropertyT IO ()
            test Iface {isEmpty, singleton} =
              isEmpty (singleton ()) === False
        test realTimeQueueIface
        test ephemeralQueueIface
    ),
    ( "EphemeralQueue: traverse traverses in order",
      (withTests 1 . property) do
        -- Make a queue that looks like: Q [1,2,3] [6,5,4]
        let queue :: EphemeralQueue Int
            queue =
              Queue.Ephemeral.fromList [1, 2, 3]
                & Queue.Ephemeral.enqueue 4
                & Queue.Ephemeral.enqueue 5
                & Queue.Ephemeral.enqueue 6
        annotateShow queue
        let (elems, _) = Queue.Ephemeral.traverse (\x -> ([x], ())) queue
        elems === [1, 2, 3, 4, 5, 6]
    ),
    ( "state machine tests",
      (withTests 1 . property) do
        actions <- forAll (generateQueueActions 1000)
        let expected = applyQueueActions seqIface actions
        applyQueueActions ephemeralQueueIface actions === expected
        applyQueueActions realTimeQueueIface actions === expected
    )
  ]

------------------------------------------------------------------------------------------------------------------------
-- Queue interface

data Iface a = forall queue.
  (Show (queue a), forall x. (Eq x) => Eq (queue x), forall x. Semigroup (queue x)) =>
  Iface
  { dequeue :: queue a -> Maybe (a, queue a),
    empty :: queue a,
    enqueue :: a -> queue a -> queue a,
    enqueueFront :: a -> queue a -> queue a,
    isEmpty :: queue a -> Bool,
    fromList :: [a] -> queue a,
    singleton :: a -> queue a,
    toList :: queue a -> [a]
  }

realTimeQueueIface :: (Show a) => Iface a
realTimeQueueIface =
  Iface
    Queue.dequeue
    Queue.empty
    Queue.enqueue
    Queue.enqueueFront
    Queue.isEmpty
    Queue.fromList
    Queue.singleton
    Queue.toList

ephemeralQueueIface :: (Show a) => Iface a
ephemeralQueueIface =
  Iface
    Queue.Ephemeral.dequeue
    Queue.Ephemeral.empty
    Queue.Ephemeral.enqueue
    Queue.Ephemeral.enqueueFront
    Queue.Ephemeral.isEmpty
    Queue.Ephemeral.fromList
    Queue.Ephemeral.singleton
    Queue.Ephemeral.toList

seqIface :: (Show a) => Iface a
seqIface =
  Iface
    seqDequeue
    Seq.empty
    seqEnqueue
    seqEnqueueFront
    Seq.null
    Seq.fromList
    Seq.singleton
    Foldable.toList
  where
    seqEnqueue :: a -> Seq a -> Seq a
    seqEnqueue =
      flip (Seq.|>)

    seqDequeue :: Seq a -> Maybe (a, Seq a)
    seqDequeue = \case
      Seq.Empty -> Nothing
      x Seq.:<| xs -> Just (x, xs)

    seqEnqueueFront :: a -> Seq a -> Seq a
    seqEnqueueFront =
      (Seq.<|)

------------------------------------------------------------------------------------------------------------------------
-- Generators

generateList :: Gen [Word8]
generateList =
  Gen.list (Range.linear 0 200) generateWord8

data QueueAction
  = QueueActionEnqueue !Word8
  | QueueActionEnqueueFront !Word8
  | QueueActionDequeue
  deriving stock (Show)

generateQueueActions :: Int -> Gen [QueueAction]
generateQueueActions n =
  Gen.list
    (Range.linear 0 n)
    ( Gen.frequency
        [ (8, QueueActionEnqueue <$> generateWord8),
          (2, QueueActionEnqueueFront <$> generateWord8),
          (2, pure QueueActionDequeue)
        ]
    )

applyQueueActions :: Iface Word8 -> [QueueAction] -> ([Maybe Word8], [Word8])
applyQueueActions Iface {empty, enqueue, dequeue, enqueueFront, toList} =
  second toList . List.foldl' apply ([], empty)
  where
    apply (dequeues, queue) = \case
      QueueActionEnqueue x -> (dequeues, enqueue x queue)
      QueueActionEnqueueFront x -> (dequeues, enqueueFront x queue)
      QueueActionDequeue ->
        case dequeue queue of
          Nothing -> (Nothing : dequeues, queue)
          Just (x, queue1) -> (Just x : dequeues, queue1)

generateWord8 :: Gen Word8
generateWord8 =
  Gen.word8 Range.constantBounded
