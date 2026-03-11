module PrimitiveExtras.SmallArray where

import qualified Focus
import GHC.Exts hiding (toList)
import qualified ListT
import PrimitiveExtras.Prelude

-- | A workaround for the weird forcing of 'undefined' values int 'newSmallArray'
{-# INLINE newEmptySmallArray #-}
newEmptySmallArray :: (PrimMonad m) => Int -> m (SmallMutableArray (PrimState m) a)
newEmptySmallArray size = newSmallArray size (unsafeCoerce 0)

{-# INLINE list #-}
list :: [a] -> SmallArray a
list list =
  let !size = length list
   in runSmallArray $ do
        m <- newEmptySmallArray size
        let populate index list = case list of
              element : list -> do
                writeSmallArray m index element
                populate (succ index) list
              [] -> return m
         in populate 0 list

-- |
-- Remove an element.
{-# INLINE unset #-}
unset :: Int -> SmallArray a -> SmallArray a
unset index array =
  {-# SCC "unset" #-}
  let !size = sizeofSmallArray array
      !newSize = pred size
      !newIndex = succ index
      !amountOfFollowingElements = size - newIndex
   in runSmallArray $ do
        newMa <- newSmallArray newSize undefined
        copySmallArray newMa 0 array 0 index
        copySmallArray newMa index array newIndex amountOfFollowingElements
        return newMa

{-# INLINE set #-}
set :: Int -> a -> SmallArray a -> SmallArray a
set index a array =
  {-# SCC "set" #-}
  let size = sizeofSmallArray array
   in runST $ do
        m <- thawSmallArray array 0 size
        writeSmallArray m index a
        unsafeFreezeSmallArray m

{-# INLINE insert #-}
insert :: Int -> a -> SmallArray a -> SmallArray a
insert index a array =
  {-# SCC "insert" #-}
  let !size = sizeofSmallArray array
      !newSize = succ size
      !nextIndex = succ index
      !amountOfFollowingElements = size - index
   in runSmallArray $ do
        newMa <- newSmallArray newSize a
        copySmallArray newMa 0 array 0 index
        copySmallArray newMa nextIndex array index amountOfFollowingElements
        return newMa

{-# INLINE adjust #-}
adjust :: (a -> a) -> Int -> SmallArray a -> SmallArray a
adjust fn index array =
  let size = sizeofSmallArray array
   in if size > index && index >= 0
        then unsafeAdjustWithSize fn index size array
        else array

{-# INLINE unsafeAdjust #-}
unsafeAdjust :: (a -> a) -> Int -> SmallArray a -> SmallArray a
unsafeAdjust fn index array =
  unsafeAdjustWithSize fn index (sizeofSmallArray array) array

{-# INLINE unsafeAdjustWithSize #-}
unsafeAdjustWithSize :: (a -> a) -> Int -> Int -> SmallArray a -> SmallArray a
unsafeAdjustWithSize fn index size array =
  runST $ do
    m <- thawSmallArray array 0 size
    element <- readSmallArray m index
    writeSmallArray m index $! fn element
    unsafeFreezeSmallArray m

{-# INLINE cons #-}
cons :: a -> SmallArray a -> SmallArray a
cons a array =
  {-# SCC "cons" #-}
  let size = sizeofSmallArray array
      newSize = succ size
   in runSmallArray $ do
        newMa <- newSmallArray newSize a
        copySmallArray newMa 1 array 0 size
        return newMa

{-# INLINE orderedPair #-}
orderedPair :: Int -> e -> Int -> e -> SmallArray e
orderedPair i1 e1 i2 e2 =
  {-# SCC "orderedPair" #-}
  runSmallArray
    $ if
      | i1 < i2 -> do
          a <- newSmallArray 2 e1
          writeSmallArray a 1 e2
          return a
      | i1 > i2 -> do
          a <- newSmallArray 2 e1
          writeSmallArray a 0 e2
          return a
      | otherwise -> do
          a <- newSmallArray 1 e2
          return a

{-# INLINE findAndReplace #-}
findAndReplace :: (a -> Maybe a) -> SmallArray a -> SmallArray a
findAndReplace f array =
  let size = sizeofSmallArray array
      iterate index =
        if index < size
          then case f (indexSmallArray array index) of
            Just newElement -> set index newElement array
            Nothing -> iterate (succ index)
          else array
   in iterate 0

{-# INLINE findAndMap #-}
findAndMap :: (a -> Maybe b) -> SmallArray a -> Maybe b
findAndMap f array =
  let size = sizeofSmallArray array
      iterate index =
        if index < size
          then case f (indexSmallArray array index) of
            Just b -> Just b
            Nothing -> iterate (succ index)
          else Nothing
   in iterate 0

{-# INLINE find #-}
find :: (a -> Bool) -> SmallArray a -> Maybe a
find test array =
  {-# SCC "find" #-}
  let !size = sizeofSmallArray array
      iterate !index =
        if index < size
          then
            let !element = indexSmallArray array index
             in if test element
                  then Just element
                  else iterate (succ index)
          else Nothing
   in iterate 0

{-# INLINE findWithIndex #-}
findWithIndex :: (a -> Bool) -> SmallArray a -> Maybe (Int, a)
findWithIndex test array =
  {-# SCC "findWithIndex" #-}
  let !size = sizeofSmallArray array
      iterate !index =
        if index < size
          then
            let !element = indexSmallArray array index
             in if test element
                  then Just (index, element)
                  else iterate (succ index)
          else Nothing
   in iterate 0

{-# INLINE elementsUnfoldlM #-}
elementsUnfoldlM :: (Monad m) => SmallArray e -> UnfoldlM m e
elementsUnfoldlM array = UnfoldlM $ \step initialState ->
  let !size = sizeofSmallArray array
      iterate index !state =
        if index < size
          then do
            element <- indexSmallArrayM array index
            newState <- step state element
            iterate (succ index) newState
          else return state
   in iterate 0 initialState

{-# INLINE elementsListT #-}
elementsListT :: (Monad m) => SmallArray a -> ListT m a
elementsListT = ListT.fromFoldable

{-# INLINE onFoundElementFocus #-}
onFoundElementFocus :: (Monad m) => (a -> Bool) -> (a -> Bool) -> Focus a m b -> Focus (SmallArray a) m b
onFoundElementFocus testAsKey testWholeEntry (Focus concealA revealA) = Focus concealArray revealArray
  where
    concealArray = fmap (fmap arrayChange) concealA
      where
        arrayChange = \case
          Focus.Set newEntry -> Focus.Set (pure newEntry)
          _ -> Focus.Leave
    revealArray array = case findWithIndex testAsKey array of
      Just (index, entry) -> fmap (fmap arrayChange) (revealA entry)
        where
          arrayChange = \case
            Focus.Leave -> Focus.Leave
            Focus.Set newEntry ->
              if testWholeEntry newEntry
                then Focus.Leave
                else Focus.Set (set index newEntry array)
            Focus.Remove ->
              if sizeofSmallArray array > 1
                then Focus.Set (unset index array)
                else Focus.Remove
      Nothing -> fmap (fmap arrayChange) concealA
        where
          arrayChange = \case
            Focus.Set newEntry -> Focus.Set (cons newEntry array)
            _ -> Focus.Leave

{-# INLINE focusOnFoundElement #-}
focusOnFoundElement :: (Monad m) => Focus a m b -> (a -> Bool) -> (a -> Bool) -> SmallArray a -> m (b, SmallArray a)
focusOnFoundElement focus testAsKey testWholeEntry = case onFoundElementFocus testAsKey testWholeEntry focus of
  Focus conceal reveal -> \sa -> do
    (b, change) <- reveal sa
    return $ (b,) $ case change of
      Focus.Leave -> sa
      Focus.Set newSa -> newSa
      Focus.Remove -> empty

toList :: forall a. SmallArray a -> [a]
toList array = PrimitiveExtras.Prelude.toList (elementsUnfoldlM array :: UnfoldlM Identity a)
