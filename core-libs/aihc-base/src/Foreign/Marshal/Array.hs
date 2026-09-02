module Foreign.Marshal.Array
  ( mallocArray,
    mallocArray0,
    allocaArray,
    allocaArray0,
    callocArray,
    callocArray0,
    reallocArray,
    reallocArray0,
    peekArray,
    peekArray0,
    pokeArray,
    pokeArray0,
    newArray,
    newArray0,
    withArray,
    withArray0,
    withArrayLen,
    withArrayLen0,
    copyArray,
    moveArray,
    lengthArray0,
    advancePtr,
  )
where

import Foreign.Marshal.Alloc (allocaBytes, callocBytes, mallocBytes, reallocBytes)
import Foreign.Marshal.Utils (copyBytes, moveBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable (..))
import Prelude (Eq (..), IO, Int, Num (..), Ord (..), length, mapM_, return, uncurry, undefined, zip, (.), (>>))

mallocArray :: (Storable a) => Int -> IO (Ptr a)
mallocArray = mallocArrayOf undefined

mallocArrayOf :: (Storable a) => a -> Int -> IO (Ptr a)
mallocArrayOf placeholder count = mallocBytes (count * sizeOf placeholder)

mallocArray0 :: (Storable a) => Int -> IO (Ptr a)
mallocArray0 count = mallocArray (count + 1)

allocaArray :: (Storable a) => Int -> (Ptr a -> IO b) -> IO b
allocaArray = allocaArrayOf undefined

allocaArrayOf :: (Storable a) => a -> Int -> (Ptr a -> IO b) -> IO b
allocaArrayOf placeholder count = allocaBytes (count * sizeOf placeholder)

allocaArray0 :: (Storable a) => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 count = allocaArray (count + 1)

callocArray :: (Storable a) => Int -> IO (Ptr a)
callocArray = callocArrayOf undefined

callocArrayOf :: (Storable a) => a -> Int -> IO (Ptr a)
callocArrayOf placeholder count = callocBytes (count * sizeOf placeholder)

callocArray0 :: (Storable a) => Int -> IO (Ptr a)
callocArray0 count = callocArray (count + 1)

reallocArray :: (Storable a) => Ptr a -> Int -> IO (Ptr a)
reallocArray = reallocArrayOf undefined

reallocArrayOf :: (Storable a) => a -> Ptr a -> Int -> IO (Ptr a)
reallocArrayOf placeholder pointer count = reallocBytes pointer (count * sizeOf placeholder)

reallocArray0 :: (Storable a) => Ptr a -> Int -> IO (Ptr a)
reallocArray0 pointer count = reallocArray pointer (count + 1)

peekArray :: (Storable a) => Int -> Ptr a -> IO [a]
peekArray count pointer =
  if count <= 0
    then return []
    else peekArrayFrom pointer 0 count

peekArrayFrom :: (Storable a) => Ptr a -> Int -> Int -> IO [a]
peekArrayFrom pointer index count =
  if index >= count
    then return []
    else do
      value <- peekElemOff pointer index
      rest <- peekArrayFrom pointer (index + 1) count
      return (value : rest)

peekArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO [a]
peekArray0 marker pointer = do
  count <- lengthArray0 marker pointer
  peekArray count pointer

pokeArray :: (Storable a) => Ptr a -> [a] -> IO ()
pokeArray pointer values =
  mapM_ (uncurry (pokeElemOff pointer)) (zip (countFrom 0) values)

countFrom :: Int -> [Int]
countFrom start = start : countFrom (start + 1)

pokeArray0 :: (Storable a) => a -> Ptr a -> [a] -> IO ()
pokeArray0 marker pointer values = pokeArray pointer values >> pokeElemOff pointer (length values) marker

newArray :: (Storable a) => [a] -> IO (Ptr a)
newArray values = do
  pointer <- mallocArray (length values)
  pokeArray pointer values
  return pointer

newArray0 :: (Storable a) => a -> [a] -> IO (Ptr a)
newArray0 marker values = do
  pointer <- mallocArray0 (length values)
  pokeArray0 marker pointer values
  return pointer

withArray :: (Storable a) => [a] -> (Ptr a -> IO b) -> IO b
withArray values = withArrayLen values . withoutLength

withArray0 :: (Storable a) => a -> [a] -> (Ptr a -> IO b) -> IO b
withArray0 marker values = withArrayLen0 marker values . withoutLength

withoutLength :: (Ptr a -> IO b) -> Int -> Ptr a -> IO b
withoutLength action _ = action

withArrayLen :: (Storable a) => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen values action =
  allocaArray count (\pointer -> pokeArray pointer values >> action count pointer)
  where
    count = length values

withArrayLen0 :: (Storable a) => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen0 marker values action =
  allocaArray0 count (\pointer -> pokeArray0 marker pointer values >> action count pointer)
  where
    count = length values

copyArray :: (Storable a) => Ptr a -> Ptr a -> Int -> IO ()
copyArray destination source count = copyBytes destination source (count * sizeOf (pointerElementOf destination))

moveArray :: (Storable a) => Ptr a -> Ptr a -> Int -> IO ()
moveArray destination source count = moveBytes destination source (count * sizeOf (pointerElementOf destination))

pointerElementOf :: Ptr a -> a
pointerElementOf _ = undefined

lengthArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO Int
lengthArray0 marker pointer = lengthArrayFrom marker pointer 0

lengthArrayFrom :: (Storable a, Eq a) => a -> Ptr a -> Int -> IO Int
lengthArrayFrom marker pointer index = do
  value <- peekElemOff pointer index
  if value == marker
    then return index
    else lengthArrayFrom marker pointer (index + 1)

advancePtr :: (Storable a) => Ptr a -> Int -> Ptr a
advancePtr pointer count = pointer `plusPtr` (count * sizeOf (pointerElementOf pointer))
