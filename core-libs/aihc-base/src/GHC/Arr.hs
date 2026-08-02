{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Arr
  ( Ix (..),
    Array (..),
    STArray (..),
    arrEleBottom,
    array,
    listArray,
    (!),
    safeRangeSize,
    negRange,
    safeIndex,
    badSafeIndex,
    bounds,
    numElements,
    numElementsSTArray,
    indices,
    elems,
    assocs,
    accumArray,
    adjust,
    (//),
    accum,
    amap,
    ixmap,
    eqArray,
    cmpArray,
    cmpIntArray,
    newSTArray,
    boundsSTArray,
    readSTArray,
    writeSTArray,
    freezeSTArray,
    thawSTArray,
    foldlElems,
    foldlElems',
    foldl1Elems,
    foldrElems,
    foldrElems',
    foldr1Elems,
    fill,
    done,
    unsafeArray,
    unsafeArray',
    lessSafeIndex,
    unsafeAt,
    unsafeReplace,
    unsafeAccumArray,
    unsafeAccumArray',
    unsafeAccum,
    unsafeReadSTArray,
    unsafeWriteSTArray,
    unsafeFreezeSTArray,
    unsafeThawSTArray,
  )
where

import GHC.Int (Int (..))
import GHC.Ix (Ix (..))
import GHC.Prim
  ( Array#,
    MutableArray#,
    indexArray#,
    newArray#,
    readArray#,
    sameMutableArray#,
    unsafeFreezeArray#,
    unsafeThawArray#,
    writeArray#,
  )
import GHC.ST (ST (..), STRep, runST)
import Prelude

infixl 9 !, //

-- | An immutable, non-strict boxed array.
data Array i e = Array i i Int (Array# e)

-- | A mutable, non-strict boxed array in an 'ST' state thread.
data STArray s i e = STArray i i Int (MutableArray# s e)

instance Eq (STArray s i e) where
  STArray _ _ _ left == STArray _ _ _ right =
    case sameMutableArray# left right of
      0# -> False
      _ -> True

  left /= right = not (left == right)

-- | The value stored in an uninitialized immutable array slot.
arrEleBottom :: a
arrEleBottom = arrEleBottom

array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array bounds' associations =
  unsafeArray' bounds' size (indexedAssociations bounds' size associations)
  where
    size = safeRangeSize bounds'

unsafeArray :: (Ix i) => (i, i) -> [(Int, e)] -> Array i e
unsafeArray bounds' = unsafeArray' bounds' (rangeSize bounds')

unsafeArray' :: (i, i) -> Int -> [(Int, e)] -> Array i e
unsafeArray' (lower, upper) size@(I# size#) associations =
  runST
    ( ST
        ( \state ->
            case newArray# size# arrEleBottom state of
              (# nextState, mutable# #) ->
                foldrList (fill mutable#) (done lower upper size mutable#) associations nextState
        )
    )

fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill mutable# (I# index#, value) next state =
  case writeArray# mutable# index# value state of
    nextState -> next nextState

done :: i -> i -> Int -> MutableArray# s e -> STRep s (Array i e)
done lower upper size mutable# state =
  case unsafeFreezeArray# mutable# state of
    (# nextState, immutable# #) -> (# nextState, Array lower upper size immutable# #)

listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray bounds' values = unsafeArray' bounds' size (numberValues 0 size values)
  where
    size = safeRangeSize bounds'

(!) :: (Ix i) => Array i e -> i -> e
array'@(Array lower upper size _) ! index' =
  unsafeAt array' (safeIndex (lower, upper) size index')

safeRangeSize :: (Ix i) => (i, i) -> Int
safeRangeSize bounds' =
  case rangeSize bounds' of
    size ->
      case size < 0 of
        True -> negRange
        False -> size

negRange :: Int
negRange = negRange

safeIndex :: (Ix i) => (i, i) -> Int -> i -> Int
safeIndex bounds' size index' =
  case index bounds' index' of
    offset ->
      case 0 <= offset && offset < size of
        True -> offset
        False -> badSafeIndex offset size

lessSafeIndex :: (Ix i) => (i, i) -> Int -> i -> Int
lessSafeIndex bounds' _ = index bounds'

badSafeIndex :: Int -> Int -> Int
badSafeIndex _ _ = badSafeIndex 0 0

unsafeAt :: Array i e -> Int -> e
unsafeAt (Array _ _ _ immutable#) (I# index#) = indexArray# immutable# index#

bounds :: Array i e -> (i, i)
bounds (Array lower upper _ _) = (lower, upper)

numElements :: Array i e -> Int
numElements (Array _ _ size _) = size

indices :: (Ix i) => Array i e -> [i]
indices (Array lower upper _ _) = range (lower, upper)

elems :: Array i e -> [e]
elems array' = elementsFrom 0 (numElements array') array'

assocs :: (Ix i) => Array i e -> [(i, e)]
assocs array' = associateIndices (indices array') (elems array')

accumArray :: (Ix i) => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
accumArray combine initial bounds' associations =
  unsafeAccumArray' combine initial bounds' size (indexedAssociations bounds' size associations)
  where
    size = safeRangeSize bounds'

unsafeAccumArray :: (Ix i) => (e -> a -> e) -> e -> (i, i) -> [(Int, a)] -> Array i e
unsafeAccumArray combine initial bounds' =
  unsafeAccumArray' combine initial bounds' (rangeSize bounds')

unsafeAccumArray' :: (e -> a -> e) -> e -> (i, i) -> Int -> [(Int, a)] -> Array i e
unsafeAccumArray' combine initial (lower, upper) size@(I# size#) associations =
  runST
    ( ST
        ( \state ->
            case newArray# size# initial state of
              (# nextState, mutable# #) ->
                foldrList (adjust' combine mutable#) (done lower upper size mutable#) associations nextState
        )
    )

adjust :: (e -> a -> e) -> MutableArray# s e -> (Int, a) -> STRep s b -> STRep s b
adjust combine mutable# (I# index#, value) next state =
  case readArray# mutable# index# state of
    (# readState, old #) ->
      case writeArray# mutable# index# (combine old value) readState of
        writeState -> next writeState

adjust' :: (e -> a -> e) -> MutableArray# s e -> (Int, a) -> STRep s b -> STRep s b
adjust' combine mutable# (I# index#, value) next state =
  case readArray# mutable# index# state of
    (# readState, old #) ->
      case combine old value of
        combined ->
          case writeArray# mutable# index# combined readState of
            writeState -> next writeState

(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
array'@(Array lower upper size _) // associations =
  unsafeReplace array' (indexedAssociations (lower, upper) size associations)

unsafeReplace :: Array i e -> [(Int, e)] -> Array i e
unsafeReplace array' associations =
  runST
    ( do
        mutable@(STArray lower upper size mutable#) <- thawSTArray array'
        writeAssociations mutable associations
        unsafeFreezeSTArray (STArray lower upper size mutable#)
    )

accum :: (Ix i) => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
accum combine array'@(Array lower upper size _) associations =
  unsafeAccum combine array' (indexedAssociations (lower, upper) size associations)

unsafeAccum :: (e -> a -> e) -> Array i e -> [(Int, a)] -> Array i e
unsafeAccum combine array' associations =
  runST
    ( do
        mutable <- thawSTArray array'
        accumulateAssociations combine mutable associations
        unsafeFreezeSTArray mutable
    )

amap :: (a -> b) -> Array i a -> Array i b
amap function array' =
  unsafeArray' (bounds array') (numElements array') (mapElements 0 function array')

ixmap :: (Ix i, Ix j) => (i, i) -> (i -> j) -> Array j e -> Array i e
ixmap bounds' function array' =
  array bounds' (mapIndices function array' (range bounds'))

eqArray :: (Ix i, Eq e) => Array i e -> Array i e -> Bool
eqArray left@(Array lowerLeft upperLeft sizeLeft _) right@(Array lowerRight upperRight sizeRight _) =
  case sizeLeft == 0 of
    True -> sizeRight == 0
    False ->
      lowerLeft
        == lowerRight
        && upperLeft
        == upperRight
        && sizeLeft
        == sizeRight
        && equalElements 0 sizeLeft left right

cmpArray :: (Ix i, Ord e) => Array i e -> Array i e -> Ordering
cmpArray left@(Array lowerLeft upperLeft sizeLeft _) right@(Array lowerRight upperRight sizeRight _) =
  case sizeLeft == 0 of
    True -> compare sizeLeft sizeRight
    False ->
      case sizeRight == 0 of
        True -> GT
        False ->
          case compare lowerLeft lowerRight of
            EQ ->
              case compareElements 0 (min sizeLeft sizeRight) left right of
                EQ -> compare upperLeft upperRight
                result -> result
            result -> result

cmpIntArray :: (Ord e) => Array Int e -> Array Int e -> Ordering
cmpIntArray = cmpArray

instance Functor (Array i) where
  fmap = amap

instance (Ix i, Eq e) => Eq (Array i e) where
  (==) = eqArray

  left /= right = not (left == right)

instance (Ix i, Ord e) => Ord (Array i e) where
  compare = cmpArray
  left < right = arrayLess (cmpArray left right)
  left <= right = arrayLessOrEqual (cmpArray left right)
  left > right = arrayGreater (cmpArray left right)
  left >= right = arrayGreaterOrEqual (cmpArray left right)
  max left right =
    case cmpArray left right of
      LT -> right
      _ -> left
  min left right =
    case cmpArray left right of
      GT -> right
      _ -> left

instance (Ix i, Show i, Show e) => Show (Array i e) where
  showsPrec _ array' =
    showString "array " . shows (bounds array') . showChar ' ' . shows (assocs array')

newSTArray :: (Ix i) => (i, i) -> e -> ST s (STArray s i e)
newSTArray bounds' = newRawSTArray bounds' (safeRangeSize bounds')

newRawSTArray :: (i, i) -> Int -> e -> ST s (STArray s i e)
newRawSTArray (lower, upper) size@(I# size#) initial =
  ST
    ( \state ->
        case newArray# size# initial state of
          (# nextState, mutable# #) -> (# nextState, STArray lower upper size mutable# #)
    )

boundsSTArray :: STArray s i e -> (i, i)
boundsSTArray (STArray lower upper _ _) = (lower, upper)

numElementsSTArray :: STArray s i e -> Int
numElementsSTArray (STArray _ _ size _) = size

readSTArray :: (Ix i) => STArray s i e -> i -> ST s e
readSTArray array'@(STArray lower upper size _) index' =
  unsafeReadSTArray array' (safeIndex (lower, upper) size index')

unsafeReadSTArray :: STArray s i e -> Int -> ST s e
unsafeReadSTArray (STArray _ _ _ mutable#) (I# index#) = ST (readArray# mutable# index#)

writeSTArray :: (Ix i) => STArray s i e -> i -> e -> ST s ()
writeSTArray array'@(STArray lower upper size _) index' =
  unsafeWriteSTArray array' (safeIndex (lower, upper) size index')

unsafeWriteSTArray :: STArray s i e -> Int -> e -> ST s ()
unsafeWriteSTArray (STArray _ _ _ mutable#) (I# index#) value =
  ST
    ( \state ->
        case writeArray# mutable# index# value state of
          nextState -> (# nextState, () #)
    )

freezeSTArray :: STArray s i e -> ST s (Array i e)
freezeSTArray source@(STArray lower upper size _) = do
  destination <- newRawSTArray (lower, upper) size arrEleBottom
  copyMutable 0 size source destination
  unsafeFreezeSTArray destination

unsafeFreezeSTArray :: STArray s i e -> ST s (Array i e)
unsafeFreezeSTArray (STArray lower upper size mutable#) =
  ST
    ( \state ->
        case unsafeFreezeArray# mutable# state of
          (# nextState, immutable# #) -> (# nextState, Array lower upper size immutable# #)
    )

thawSTArray :: Array i e -> ST s (STArray s i e)
thawSTArray source = do
  destination <- newRawSTArray (bounds source) (numElements source) arrEleBottom
  copyImmutable 0 (numElements source) source destination
  return destination

unsafeThawSTArray :: Array i e -> ST s (STArray s i e)
unsafeThawSTArray (Array lower upper size immutable#) =
  ST
    ( \state ->
        case unsafeThawArray# immutable# state of
          (# nextState, mutable# #) -> (# nextState, STArray lower upper size mutable# #)
    )

foldrElems :: (a -> b -> b) -> b -> Array i a -> b
foldrElems combine initial array' = foldRightFrom 0 (numElements array') combine initial array'

foldlElems :: (b -> a -> b) -> b -> Array i a -> b
foldlElems combine initial array' = foldLeftFrom 0 (numElements array') combine initial array'

foldrElems' :: (a -> b -> b) -> b -> Array i a -> b
foldrElems' combine initial array' =
  strictFoldRightFrom (numElements array' - 1) combine initial array'

foldlElems' :: (b -> a -> b) -> b -> Array i a -> b
foldlElems' combine initial array' =
  strictFoldLeftFrom 0 (numElements array') combine initial array'

foldl1Elems :: (a -> a -> a) -> Array i a -> a
foldl1Elems combine array' =
  case numElements array' == 0 of
    True -> emptyFold
    False -> foldLeftFrom 1 (numElements array') combine (unsafeAt array' 0) array'

foldr1Elems :: (a -> a -> a) -> Array i a -> a
foldr1Elems combine array' =
  case numElements array' == 0 of
    True -> emptyFold
    False -> foldRightFrom 0 (numElements array' - 1) combine (unsafeAt array' (numElements array' - 1)) array'

emptyFold :: a
emptyFold = emptyFold

indexedAssociations :: (Ix i) => (i, i) -> Int -> [(i, a)] -> [(Int, a)]
indexedAssociations _ _ [] = []
indexedAssociations bounds' size ((index', value) : rest) =
  (safeIndex bounds' size index', value) : indexedAssociations bounds' size rest

numberValues :: Int -> Int -> [a] -> [(Int, a)]
numberValues _ _ [] = []
numberValues index' size (value : rest) =
  case index' == size of
    True -> []
    False -> (index', value) : numberValues (index' + 1) size rest

elementsFrom :: Int -> Int -> Array i e -> [e]
elementsFrom index' size array' =
  case index' == size of
    True -> []
    False -> unsafeAt array' index' : elementsFrom (index' + 1) size array'

associateIndices :: [i] -> [e] -> [(i, e)]
associateIndices [] _ = []
associateIndices _ [] = []
associateIndices (index' : restIndices) (value : restValues) =
  (index', value) : associateIndices restIndices restValues

mapElements :: Int -> (a -> b) -> Array i a -> [(Int, b)]
mapElements index' function array' =
  case index' == numElements array' of
    True -> []
    False ->
      (index', function (unsafeAt array' index')) : mapElements (index' + 1) function array'

mapIndices :: (Ix j) => (i -> j) -> Array j e -> [i] -> [(i, e)]
mapIndices _ _ [] = []
mapIndices function array' (index' : rest) =
  (index', array' ! function index') : mapIndices function array' rest

equalElements :: (Eq e) => Int -> Int -> Array i e -> Array i e -> Bool
equalElements index' size left right =
  case index' == size of
    True -> True
    False ->
      unsafeAt left index'
        == unsafeAt right index'
        && equalElements (index' + 1) size left right

compareElements :: (Ord e) => Int -> Int -> Array i e -> Array i e -> Ordering
compareElements index' size left right =
  case index' == size of
    True -> EQ
    False ->
      case compare (unsafeAt left index') (unsafeAt right index') of
        EQ -> compareElements (index' + 1) size left right
        result -> result

arrayLess :: Ordering -> Bool
arrayLess LT = True
arrayLess _ = False

arrayLessOrEqual :: Ordering -> Bool
arrayLessOrEqual GT = False
arrayLessOrEqual _ = True

arrayGreater :: Ordering -> Bool
arrayGreater GT = True
arrayGreater _ = False

arrayGreaterOrEqual :: Ordering -> Bool
arrayGreaterOrEqual LT = False
arrayGreaterOrEqual _ = True

writeAssociations :: STArray s i e -> [(Int, e)] -> ST s ()
writeAssociations _ [] = return ()
writeAssociations array' ((index', value) : rest) =
  unsafeWriteSTArray array' index' value >> writeAssociations array' rest

accumulateAssociations :: (e -> a -> e) -> STArray s i e -> [(Int, a)] -> ST s ()
accumulateAssociations _ _ [] = return ()
accumulateAssociations combine array' ((index', value) : rest) = do
  old <- unsafeReadSTArray array' index'
  case combine old value of
    combined -> unsafeWriteSTArray array' index' combined
  accumulateAssociations combine array' rest

copyMutable :: Int -> Int -> STArray s i e -> STArray s i e -> ST s ()
copyMutable index' size source destination = do
  case index' == size of
    True -> return ()
    False -> do
      value <- unsafeReadSTArray source index'
      unsafeWriteSTArray destination index' value
      copyMutable (index' + 1) size source destination

copyImmutable :: Int -> Int -> Array i e -> STArray s i e -> ST s ()
copyImmutable index' size source destination = do
  case index' == size of
    True -> return ()
    False -> do
      unsafeWriteSTArray destination index' (unsafeAt source index')
      copyImmutable (index' + 1) size source destination

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList _ initial [] = initial
foldrList combine initial (value : rest) = combine value (foldrList combine initial rest)

foldRightFrom :: Int -> Int -> (a -> b -> b) -> b -> Array i a -> b
foldRightFrom index' size combine initial array' =
  case index' == size of
    True -> initial
    False -> combine (unsafeAt array' index') (foldRightFrom (index' + 1) size combine initial array')

foldLeftFrom :: Int -> Int -> (b -> a -> b) -> b -> Array i a -> b
foldLeftFrom index' size combine initial array' =
  case index' == size of
    True -> initial
    False -> foldLeftFrom (index' + 1) size combine (combine initial (unsafeAt array' index')) array'

strictFoldRightFrom :: Int -> (a -> b -> b) -> b -> Array i a -> b
strictFoldRightFrom index' combine initial array' =
  case index' < 0 of
    True -> initial
    False ->
      case combine (unsafeAt array' index') initial of
        result -> strictFoldRightFrom (index' - 1) combine result array'

strictFoldLeftFrom :: Int -> Int -> (b -> a -> b) -> b -> Array i a -> b
strictFoldLeftFrom index' size combine initial array' =
  case index' == size of
    True -> initial
    False ->
      case combine initial (unsafeAt array' index') of
        result -> strictFoldLeftFrom (index' + 1) size combine result array'
