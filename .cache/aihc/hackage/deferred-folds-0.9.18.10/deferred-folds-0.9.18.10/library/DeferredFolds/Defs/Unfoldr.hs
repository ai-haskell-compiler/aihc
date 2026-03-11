{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

module DeferredFolds.Defs.Unfoldr where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Short.Internal as ShortByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Text.Internal as TextInternal
import qualified Data.Vector.Generic as GenericVector
import DeferredFolds.Prelude hiding (fold, reverse)
import qualified DeferredFolds.Prelude as Prelude
import DeferredFolds.Types
import qualified DeferredFolds.Util.TextArray as TextArrayUtil

deriving instance Functor Unfoldr

instance Applicative Unfoldr where
  pure x = Unfoldr (\step -> step x)
  (<*>) = ap

instance Alternative Unfoldr where
  empty = Unfoldr (const id)
  {-# INLINE (<|>) #-}
  (<|>) (Unfoldr left) (Unfoldr right) = Unfoldr (\step init -> left step (right step init))

instance Monad Unfoldr where
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (Unfoldr left) rightK =
    Unfoldr $ \step -> left $ \input -> case rightK input of Unfoldr right -> right step

instance MonadPlus Unfoldr where
  mzero = empty
  mplus = (<|>)

instance Semigroup (Unfoldr a) where
  (<>) = (<|>)

instance Monoid (Unfoldr a) where
  mempty = empty
  mappend = (<>)

instance Foldable Unfoldr where
  {-# INLINE foldMap #-}
  foldMap fn (Unfoldr unfoldr) = unfoldr (mappend . fn) mempty
  {-# INLINE foldr #-}
  foldr step state (Unfoldr run) = run step state
  foldl = foldl'
  {-# INLINE foldl' #-}
  foldl' leftStep state (Unfoldr unfoldr) = unfoldr rightStep id state
    where
      rightStep element k state = k $! leftStep state element

instance Traversable Unfoldr where
  traverse f (Unfoldr unfoldr) =
    unfoldr (\a next -> liftA2 cons (f a) next) (pure mempty)

instance (Eq a) => Eq (Unfoldr a) where
  (==) left right = toList left == toList right

instance (Show a) => Show (Unfoldr a) where
  show = show . toList

instance IsList (Unfoldr a) where
  type Item (Unfoldr a) = a
  fromList list = foldable list
  toList = foldr (:) []

-- | Apply a Gonzalez fold
{-# INLINE fold #-}
fold :: Fold input output -> Unfoldr input -> output
fold (Fold step init extract) (Unfoldr run) =
  run (\input next state -> next $! step state input) extract init

-- | Apply a monadic Gonzalez fold
{-# INLINE foldM #-}
foldM :: (Monad m) => FoldM m input output -> Unfoldr input -> m output
foldM (FoldM step init extract) (Unfoldr unfoldr) =
  init >>= unfoldr (\input next state -> step state input >>= next) return >>= extract

-- | Construct from any value by supplying a definition of foldr
{-# INLINE foldrAndContainer #-}
foldrAndContainer :: (forall x. (elem -> x -> x) -> x -> container -> x) -> container -> Unfoldr elem
foldrAndContainer foldr a = Unfoldr (\step init -> foldr step init a)

-- | Construct from any foldable
{-# INLINE foldable #-}
foldable :: (Foldable foldable) => foldable a -> Unfoldr a
foldable = foldrAndContainer foldr

-- | Elements of IntSet.
{-# INLINE intSet #-}
intSet :: IntSet -> Unfoldr Int
intSet = foldrAndContainer IntSet.foldr

-- | Filter the values given a predicate
{-# INLINE filter #-}
filter :: (a -> Bool) -> Unfoldr a -> Unfoldr a
filter test (Unfoldr run) = Unfoldr (\step -> run (\element state -> if test element then step element state else state))

-- | Ascending infinite stream of enums starting from the one specified
{-# INLINE enumsFrom #-}
enumsFrom :: (Enum a) => a -> Unfoldr a
enumsFrom from = Unfoldr $ \step init ->
  let loop int = step int (loop (succ int))
   in loop from

-- | Enums in the specified inclusive range
{-# INLINE enumsInRange #-}
enumsInRange :: (Enum a, Ord a) => a -> a -> Unfoldr a
enumsInRange from to =
  Unfoldr $ \step init ->
    let loop int =
          if int <= to
            then step int (loop (succ int))
            else init
     in loop from

-- | Ascending infinite stream of ints starting from the one specified
{-# INLINE intsFrom #-}
intsFrom :: Int -> Unfoldr Int
intsFrom = enumsFrom

-- | Ints in the specified inclusive range
{-# INLINE intsInRange #-}
intsInRange :: Int -> Int -> Unfoldr Int
intsInRange = enumsInRange

-- | Associations of a map
{-# INLINE mapAssocs #-}
mapAssocs :: Map key value -> Unfoldr (key, value)
mapAssocs map =
  Unfoldr (\step init -> Map.foldrWithKey (\key value state -> step (key, value) state) init map)

-- | Associations of an intmap
{-# INLINE intMapAssocs #-}
intMapAssocs :: IntMap value -> Unfoldr (Int, value)
intMapAssocs intMap =
  Unfoldr (\step init -> IntMap.foldrWithKey (\key value state -> step (key, value) state) init intMap)

-- | Keys of a hash-map
{-# INLINE hashMapKeys #-}
hashMapKeys :: HashMap key value -> Unfoldr key
hashMapKeys hashMap =
  Unfoldr (\step init -> HashMap.foldrWithKey (\key _ state -> step key state) init hashMap)

-- | Associations of a hash-map
{-# INLINE hashMapAssocs #-}
hashMapAssocs :: HashMap key value -> Unfoldr (key, value)
hashMapAssocs hashMap =
  Unfoldr (\step init -> HashMap.foldrWithKey (\key value state -> step (key, value) state) init hashMap)

-- | Value of a hash-map by key
{-# INLINE hashMapAt #-}
hashMapAt :: (Hashable key, Eq key) => HashMap key value -> key -> Unfoldr value
hashMapAt hashMap key = foldable (HashMap.lookup key hashMap)

-- | Value of a hash-map by key
{-# INLINE hashMapValue #-}
{-# DEPRECATED hashMapValue "Use 'hashMapAt' instead" #-}
hashMapValue :: (Hashable key, Eq key) => key -> HashMap key value -> Unfoldr value
hashMapValue key = foldable . HashMap.lookup key

-- | Values of a hash-map by their keys
{-# INLINE hashMapValues #-}
hashMapValues :: (Hashable key, Eq key) => HashMap key value -> Unfoldr key -> Unfoldr value
hashMapValues hashMap keys = keys >>= flip hashMapValue hashMap

-- | Bytes of a bytestring
{-# INLINE byteStringBytes #-}
byteStringBytes :: ByteString -> Unfoldr Word8
byteStringBytes bs = Unfoldr (\step init -> ByteString.foldr step init bs)

-- | Bytes of a short bytestring
{-# INLINE shortByteStringBytes #-}
shortByteStringBytes :: ShortByteString -> Unfoldr Word8
shortByteStringBytes (ShortByteString.SBS ba#) = primArray (PrimArray ba#)

-- | Elements of a prim array
{-# INLINE primArray #-}
primArray :: (Prim prim) => PrimArray prim -> Unfoldr prim
primArray ba = Unfoldr $ \f z -> foldrPrimArray f z ba

-- | Elements of a prim array coming paired with indices
{-# INLINE primArrayWithIndices #-}
primArrayWithIndices :: (Prim prim) => PrimArray prim -> Unfoldr (Int, prim)
primArrayWithIndices pa = Unfoldr $ \step state ->
  let !size = sizeofPrimArray pa
      loop index =
        if index < size
          then step (index, indexPrimArray pa index) (loop (succ index))
          else state
   in loop 0

-- | Elements of a vector
{-# INLINE vector #-}
vector :: (GenericVector.Vector vector a) => vector a -> Unfoldr a
vector vector = Unfoldr $ \step state -> GenericVector.foldr step state vector

-- | Elements of a vector coming paired with indices
{-# INLINE vectorWithIndices #-}
vectorWithIndices :: (GenericVector.Vector vector a) => vector a -> Unfoldr (Int, a)
vectorWithIndices vector = Unfoldr $ \step state -> GenericVector.ifoldr (\index a -> step (index, a)) state vector

-- |
-- Binary digits of a non-negative integral number.
{-# INLINEABLE binaryDigits #-}
binaryDigits :: (Integral a) => a -> Unfoldr a
binaryDigits = reverse . reverseBinaryDigits

-- |
-- Binary digits of a non-negative integral number in reverse order.
{-# INLINEABLE reverseBinaryDigits #-}
reverseBinaryDigits :: (Integral a) => a -> Unfoldr a
reverseBinaryDigits = reverseDigits 2

-- |
-- Octal digits of a non-negative integral number.
{-# INLINEABLE octalDigits #-}
octalDigits :: (Integral a) => a -> Unfoldr a
octalDigits = reverse . reverseOctalDigits

-- |
-- Octal digits of a non-negative integral number in reverse order.
{-# INLINEABLE reverseOctalDigits #-}
reverseOctalDigits :: (Integral a) => a -> Unfoldr a
reverseOctalDigits = reverseDigits 8

-- |
-- Decimal digits of a non-negative integral number.
{-# INLINEABLE decimalDigits #-}
decimalDigits :: (Integral a) => a -> Unfoldr a
decimalDigits = reverse . reverseDecimalDigits

-- |
-- Decimal digits of a non-negative integral number in reverse order.
-- More efficient than 'decimalDigits'.
{-# INLINEABLE reverseDecimalDigits #-}
reverseDecimalDigits :: (Integral a) => a -> Unfoldr a
reverseDecimalDigits = reverseDigits 10

-- |
-- Hexadecimal digits of a non-negative number.
{-# INLINEABLE hexadecimalDigits #-}
hexadecimalDigits :: (Integral a) => a -> Unfoldr a
hexadecimalDigits = reverse . reverseHexadecimalDigits

-- |
-- Hexadecimal digits of a non-negative number in reverse order.
{-# INLINEABLE reverseHexadecimalDigits #-}
reverseHexadecimalDigits :: (Integral a) => a -> Unfoldr a
reverseHexadecimalDigits = reverseDigits 16

-- |
-- Digits of a non-negative number in numeral system based on the specified radix.
-- The digits come in reverse order.
--
-- E.g., here's how an unfold of binary digits in proper order looks:
--
-- @
-- binaryDigits :: Integral a => a -> Unfoldr a
-- binaryDigits = 'reverse' . 'reverseDigits' 2
-- @
{-# INLINEABLE reverseDigits #-}
reverseDigits ::
  (Integral a) =>
  -- | Radix
  a ->
  -- | Number
  a ->
  Unfoldr a
reverseDigits radix x = Unfoldr $ \step init ->
  let loop x = case divMod x radix of
        (next, digit) -> step digit (if next <= 0 then init else loop next)
   in loop x

-- |
-- Reverse the order.
--
-- Use with care, because it requires to allocate all elements.
reverse :: Unfoldr a -> Unfoldr a
reverse (Unfoldr unfoldr) = Unfoldr $ \step -> unfoldr (\a f -> f . step a) id

zipWith :: (a -> b -> c) -> Unfoldr a -> Unfoldr b -> Unfoldr c
zipWith f l r =
  Prelude.zipWith f (toList l) (toList r) & foldable

-- |
-- Lift into an unfold, which produces pairs with index.
zipWithIndex :: Unfoldr a -> Unfoldr (Int, a)
zipWithIndex (Unfoldr unfoldr) = Unfoldr $ \indexedStep indexedState ->
  unfoldr
    (\a nextStateByIndex index -> indexedStep (index, a) (nextStateByIndex (succ index)))
    (const indexedState)
    0

-- |
-- Lift into an unfold, which produces pairs with right-associative index.
{-# DEPRECATED zipWithReverseIndex "This function builds up stack. Use 'zipWithIndex' instead." #-}
zipWithReverseIndex :: Unfoldr a -> Unfoldr (Int, a)
zipWithReverseIndex (Unfoldr unfoldr) = Unfoldr $ \step init ->
  snd
    $ unfoldr
      (\a (index, state) -> (succ index, step (index, a) state))
      (0, init)

-- |
-- Indices of set bits.
{-# INLINEABLE setBitIndices #-}
setBitIndices :: (FiniteBits a) => a -> Unfoldr Int
setBitIndices a =
  let !size = finiteBitSize a
   in Unfoldr $ \step state ->
        let loop !index =
              if index < size
                then
                  if testBit a index
                    then step index (loop (succ index))
                    else loop (succ index)
                else state
         in loop 0

-- |
-- Indices of unset bits.
{-# INLINEABLE unsetBitIndices #-}
unsetBitIndices :: (FiniteBits a) => a -> Unfoldr Int
unsetBitIndices a =
  let !size = finiteBitSize a
   in Unfoldr $ \step state ->
        let loop !index =
              if index < size
                then
                  if testBit a index
                    then loop (succ index)
                    else step index (loop (succ index))
                else state
         in loop 0

take :: Int -> Unfoldr a -> Unfoldr a
take amount (Unfoldr unfoldr) = Unfoldr $ \step init ->
  unfoldr
    ( \a nextState index ->
        if index < amount
          then step a (nextState (succ index))
          else init
    )
    (const init)
    0

takeWhile :: (a -> Bool) -> Unfoldr a -> Unfoldr a
takeWhile predicate (Unfoldr unfoldr) = Unfoldr $ \step init ->
  unfoldr
    ( \a nextState ->
        if predicate a
          then step a nextState
          else init
    )
    init

cons :: a -> Unfoldr a -> Unfoldr a
cons a (Unfoldr unfoldr) = Unfoldr $ \step init -> step a (unfoldr step init)

snoc :: a -> Unfoldr a -> Unfoldr a
snoc a (Unfoldr unfoldr) = Unfoldr $ \step init -> unfoldr step (step a init)

-- |
-- Insert a separator value between each element.
--
-- Behaves the same way as 'Data.List.intersperse'.
{-# INLINE intersperse #-}
intersperse :: a -> Unfoldr a -> Unfoldr a
intersperse sep (Unfoldr unfoldr) =
  Unfoldr $ \step init ->
    unfoldr
      ( \a next first ->
          if first
            then step a (next False)
            else step sep (step a (next False))
      )
      (const init)
      True

-- |
-- Reproduces the behaviour of 'Data.Text.unpack'.
--
-- Implementation is efficient and avoids allocation of an intermediate list.
textChars :: Text -> Unfoldr Char
textChars (TextInternal.Text arr off len) =
  Unfoldr $ \step term ->
    let loop !offset =
          if offset >= len
            then term
            else TextArrayUtil.iter arr offset $ \char nextOffset ->
              step char (loop nextOffset)
     in loop off

-- |
-- Reproduces the behaviour of 'Data.Text.words'.
--
-- Implementation is efficient and avoids allocation of an intermediate list.
textWords :: Text -> Unfoldr Text
textWords (TextInternal.Text arr off len) =
  Unfoldr $ \step term ->
    let loop !wordOffset !offset =
          if offset >= len
            then
              if wordOffset == offset
                then term
                else step (chunk wordOffset offset) term
            else TextArrayUtil.iter arr offset $ \char nextOffset ->
              if isSpace char
                then
                  if wordOffset == offset
                    then loop nextOffset nextOffset
                    else step (chunk wordOffset offset) (loop nextOffset nextOffset)
                else loop wordOffset nextOffset
     in loop off off
  where
    chunk startOffset afterEndOffset =
      TextInternal.Text arr startOffset (afterEndOffset - startOffset)

-- |
-- Transformer of chars,
-- replaces all space-like chars with space,
-- all newline-like chars with @\\n@,
-- and trims their duplicate sequences to single-char.
-- Oh yeah, it also trims whitespace from beginning and end.
trimWhitespace :: Unfoldr Char -> Unfoldr Char
trimWhitespace =
  \foldable ->
    Unfoldr $ \substep subterm ->
      foldr (step substep) (finalize subterm) foldable False False False
  where
    step substep char next notFirst spacePending newlinePending =
      if isSpace char
        then
          if char == '\n' || char == '\r'
            then next notFirst False True
            else next notFirst True newlinePending
        else
          let mapper =
                if notFirst
                  then
                    if newlinePending
                      then substep '\n'
                      else
                        if spacePending
                          then substep ' '
                          else id
                  else id
           in mapper $ substep char $ next True False False
    finalize subterm notFirst spacePending newlinePending =
      subterm
