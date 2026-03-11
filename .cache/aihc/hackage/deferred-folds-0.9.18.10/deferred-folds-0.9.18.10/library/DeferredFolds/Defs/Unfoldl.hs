{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

module DeferredFolds.Defs.Unfoldl where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Short.Internal as ShortByteString
import qualified Data.IntMap.Strict as D
import qualified Data.Map.Strict as C
import DeferredFolds.Prelude hiding (fold)
import qualified DeferredFolds.Prelude as A
import DeferredFolds.Types

deriving instance Functor Unfoldl

instance Applicative Unfoldl where
  pure x =
    Unfoldl (\step init -> step init x)
  (<*>) = ap

instance Alternative Unfoldl where
  empty =
    Unfoldl (const id)
  {-# INLINE (<|>) #-}
  (<|>) (Unfoldl left) (Unfoldl right) =
    Unfoldl (\step init -> right step (left step init))

instance Monad Unfoldl where
  return = pure
  (>>=) (Unfoldl left) rightK =
    Unfoldl $ \step init ->
      let newStep output x =
            case rightK x of
              Unfoldl right ->
                right step output
       in left newStep init

instance MonadPlus Unfoldl where
  mzero = empty
  mplus = (<|>)

instance Semigroup (Unfoldl a) where
  (<>) = (<|>)

instance Monoid (Unfoldl a) where
  mempty = empty
  mappend = (<>)

instance Foldable Unfoldl where
  {-# INLINE foldMap #-}
  foldMap inputMonoid = foldl' step mempty
    where
      step monoid input = mappend monoid (inputMonoid input)
  foldl = foldl'
  {-# INLINE foldl' #-}
  foldl' step init (Unfoldl run) = run step init

instance (Eq a) => Eq (Unfoldl a) where
  (==) left right = toList left == toList right

instance (Show a) => Show (Unfoldl a) where
  show = show . toList

instance IsList (Unfoldl a) where
  type Item (Unfoldl a) = a
  fromList list = foldable list
  toList = foldr (:) []

-- | Apply a Gonzalez fold
{-# INLINE fold #-}
fold :: Fold input output -> Unfoldl input -> output
fold (Fold step init extract) (Unfoldl run) = extract (run step init)

-- | Unlift a monadic unfold
{-# INLINE unfoldlM #-}
unfoldlM :: UnfoldlM Identity input -> Unfoldl input
unfoldlM (UnfoldlM runFoldM) = Unfoldl (\step init -> runIdentity (runFoldM (\a b -> return (step a b)) init))

-- | Lift a fold input mapping function into a mapping of unfolds
{-# INLINE mapFoldInput #-}
mapFoldInput :: (forall x. Fold b x -> Fold a x) -> Unfoldl a -> Unfoldl b
mapFoldInput newFold unfold = Unfoldl $ \step init -> fold (newFold (Fold step init id)) unfold

-- | Construct from any foldable
{-# INLINE foldable #-}
foldable :: (Foldable foldable) => foldable a -> Unfoldl a
foldable foldable = Unfoldl (\step init -> A.foldl' step init foldable)

-- | Filter the values given a predicate
{-# INLINE filter #-}
filter :: (a -> Bool) -> Unfoldl a -> Unfoldl a
filter test (Unfoldl run) = Unfoldl (\step -> run (\state element -> if test element then step state element else state))

-- | Ints in the specified inclusive range
{-# INLINE intsInRange #-}
intsInRange :: Int -> Int -> Unfoldl Int
intsInRange from to =
  Unfoldl $ \step init ->
    let loop !state int =
          if int <= to
            then loop (step state int) (succ int)
            else state
     in loop init from

-- | Associations of a map
{-# INLINE mapAssocs #-}
mapAssocs :: Map key value -> Unfoldl (key, value)
mapAssocs map =
  Unfoldl (\step init -> C.foldlWithKey' (\state key value -> step state (key, value)) init map)

-- | Associations of an intmap
{-# INLINE intMapAssocs #-}
intMapAssocs :: IntMap value -> Unfoldl (Int, value)
intMapAssocs intMap =
  Unfoldl (\step init -> D.foldlWithKey' (\state key value -> step state (key, value)) init intMap)

-- | Bytes of a bytestring
{-# INLINE byteStringBytes #-}
byteStringBytes :: ByteString -> Unfoldl Word8
byteStringBytes bs = Unfoldl (\step init -> ByteString.foldl' step init bs)

-- | Bytes of a short bytestring
{-# INLINE shortByteStringBytes #-}
shortByteStringBytes :: ShortByteString -> Unfoldl Word8
shortByteStringBytes (ShortByteString.SBS ba#) = primArray (PrimArray ba#)

-- | Elements of a prim array
{-# INLINE primArray #-}
primArray :: (Prim prim) => PrimArray prim -> Unfoldl prim
primArray ba = Unfoldl $ \f z -> foldlPrimArray' f z ba

-- | Elements of a prim array coming paired with indices
{-# INLINE primArrayWithIndices #-}
primArrayWithIndices :: (Prim prim) => PrimArray prim -> Unfoldl (Int, prim)
primArrayWithIndices pa = Unfoldl $ \step state ->
  let !size = sizeofPrimArray pa
      iterate index !state =
        if index < size
          then iterate (succ index) (step state (index, indexPrimArray pa index))
          else state
   in iterate 0 state
