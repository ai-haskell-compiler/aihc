{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

module DeferredFolds.Defs.UnfoldlM where

import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Short.Internal as ShortByteString
import DeferredFolds.Prelude hiding (foldM, mapM_)
import qualified DeferredFolds.Prelude as A
import DeferredFolds.Types

deriving instance (Functor m) => Functor (UnfoldlM m)

instance (Monad m) => Applicative (UnfoldlM m) where
  pure x =
    UnfoldlM (\step init -> step init x)
  (<*>) = ap

instance (Monad m) => Alternative (UnfoldlM m) where
  empty =
    UnfoldlM (const return)
  {-# INLINE (<|>) #-}
  (<|>) (UnfoldlM left) (UnfoldlM right) =
    UnfoldlM (\step init -> left step init >>= right step)

instance (Monad m) => Monad (UnfoldlM m) where
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (UnfoldlM left) rightK =
    UnfoldlM $ \step init ->
      let newStep output x =
            case rightK x of
              UnfoldlM right ->
                right step output
       in left newStep init

instance (Monad m) => MonadPlus (UnfoldlM m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans UnfoldlM where
  lift m = UnfoldlM (\step init -> m >>= step init)

instance (Monad m) => Semigroup (UnfoldlM m a) where
  (<>) = (<|>)

instance (Monad m) => Monoid (UnfoldlM m a) where
  mempty = empty
  mappend = (<>)

instance Foldable (UnfoldlM Identity) where
  {-# INLINE foldMap #-}
  foldMap inputMonoid = foldl' step mempty
    where
      step monoid input = mappend monoid (inputMonoid input)
  foldl = foldl'
  {-# INLINE foldl' #-}
  foldl' step init (UnfoldlM run) =
    runIdentity (run identityStep init)
    where
      identityStep state input = return (step state input)

instance (Eq a) => Eq (UnfoldlM Identity a) where
  (==) left right = toList left == toList right

instance (Show a) => Show (UnfoldlM Identity a) where
  show = show . toList

instance IsList (UnfoldlM Identity a) where
  type Item (UnfoldlM Identity a) = a
  fromList list = foldable list
  toList = foldr (:) []

-- | Check whether it's empty
{-# INLINE null #-}
null :: (Monad m) => UnfoldlM m input -> m Bool
null (UnfoldlM run) = run (\_ _ -> return False) True

-- | Perform a monadic strict left fold
{-# INLINE foldlM' #-}
foldlM' :: (Monad m) => (output -> input -> m output) -> output -> UnfoldlM m input -> m output
foldlM' step init (UnfoldlM run) =
  run step init

-- | A more efficient implementation of mapM_
{-# INLINE mapM_ #-}
mapM_ :: (Monad m) => (input -> m ()) -> UnfoldlM m input -> m ()
mapM_ step = foldlM' (const step) ()

-- | Same as 'mapM_' with arguments flipped
{-# INLINE forM_ #-}
forM_ :: (Monad m) => UnfoldlM m input -> (input -> m ()) -> m ()
forM_ = flip mapM_

-- | Apply a Gonzalez fold
{-# INLINE fold #-}
fold :: Fold input output -> UnfoldlM Identity input -> output
fold (Fold step init extract) = extract . foldl' step init

-- | Apply a monadic Gonzalez fold
{-# INLINE foldM #-}
foldM :: (Monad m) => FoldM m input output -> UnfoldlM m input -> m output
foldM (FoldM step init extract) view =
  do
    initialState <- init
    finalState <- foldlM' step initialState view
    extract finalState

-- | Lift a fold input mapping function into a mapping of unfolds
{-# INLINE mapFoldMInput #-}
mapFoldMInput :: (Monad m) => (forall x. FoldM m b x -> FoldM m a x) -> UnfoldlM m a -> UnfoldlM m b
mapFoldMInput newFoldM unfoldM = UnfoldlM $ \step init -> foldM (newFoldM (FoldM step (return init) return)) unfoldM

-- | Construct from any foldable
{-# INLINE foldable #-}
foldable :: (Monad m, Foldable foldable) => foldable a -> UnfoldlM m a
foldable foldable = UnfoldlM (\step init -> A.foldlM step init foldable)

-- | Construct from a specification of how to execute a left-fold
{-# INLINE foldlRunner #-}
foldlRunner :: (Monad m) => (forall x. (x -> a -> x) -> x -> x) -> UnfoldlM m a
foldlRunner run = UnfoldlM (\stepM state -> run (\stateM a -> stateM >>= \state -> stepM state a) (return state))

-- | Construct from a specification of how to execute a right-fold
{-# INLINE foldrRunner #-}
foldrRunner :: (Monad m) => (forall x. (a -> x -> x) -> x -> x) -> UnfoldlM m a
foldrRunner run = UnfoldlM (\stepM -> run (\x k z -> stepM z x >>= k) return)

{-# INLINEABLE unfoldr #-}
unfoldr :: (Monad m) => Unfoldr a -> UnfoldlM m a
unfoldr (Unfoldr unfoldr) = foldrRunner unfoldr

-- | Filter the values given a predicate
{-# INLINE filter #-}
filter :: (Monad m) => (a -> m Bool) -> UnfoldlM m a -> UnfoldlM m a
filter test (UnfoldlM run) = UnfoldlM (\step -> run (\state element -> test element >>= bool (return state) (step state element)))

-- | Ints in the specified inclusive range
{-# INLINE intsInRange #-}
intsInRange :: (Monad m) => Int -> Int -> UnfoldlM m Int
intsInRange from to =
  UnfoldlM $ \step init ->
    let loop !state int =
          if int <= to
            then do
              newState <- step state int
              loop newState (succ int)
            else return state
     in loop init from

-- | TVar contents
{-# INLINE tVarValue #-}
tVarValue :: TVar a -> UnfoldlM STM a
tVarValue var = UnfoldlM $ \step state -> do
  a <- readTVar var
  step state a

-- | Change the base monad using invariant natural transformations
{-# INLINE hoist #-}
hoist :: (forall a. m a -> n a) -> (forall a. n a -> m a) -> UnfoldlM m a -> UnfoldlM n a
hoist trans1 trans2 (UnfoldlM unfold) = UnfoldlM $ \step init ->
  trans1 (unfold (\a b -> trans2 (step a b)) init)

-- | Bytes of a bytestring
{-# INLINEABLE byteStringBytes #-}
byteStringBytes :: ByteString -> UnfoldlM IO Word8
byteStringBytes (ByteString.PS fp off len) =
  UnfoldlM $ \step init ->
    withForeignPtr fp $ \ptr ->
      let endPtr = plusPtr ptr (off + len)
          iterate !state !ptr =
            if ptr == endPtr
              then return state
              else do
                x <- peek ptr
                newState <- step state x
                iterate newState (plusPtr ptr 1)
       in iterate init (plusPtr ptr off)

-- | Bytes of a short bytestring
{-# INLINE shortByteStringBytes #-}
shortByteStringBytes :: (Monad m) => ShortByteString -> UnfoldlM m Word8
shortByteStringBytes (ShortByteString.SBS ba#) = primArray (PrimArray ba#)

-- | Elements of a prim array
{-# INLINE primArray #-}
primArray :: (Monad m, Prim prim) => PrimArray prim -> UnfoldlM m prim
primArray pa = UnfoldlM $ \f z -> foldlPrimArrayM' f z pa

-- | Elements of a prim array coming paired with indices
{-# INLINE primArrayWithIndices #-}
primArrayWithIndices :: (Monad m, Prim prim) => PrimArray prim -> UnfoldlM m (Int, prim)
primArrayWithIndices pa = UnfoldlM $ \step state ->
  let !size = sizeofPrimArray pa
      iterate index !state =
        if index < size
          then do
            newState <- step state (index, indexPrimArray pa index)
            iterate (succ index) newState
          else return state
   in iterate 0 state
