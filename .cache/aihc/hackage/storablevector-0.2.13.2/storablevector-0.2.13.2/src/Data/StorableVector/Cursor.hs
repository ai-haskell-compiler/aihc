{-# LANGUAGE ExistentialQuantification #-}
{- |
Simulate a list with strict elements by a more efficient array structure.
-}
module Data.StorableVector.Cursor (
   Vector,
   create,
   unfoldrNTerm,
   unfoldrN,
   pack,
   cons,
   zipWith,
   zipNWith,
   whileM,
   switchL,
   viewL,
   foldr,
   unpack,
   null,
   drop,
   take,
   filter,
   ) where

import Control.Exception        (assert, )
import Control.Monad.Trans.State (StateT(StateT), runStateT, )
import Data.IORef               (IORef, newIORef, readIORef, writeIORef, )

import Foreign.Storable         (Storable(peekElemOff, pokeElemOff))
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr, )
-- import Foreign.Ptr              (Ptr)
import Data.StorableVector.Memory (mallocForeignPtrArray, )

import Control.Monad            (when)
import Data.Maybe               (isNothing)

import qualified System.Unsafe as Unsafe

import qualified Data.List.HT as ListHT
import Data.Tuple.HT (mapSnd)

import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Function (const, ($))
import Data.Tuple (uncurry)
import Data.Bool (Bool(True,False), (||))
import Data.Ord ((<), (<=), (>=), min, max)
import Data.Eq ((==))
import Control.Monad (Monad, return, fmap, (>>))
import Text.Show (Show, showsPrec)
import Prelude (IO, pred, succ, ($!), Int, (+), (-))


{-
ToDo:
I think that the state should be Storable as well
and that the IORef should be replaced by a ForeignPtr.
I hope that this is more efficient.
With this restriction @s@ cannot be e.g. a function type
but this would kill performance anyway.
Functions that need this flexibility may fall back to other data structures
(lists or chunky StorableVectors) and convert to the Cursor structure later.
-}
-- | Cf. StreamFusion  Data.Stream
data Generator a =
   forall s. -- Seq s =>
      Generator
         !(StateT s Maybe a)  -- compute next value
         {-# UNPACK #-}
         !(IORef (Maybe s))   -- current state

{- |
This simulates a
@ data StrictList a = Elem !a (StrictList a) | End @
by an array and some unsafe hacks.
-}
data Buffer a =
   Buffer {
       memory :: {-# UNPACK #-} !(ForeignPtr a),
       _size  :: {-# UNPACK #-} !Int,  -- size of allocated memory, I think I only need it for debugging
       _gen   ::                !(Generator a),  -- we need this indirection for the existential type in Generator
       cursor :: {-# UNPACK #-} !(IORef Int)
   }

{- |
Vector is a part of a buffer.
-}
data Vector a =
   Vector {
       buffer :: {-# UNPACK #-} !(Buffer a),
       start  :: {-# UNPACK #-} !Int,   -- invariant: start <= cursor
       maxLen :: {-# UNPACK #-} !Int    -- invariant: start+maxLen <= size buffer
   }


-- * construction

{-# INLINE create #-}
create :: (Storable a) => Int -> Generator a -> Buffer a
create l g = Unsafe.performIO (createIO l g)

-- | Wrapper of mallocForeignPtrArray.
createIO :: (Storable a) => Int -> Generator a -> IO (Buffer a)
createIO l g = do
    fp <- mallocForeignPtrArray l
    cur <- newIORef 0
    return $! Buffer fp l g cur


{- |
@ unfoldrNTerm 20  (\n -> Just (n, succ n)) 'a' @
-}
unfoldrNTerm :: (Storable b) =>
   Int -> (a -> Maybe (b, a)) -> a -> Vector b
unfoldrNTerm l f x0 =
   Unsafe.performIO (unfoldrNTermIO l f x0)

unfoldrNTermIO :: (Storable b) =>
   Int -> (a -> Maybe (b, a)) -> a -> IO (Vector b)
unfoldrNTermIO l f x0 =
   do ref <- newIORef (Just x0)
      buf <- createIO l (Generator (StateT f) ref)
      return (Vector buf 0 l)

unfoldrN :: (Storable b) =>
   Int -> (a -> Maybe (b, a)) -> a -> (Vector b, Maybe a)
unfoldrN l f x0 =
   Unsafe.performIO (unfoldrNIO l f x0)

unfoldrNIO :: (Storable b) =>
   Int -> (a -> Maybe (b, a)) -> a -> IO (Vector b, Maybe a)
unfoldrNIO l f x0 =
   do ref <- newIORef (Just x0)
      buf <- createIO l (Generator (StateT f) ref)
      s <- Unsafe.interleaveIO $
             do evaluateToIO l buf
                readIORef ref
      return (Vector buf 0 l, s)
{-
unfoldrNIO :: (Storable b) =>
   Int -> (a -> Maybe (b, a)) -> a -> IO (Vector b, Maybe a)
unfoldrNIO l f x0 =
   do y <- unfoldrNTermIO l f x0
--      evaluateTo l y
      let (Generator _ ref) = gen (buffer y)
      s <- readIORef ref
      return (y, s)

Data/StorableVector/Cursor.hs:98:10:
    My brain just exploded.
    I can't handle pattern bindings for existentially-quantified constructors.
    In the binding group
        (Generator _ ref) = gen (buffer y)
    In the definition of `unfoldrNIO':
        unfoldrNIO l f x0
                     = do
                         y <- unfoldrNTermIO l f x0
                         let (Generator _ ref) = gen (buffer y)
                         s <- readIORef ref
                         return (y, s)
-}


{-
unfoldrN :: (Storable b) =>
   Int -> (a -> Maybe (b, a)) -> a -> (Vector b, Maybe a)
unfoldrN i f x0 =
   let y = unfoldrNTerm i f x0
   in  (y, getFinalState y)

getFinalState :: (Storable b) =>
   Vector b -> Maybe a
getFinalState y =
   Unsafe.performIO $
      ...
-}


{-# INLINE pack #-}
pack :: (Storable a) => Int -> [a] -> Vector a
pack n = unfoldrNTerm n ListHT.viewL


{-# INLINE cons #-}
{- |
This is expensive and should not be used to construct lists iteratively!
A recursion-enabling 'cons' would be 'consN'
that allocates a buffer of given size,
initializes the leading cell and sets the buffer pointer to the next cell.
-}
cons :: Storable a =>
   a -> Vector a -> Vector a
cons x xs =
   unfoldrNTerm (succ (maxLen xs))
      (\(mx0,xs0) ->
          fmap (mapSnd ((,) Nothing)) $
          maybe
             (viewL xs0)
             (\x0 -> Just (x0, xs0))
             mx0) $
   (Just x, xs)


{-# INLINE zipWith #-}
zipWith :: (Storable a, Storable b, Storable c) =>
   (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f ps0 qs0 =
   zipNWith (min (maxLen ps0) (maxLen qs0)) f ps0 qs0

-- zipWith f ps qs = pack $ List.zipWith f (unpack ps) (unpack qs)

{-# INLINE zipNWith #-}
zipNWith :: (Storable a, Storable b, Storable c) =>
   Int -> (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipNWith n f ps0 qs0 =
   unfoldrNTerm n
      (\(ps,qs) ->
         do (ph,pt) <- viewL ps
            (qh,qt) <- viewL qs
            return (f ph qh, (pt,qt)))
      (ps0,qs0)
{-
let f2 = zipNWith 15 (+) f0 f1; f1 = cons 1 f2; f0 = cons (0::Int) f1 in f0

*Data.StorableVector.Cursor> let xs = unfoldrNTerm 20  (\n -> Just (n, succ n)) (0::Int)
*Data.StorableVector.Cursor> let ys = unfoldrNTerm 20  (\n -> Just (n, 2*n)) (1::Int)
*Data.StorableVector.Cursor> zipWith (+) xs ys
-}




-- * inspection

-- | evaluate next value in a buffer
advanceIO :: Storable a =>
   Buffer a -> IO (Maybe a)
advanceIO (Buffer p sz (Generator n s) cr) =
   do c <- readIORef cr
      assert (c < sz) $
         do writeIORef cr (succ c)
            ms <- readIORef s
            case ms of
               Nothing -> return Nothing
               Just s0 ->
                  case runStateT n s0 of
                     Nothing ->
                        writeIORef s Nothing >>
                        return Nothing
                     Just (a,s1) ->
                        writeIORef s (Just s1) >>
                        withForeignPtr p (\q -> pokeElemOff q c a) >>
                        return (Just a)

{-
It is tempting to turn this into a simple loop without the IORefs.
This could be compiled to an efficient strict loop,
but it would fail if the vector content depends on its own,
like in @fix (consN 1000 'a')@.
-}
-- | evaluate all values up to a given position
evaluateToIO :: Storable a =>
   Int -> Buffer a -> IO ()
evaluateToIO l buf@(Buffer _p _sz _g cr) =
   whileM
      (fmap (<l) (readIORef cr))
      (advanceIO buf)

whileM :: Monad m => m Bool -> m a -> m ()
whileM p f =
   let recourse =
          do b <- p
             when b (f >> recourse)
   in  recourse

{-# INLINE switchL #-}
switchL :: Storable a => b -> (a -> Vector a -> b) -> Vector a -> b
switchL n j v = maybe n (uncurry j) (viewL v)


{-
If it returns False the list can be empty anyway.
-}
_obviousNullIO :: Vector a -> IO Bool
_obviousNullIO (Vector (Buffer _ _ (Generator _ s) _) _ ml) =
   assert (ml >= 0) $
   do b <- readIORef s
      return (ml == 0 || isNothing b)

{-
_obviousNullIO :: Vector a -> IO Bool
_obviousNullIO (Vector (Buffer _ sz (Generator _ s) _) st _) =
   do b <- readIORef s
      return (st >= sz || isNothing b)
-}
--   assert (l >= 0) $ l <= 0

{-# INLINE viewL #-}
viewL :: Storable a => Vector a -> Maybe (a, Vector a)
viewL v = Unsafe.performIO (viewLIO v)

{-# INLINE viewLIO #-}
viewLIO :: Storable a => Vector a -> IO (Maybe (a, Vector a))
viewLIO (Vector buf st ml) =
   do c <- readIORef (cursor buf)
      fmap (fmap (\a -> (a, Vector buf (succ st) (pred ml)))) $
        assert (st <= c) $
           if st == c
             then advanceIO buf
             else fmap Just $ withForeignPtr (memory buf) (\p -> peekElemOff p st)


{-# INLINE foldr #-}
foldr :: (Storable a) => (a -> b -> b) -> b -> Vector a -> b
foldr k z =
   let recourse = switchL z (\h t -> k h (recourse t))
   in  recourse

-- | /O(n)/ Converts a 'Vector a' to a '[a]'.
{-# INLINE unpack #-}
unpack :: (Storable a) => Vector a -> [a]
unpack = foldr (:) []


instance (Show a, Storable a) => Show (Vector a) where
   showsPrec p x = showsPrec p (unpack x)


{-# INLINE null #-}
{-
This can hardly be simplified.
In order to check the list for emptiness,
we have to try to calculate the next element.
It is not enough to check whether the state is Nothing,
because when we try to compute the next value, this can be Nothing.
-}
null :: Storable a => Vector a -> Bool
null = switchL True (const (const False))


{-
toVector :: Storable a => Vector a -> VS.Vector a
toVector v =
   VS.Cons (memory (buffer v)) ()
-}

-- length

drop :: (Storable a) => Int -> Vector a -> Vector a
drop n v = Unsafe.performIO $ dropIO n v

dropIO :: (Storable a) => Int -> Vector a -> IO (Vector a)
dropIO n v =
   assert (n>=0) $
    let pos = min (maxLen v) (start v + n)
    in  do evaluateToIO pos (buffer v)
           return (Vector (buffer v) pos (max 0 (maxLen v - n)))

take :: (Storable a) => Int -> Vector a -> Vector a
take n v =
   assert (n>=0) $
   v{maxLen = min n (maxLen v)}

{-
let x = unfoldrNTerm 10 (\c -> Just (c,succ c)) 'a'
let x = unfoldrNTerm 10 (\c -> Just (sum [c..100000],succ c)) (0::Int)
-}


{- |
For the sake of laziness it may allocate considerably more memory than needed,
if it filters out very much.
-}
{-# INLINE filter #-}
filter :: (Storable a) => (a -> Bool) -> Vector a -> Vector a
filter p xs0 =
   unfoldrNTerm (maxLen xs0)
      (let recourse = switchL Nothing (\x xs -> if p x then Just (x,xs) else recourse xs)
       in  recourse)
      xs0
