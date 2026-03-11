{-# LANGUAGE RankNTypes #-}

module Control.Monad.Amb
       (
         -- * Overview
         -- $overview

         -- * Creating computations
         amb,
         aPartitionOfSize,
         aPartitionOf,
         aPermutationOf,
         aSplitOf,
         anIntegerBetween,
         aSubsetOf,
         aMemberOf,
         aBoolean,
         -- * Running computations
         isPossible,
         isPossibleT,
         isNecessary,
         isNecessaryT,
         allValues,
         allValuesT,
         oneValue,
         oneValueT,
         -- * Low-level internals
         tell',
         tellState,
         uponFailure,
         runAmbT,
         runAmbTI,
         ambCC,
         forEffects,
         -- * Types
         AmbT(..),
         AmbT',
         Amb,
         Amb',
         module Control.Applicative
       ) where
import Control.Monad.Cont
import Control.Monad.State.Lazy
import Control.Monad.Identity
import Control.Monad
import Control.Applicative
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid
#endif

-- $overview
--
-- A nondeterministic computation makes a series of choices which it
-- can then backtrack to. You can select between computations with
-- '(<|>)' or 'mplus' and abort a line of computation with 'empty' or
-- 'mzero'
--
-- As an example, here is a program which computes Pythagorean triples
-- of a certain size.
--
-- @
--import Control.Monad
--import Control.Monad.Amb
--
--pyTriple :: (Num t, Ord t) => t -> Amb r (t, t, t)
--pyTriple n = do a <- 'anIntegerBetween' 1 n
--                b <- 'anIntegerBetween' (a + 1) n
--                c <- 'anIntegerBetween' (b + 1) n
--                when (a*a + b*b /= c*c) 'empty'
--                return (a,b,c)
-- @
--
-- You can run this computation and ask for one or more of its
-- possible values.
--
-- >>> oneValue $ pyTriple 20
-- (3,4,5)
--
-- >>> allValues $ pyTriple 20
-- [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]

-- | @AmbT r m a@ is a computation whose current value is of type @a@
-- and which will ultimately return a value of type @r@. The same as
-- @ContT@.
data AmbT r m a = AmbT { 
  {- | From left to right:

       * the computation to run on failure
       
       * the continuation captured when making nondeterministic choices

       * record keeping of solutions found so far
 -}
  unAmbT ::
     StateT (AmbT r m r)
     (ContT r            
      (StateT [r] m))
     a }

type Amb r = AmbT r Identity
type AmbT' m a = forall r. AmbT r m a
type Amb' a = AmbT' Identity a

instance MonadTrans (AmbT r) where
    lift = AmbT . lift . lift . lift

instance Monad (AmbT r m) where
    AmbT a >>= b = AmbT $ a >>= unAmbT . b
    return = AmbT . return

instance MonadPlus (AmbT r m) where
  mzero = fail'
  mplus = either'

instance Functor (AmbT r m) where
  fmap = liftM

instance Applicative (AmbT r m) where
  pure = return
  (<*>) = ap

instance Alternative (AmbT r m) where
  (<|>) = either'
  empty = fail'

-- Internals

-- | call/cc lifted into the nondeterministic monad. This implements
-- the backtracking behaviour which allows Amb to try different code
-- paths and return multiple results.
ambCC :: ((a -> AmbT r m a1) -> AmbT r m a) -> AmbT r m a
ambCC f = AmbT $ callCC $ \k -> unAmbT $ f $ AmbT . k

-- | Run the nondeterministic computation. This is internal.
runAmbTI :: (Monad m) => AmbT a m a -> AmbT a m a -> m (a, [a])
runAmbTI (AmbT a) i = runStateT (runContT (evalStateT a i) return) []

-- | Run the nondeterministic computation. This is internal.
runAmbT :: (Monad m) => AmbT t m t -> m (t, [t])
runAmbT a = runAmbTI a (error "top-level fail")

-- | When the nondeterministic computation backtracks past this state,
-- execute this nondeterministic computation. Generally used to undo
-- side effects.
uponFailure :: AmbT r m a -> AmbT r m ()
uponFailure f = do
  old <- AmbT get
  AmbT $ put (f >> old)

-- | A helper to inject state into the backtracking stack
tellState :: (Monoid s, MonadState s m) => s -> m ()
tellState b = do
  a <- get
  put $ a `mappend` b

-- | A helper to inject state into the backtracking stack
tell' :: (Monad m) => [r] -> AmbT r m ()
tell' t = AmbT $ (lift $ lift $ tellState t)

-- | A low-level internal function which executes a nondeterministic
-- computation for its nondeterministic side-effects, such as its
-- ability to produce different results.
forEffects :: (Monad m) => ((t, [t]) -> r) -> (t1 -> AmbT t m t) -> AmbT t m t1 -> m r
forEffects f g e = f `liftM` runAmbTI (do ambCC $ \k -> do
                                            AmbT $ put (k undefined)
                                            v <- e
                                            g v)
                                      (return undefined)

-- Run nondeterministic computations

-- | Run a nondeterministic computation and return a result of that
-- computation.
oneValueT :: (Monad m) => AmbT b m b -> m b
oneValueT c = fst `liftM` runAmbT c

-- | Run a nondeterministic computation and return a result of that
-- computation.
oneValue :: Amb a a -> a
oneValue = runIdentity . oneValueT

-- | Run a nondeterministic computation and return a list of all
-- results that the computation can produce. Note that this function
-- is not lazy its result.
allValuesT :: (Monad m) => AmbT t m t -> m [t]
allValuesT = forEffects snd (\a -> tell' [a] >> empty)

-- | Run a nondeterministic computation and return a list of all
-- results that the computation can produce. Note that this function
-- is not lazy its result.
allValues :: Amb t t -> [t]
allValues = runIdentity . allValuesT

-- | Run a nondeterministic computation and return @True@
-- if any result is @True@, @False@ otherwise.
isPossibleT :: (Monad m) => AmbT Bool m Bool -> m Bool
isPossibleT = forEffects (([True] ==) . snd) (\a -> when (a == False) empty >> tell' [True] >> return undefined)

-- | Run a nondeterministic computation and return @True@
-- if any result is @True@, @False@ otherwise.
isPossible :: Amb Bool Bool -> Bool
isPossible = runIdentity . isPossibleT

-- | Run a nondeterministic computation and return @True@
-- if all possible results are @True@, @False@ otherwise.
isNecessaryT :: (Monad m) => AmbT Bool m Bool -> m Bool
isNecessaryT = forEffects (([] ==) . snd) (\a -> when (a == True) empty >> tell' [True] >> return undefined)

-- | Run a nondeterministic computation and return @True@
-- if all possible results are @True@, @False@ otherwise.
isNecessary :: Amb Bool Bool -> Bool
isNecessary = runIdentity . isNecessaryT

-- Generate nondeterministic computations

-- | Nondeterministically choose either of the two computations
either' :: AmbT r m b -> AmbT r m b -> AmbT r m b
either' a b = do r <- aBoolean
                 if r then a else b

-- | Terminate this branch of the computation.
fail' :: AmbT r m b
fail' = AmbT get >>= (\a -> a >> undefined)

-- | The most basic primitive that everything else is built out
-- of. Generates @True@ and @False@.
aBoolean :: AmbT r m Bool
aBoolean = ambCC $ \k -> do
             old <- AmbT get
             AmbT $ put (AmbT (put old) >> (k False) >> undefined)
             return True

-- | Generate each element of the given list.
aMemberOf :: [b] -> AmbT r m b
aMemberOf [] = empty
aMemberOf (x:xs) =  return x <|> aMemberOf xs

-- | Generate each subset of any size from the given list.
aSubsetOf :: [AmbT r m a] -> AmbT r m [a]
aSubsetOf [] = return []
aSubsetOf (x:xs) = aSubsetOf xs <|> liftM2 (:) x (aSubsetOf xs)

-- | Generate all numbers between the given bounds, inclusive.
anIntegerBetween :: (Monad m, Num b, Ord b) => b -> b -> AmbT r m b
anIntegerBetween i j | i > j = empty
                     | otherwise = either' (return i) (anIntegerBetween (i + 1) j) 

-- | Generate all splits of a list.
aSplitOf :: [a] -> AmbT r m ([a],[a])
aSplitOf l = loop [] l
    where loop x [] = return (x,[])
          loop x y@(y0:ys)  = either' (return (x,y)) (loop (x ++ [y0]) ys)

-- | Generate all permutations of a list.
aPermutationOf :: [a] -> AmbT r m [a]
aPermutationOf [] = return []
aPermutationOf (l0:ls) = do (s1,s2) <- (aPermutationOf ls >>= aSplitOf)
                            return $ s1 ++ (l0:s2)

-- | Generate all partitions of this list.
aPartitionOf :: (Eq t, Monad m) => [t] -> AmbT r m [[t]]
aPartitionOf [] = return []
aPartitionOf (x:xs) = do y <- aPartitionOf xs
                         either' (return ([x]:y))
                                 (do z <- aMemberOf y
                                     return ((x:z) : filter (z /=) y))

-- | Generate all partitions of a given size of this list.
aPartitionOfSize :: (Eq a, Monad m) => Int -> [a] -> AmbT r m [[a]]
aPartitionOfSize 0 _ = error "Can't create a partition of size 0"
aPartitionOfSize k l | length l < k = empty
                     | otherwise = loop l
    where loop x@(x0:xs) | length x == k = return $ map (:[]) x
                         | otherwise = do y <- loop xs
                                          z <- aMemberOf y
                                          return ((x0:z):filter (z /=) y)
          loop [] = empty

-- | Just for fun. This is McCarthy's @amb@ operator and is a synonym
-- for @aMemberOf@.
amb :: [b] -> AmbT r m b
amb = aMemberOf
