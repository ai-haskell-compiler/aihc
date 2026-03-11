{-|

Element-agnostic grouping utilities for @pipes@

See "Pipes.Group.Tutorial" for an extended tutorial

Some type signatures below refer to the aliases below, which are not used in
this library, but are included to simplify the documentation.

@
type Groups         a m x = 'FreeT' ('Producer' a m) m x
type Splitter       a m x = 'Producer' a m x -> Groups a m x
type Transformation a m x = Groups a m x -> Groups a m x
type Joiner         a m x = Groups a m x -> 'Producer' a m x
@
-}

{-# LANGUAGE RankNTypes #-}

module Pipes.Group (
    -- * Lenses
    groups,
    groupsBy,
    groupsBy',
    chunksOf,

    -- * Transformations
    takes,
    takes',
    drops,
    maps,
    individually,

    -- * Joiners
    concats,
    intercalates,

    -- * Folds
    -- $folds
    folds,
    foldsM,

    -- * Re-exports
    -- $reexports
    module Control.Monad.Trans.Class,
    module Control.Monad.Trans.Free,
    module Pipes
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free (FreeF(Pure, Free), FreeT(FreeT, runFreeT))
import qualified Control.Monad.Trans.Free as F
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Pipes (Producer, yield, next)
import Pipes.Parse (span, splitAt)
import qualified Pipes as P

import Prelude hiding (span, splitAt)

type Lens a' a b' b = forall f . Functor f => (b' -> f b) -> (a' -> f a)
type Setter a' a b' b = (b' -> Identity b) -> (a' -> Identity a)

(^.) :: a -> ((b -> Constant b b) -> (a -> Constant b a)) -> b
a ^. lens = getConstant (lens Constant a)

{-| 'groupsBy' splits a 'Producer' into a 'FreeT' of 'Producer's grouped using
    the given equality predicate

@
      groupsBy p  :: Monad m => Lens' ('Producer' a m x) (Groups a m x)
view (groupsBy p) :: Monad m => Splitter a m x
set  (groupsBy p) :: Monad m => Groups a m x -> 'Producer' a m x -> 'Producer' a m x
over (groupsBy p) :: Monad m => Transformation a m x -> 'Producer' a m x -> 'Producer' a m x
@

>>> import Lens.Family (view)
>>> import Pipes (yield, each)
>>> import Pipes.Prelude (toList)
>>> (toList . intercalates (yield '|') . view (groupsBy (==))) (each "12233345")
"1|22|333|4|5"
-}
groupsBy
    :: Monad m
    => (a' -> a' -> Bool)
    -> Lens (Producer a' m x) (Producer a m x) (FreeT (Producer a' m) m x) (FreeT (Producer a m) m x)
groupsBy equals k p0 = fmap concats (k (_groupsBy p0))
  where
--  _groupsBy :: Monad m => Producer a m r -> FreeT (Producer a m) m r
    _groupsBy p = FreeT $ do
        x <- next p
        return $ case x of
            Left   r      -> Pure r
            Right (a, p') -> Free $
                fmap _groupsBy (yield a >> (p' ^. span (equals a)))
{-# INLINABLE groupsBy #-}

{-| `groupsBy'` splits a 'Producer' into a 'FreeT' of 'Producer's grouped using
    the given equality predicate

    This differs from `groupsBy` by comparing successive elements for equality
    instead of comparing each element to the first member of the group

>>> import Lens.Family (view)
>>> import Pipes (yield, each)
>>> import Pipes.Prelude (toList)
>>> let cmp c1 c2 = succ c1 == c2
>>> (toList . intercalates (yield '|') . view (groupsBy' cmp)) (each "12233345")
"12|23|3|345"
>>> (toList . intercalates (yield '|') . view (groupsBy  cmp)) (each "12233345")
"122|3|3|34|5"

@
      groupsBy' p  :: Monad m => Lens' ('Producer' a m x) (Groups a m x)
view (groupsBy' p) :: Monad m => Splitter a m x
set  (groupsBy' p) :: Monad m => Groups a m x -> 'Producer' a m x -> 'Producer' a m x
over (groupsBy' p) :: Monad m => Transformation a m x -> 'Producer' a m x -> 'Producer' a m x
@
-}
groupsBy'
    :: Monad m
    => (a' -> a' -> Bool) -> Lens (Producer a' m x) (Producer a m x) (FreeT (Producer a' m) m x) (FreeT (Producer a m) m x)
groupsBy' equals k p0 = fmap concats (k (_groupsBy p0))
  where
--  _groupsBy :: Monad m => Producer a m r -> FreeT (Producer a m) m r
    _groupsBy p = FreeT $ do
        x <- next p
        return $ case x of
            Left   r      -> Pure r
            Right (a, p') -> Free (fmap _groupsBy (loop0 (yield a >> p')))

--  loop0
--      :: Monad m
--      => Producer a m r
--      -> Producer a m (Producer a m r)
    loop0 p1 = do
        x <- lift (next p1)
        case x of
            Left   r      -> return (return r)
            Right (a2, p2) -> do
                yield a2
                let loop1 a p = do
                        y <- lift (next p)
                        case y of
                            Left   r      -> return (return r)
                            Right (a', p') ->
                                if equals a a'
                                then do
                                    yield a'
                                    loop1 a' p'
                                else return (yield a' >> p')
                loop1 a2 p2
{-# INLINABLE groupsBy' #-}

{-| Like 'groupsBy', where the equality predicate is ('==')

@
     groups :: Monad m => Lens' ('Producer' a m x) (Groups a m x)
view groups :: Monad m => Splitter a m x
set  groups :: Monad m => Groups a m x -> 'Producer' a m x -> 'Producer' a m x
over groups :: Monad m => Transformation a m x -> 'Producer' a m x -> 'Producer' a m x
@

>>> import Lens.Family (view)
>>> import Pipes (yield, each)
>>> import Pipes.Prelude (toList)
>>> (toList . intercalates (yield '|') . view groups) (each "12233345")
"1|22|333|4|5"
-}
groups :: (Monad m, Eq a') => Lens (Producer a' m x) (Producer a m x) (FreeT (Producer a' m) m x) (FreeT (Producer a m) m x)
groups = groupsBy (==)
{-# INLINABLE groups #-}

{-| 'chunksOf' is an splits a 'Producer' into a 'FreeT' of 'Producer's of fixed
    length

@
      chunksOf n  :: Monad m => Lens' ('Producer' a m x) (Groups a m x)
view (chunksOf n) :: Monad m => Splitter a m x
set  (chunksOf n) :: Monad m => Groups a m x -> 'Producer' a m x -> 'Producer' a m x
over (chunksOf n) :: Monad m => Transformation a m x -> 'Producer' a m x -> 'Producer' a m x
@

>>> import Lens.Family (view)
>>> import Pipes (yield, each)
>>> import Pipes.Prelude (toList)
>>> (toList . intercalates (yield '|') . view (chunksOf 3)) (each "12233345")
"122|333|45"
-}
chunksOf
    :: Monad m => Int -> Lens (Producer a' m x) (Producer a m x) (FreeT (Producer a' m) m x) (FreeT (Producer a m) m x)
chunksOf n0 k p0 = fmap concats (k (_chunksOf p0))
  where
--  _chunksOf :: Monad m => Producer a m x -> FreeT (Producer a m) m x
    _chunksOf p = FreeT $ do
        x <- next p
        return $ case x of
            Left   r      -> Pure r
            Right (a, p') -> Free $ fmap _chunksOf ((yield a >> p')^.splitAt n0)
{-# INLINABLE chunksOf #-}

-- | Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer'
--
-- @
-- concats :: Monad m => Joiner a m x
-- @
concats :: Monad m => FreeT (Producer a m) m x -> Producer a m x
concats = go
  where
    go f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                go f'
{-# INLINABLE concats #-}

{-| Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer' by
    intercalating a 'Producer' in between them

@
intercalates :: Monad m => 'Producer' a m () -> Joiner a m x
@
-}
intercalates
    :: Monad m => Producer a m () -> FreeT (Producer a m) m x -> Producer a m x
intercalates sep = go0
  where
    go0 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                go1 f'
    go1 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                sep
                f' <- p
                go1 f'
{-# INLINABLE intercalates #-}

{-| @(takes n)@ only keeps the first @n@ functor layers of a 'FreeT'

@
takes :: Monad m => Int -> Groups a m () -> Groups a m ()
@

>>> import Lens.Family (view)
>>> import Pipes (yield, each)
>>> import Pipes.Prelude (toList)
>>> (toList . intercalates (yield '|') . takes 3 . view groups) (each "12233345")
"1|22|333"
-}
takes :: (Functor f, Monad m) => Int -> FreeT f m () -> FreeT f m ()
takes = go
  where
    go n f =
        if (n > 0)
        then FreeT $ do
            x <- runFreeT f
            case x of
                Pure () -> return (Pure ())
                Free w  -> return (Free (fmap (go $! n - 1) w))
        else return ()
{-# INLINABLE takes #-}

{-| @(takes' n)@ only keeps the first @n@ 'Producer's of a 'FreeT'

    'takes'' differs from 'takes' by draining unused 'Producer's in order
    to preserve the return value.  This makes it a suitable argument for 'maps'.

@
takes' :: Monad m => Int -> Transformation a m x
@
-}
takes' :: Monad m => Int -> FreeT (Producer a m) m x -> FreeT (Producer a m) m x
takes' = go0
  where
    go0 n f = FreeT $
        if (n > 0)
        then do
            x <- runFreeT f
            return $ case x of
                Pure r -> Pure r
                Free p -> Free $ fmap (go0 $! n - 1) p
        else go1 f
    go1 f = do
        x <- runFreeT f
        case x of
            Pure r -> return (Pure r)
            Free p -> do
                f' <- P.runEffect (P.for p P.discard)
                go1 f'
{-# INLINABLE takes' #-}

{-| @(drops n)@ peels off the first @n@ 'Producer' layers of a 'FreeT'

@
drops :: Monad m => Int -> Transformation a m x
@

>>> import Lens.Family (view)
>>> import Pipes (yield, each)
>>> import Pipes.Prelude (toList)
>>> (toList . intercalates (yield '|') . drops 3 . view groups) (each "12233345")
"4|5"

    __Use carefully__: the peeling off is not free.   This runs the first @n@
    layers, just discarding everything they produce.
-}
drops :: Monad m => Int -> FreeT (Producer a m) m x -> FreeT (Producer a m) m x
drops = go
  where
    go n ft
        | n <= 0 = ft
        | otherwise = FreeT $ do
            ff <- runFreeT ft
            case ff of
                Pure _ -> return ff
                Free f -> do
                    ft' <- P.runEffect $ P.for f P.discard
                    runFreeT $ go (n-1) ft'
{-# INLINABLE drops #-}

{-| Transform each individual functor layer of a 'FreeT'

    You can think of this as:

> maps
>     :: (forall r . Producer a m r -> Producer b m r)
>     -> FreeT (Producer a m) m x -> FreeT (Producer b m) m x

    This is just a synonym for 'F.transFreeT'
-}
maps
    :: (Monad m, Functor g)
    => (forall r . f r -> g r) -> FreeT f m x -> FreeT g m x
maps = F.transFreeT
{-# INLINABLE maps #-}

{-| Lens to transform each individual functor layer of a 'FreeT'. (@over
    'individually'@) is equivalent to 'maps', but with a less general type.

@
type Group a m x = 'Producer' a m (Groups a m x)

set  individually :: Monad m => Group a m x -> Transformation a m x
over individually :: Monad m => (Group a m x -> Group a m x) -> Transformation a m x
@
-}
individually
    :: (Monad m, Functor g)
    => Setter (FreeT f m x) (FreeT g m x) (f (FreeT f m x)) (g (FreeT f m x))
individually nat f0 = Identity (go f0)
  where
    nat' = runIdentity . nat
    go f = FreeT $ do
        x <- runFreeT f
        return $ case x of
            Pure r -> Pure r
            Free w -> Free (fmap go (nat' w))
{-# INLINABLE individually #-}

{- $folds
    These folds are designed to be compatible with the @foldl@ library.  See
    the 'Control.Foldl.purely' and 'Control.Foldl.impurely' functions from that
    library for more details.

    For example, to count the number of 'Producer' layers in a 'FreeT', you can
    write:

> import Control.Applicative (pure)
> import qualified Control.Foldl as L
> import Pipes.Group
> import qualified Pipes.Prelude as P
>
> count :: Monad m => FreeT (Producer a m) m () -> m Int
> count = P.sum . L.purely folds (pure 1)
-}
{-| Fold each 'Producer' of a 'FreeT'

@
'Control.Foldl.purely' folds :: Monad m => 'Control.Foldl.Fold' a b -> Groups a m r -> 'Producer' b m r
@
-}
folds
    :: Monad m
    => (x -> a -> x)
    -- ^ Step function
    -> x
    -- ^ Initial accumulator
    -> (x -> b)
    -- ^ Extraction function
    -> FreeT (Producer a m) m r
    -- ^
    -> Producer b m r
folds step begin done = go
  where
    go f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                (f', b) <- lift (fold p begin)
                yield b
                go f'

    fold p x = do
        y <- next p
        case y of
            Left   f      -> return (f, done x)
            Right (a, p') -> fold p' $! step x a
{-# INLINABLE folds #-}

{-| Fold each 'Producer' of a 'FreeT', monadically

@
'Control.Foldl.impurely' foldsM :: Monad m => 'Control.Foldl.FoldM' a b -> Groups a m r -> 'Producer' b m r
@
-}
foldsM
    :: Monad m
    => (x -> a -> m x)
    -- ^ Step function
    -> m x
    -- ^ Initial accumulator
    -> (x -> m b)
    -- ^ Extraction function
    -> FreeT (Producer a m) m r
    -- ^
    -> Producer b m r
foldsM step begin done = go
  where
    go f = do
        y <- lift (runFreeT f)
        case y of
            Pure r -> return r
            Free p -> do
                (f', b) <- lift $ do
                    x <- begin
                    foldM p x
                yield b
                go f'

    foldM p x = do
        y <- next p
        case y of
            Left   f      -> do
                b <- done x
                return (f, b)
            Right (a, p') -> do
                x' <- step x a
                foldM p' $! x'
{-# INLINABLE foldsM #-}
{- $reexports
    "Control.Monad.Trans.Class" re-exports 'lift'.

    "Control.Monad.Trans.Free" re-exports 'FreeF' and 'FreeT'

    "Pipes" re-exports 'Producer', 'yield', and 'next'.
-}
