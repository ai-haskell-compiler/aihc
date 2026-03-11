module Control.Monad.Exception.Asynchronous.Lazy (
   Exceptional(..),
   pure,
   broken,
   fromSynchronous,
   fromSynchronousNull,
   fromSynchronousMonoid,
   toSynchronous,
   throw,
   throwMonoid,
   eatNothing,
   zipWith,
   append,
   continue,
   maybeAbort,
   force,
   mapException,
   mapExceptional,
   simultaneousBind,
   simultaneousBindM,
   sequenceF,
   traverse,
   sequenceA,
   mapM,
   sequence,
   swapToSynchronousAsynchronous,
   swapToAsynchronousSynchronous,

   ExceptionalT(..),
   fromSynchronousT,
   fromSynchronousMonoidT,
   forceT,
   mapExceptionT,
   mapExceptionalT,
   throwMonoidT,
   eatNothingT,
   bindT,
   manySynchronousT,
   manyMonoidT,
   processToSynchronousT_,

   appendM,
   continueM,
   ) where

import qualified Control.Monad.Exception.Synchronous as Sync

import Control.Monad (Monad, return, liftM, mplus, join, (>>=), (>>), )
import Control.Applicative (Applicative, liftA, )
import Control.DeepSeq (NFData, rnf, )
import Data.Functor (Functor, fmap, )
{-
import Data.Traversable (Traversable, )
import Data.Foldable (Foldable, )
-}
import Data.Monoid(Monoid, mappend, mempty, )
import Data.Semigroup (Semigroup((<>)), )
import Data.Function (const, (.), ($), )
import Data.Maybe (Maybe(Just, Nothing), maybe, )

import Prelude (Show, )


-- * Plain monad

{- |
Contains a value and a reason why the computation of the value of type @a@ was terminated.
Imagine @a@ as a list type, and an according operation like the 'readFile' operation.
If the exception part is 'Nothing' then the value could be constructed regularly.
If the exception part is 'Just' then the value could not be constructed completely.
However you can read the result of type @a@ lazily,
even if an exception occurs while it is evaluated.
If you evaluate the exception part,
then the result value is certainly computed completely.

However, we cannot provide general 'Monad' functionality
due to the very different ways of combining the results of type @a@.
It is recommended to process the result value in an application specific way,
and after consumption of the result, throw a synchronous exception using 'toSynchronous'.

Maybe in the future we provide a monad instance
which considers subsequent actions as simultaneous processes on a lazy data structure.


This variant has lazy combinators like 'fmap'.
This implies that some laws are not fulfilled,
but in practice it saves you some calls to 'force'.
-}
data Exceptional e a =
   Exceptional {exception :: Maybe e, result :: a}
     deriving Show


{- |
Create an exceptional value without exception.
-}
pure :: a -> Exceptional e a
pure = Exceptional Nothing

{- |
Create an exceptional value with exception.
-}
broken :: e -> a -> Exceptional e a
broken e = Exceptional (Just e)


fromSynchronous :: a -> Sync.Exceptional e a -> Exceptional e a
fromSynchronous deflt x =
   force $ case x of
      Sync.Success y   -> Exceptional Nothing y
      Sync.Exception e -> Exceptional (Just e) deflt


fromSynchronousNull :: Sync.Exceptional e () -> Exceptional e ()
fromSynchronousNull = fromSynchronous ()

fromSynchronousMonoid :: Monoid a =>
   Sync.Exceptional e a -> Exceptional e a
fromSynchronousMonoid = fromSynchronous mempty


toSynchronous :: Exceptional e a -> Sync.Exceptional e a
toSynchronous (Exceptional me a) =
   maybe (Sync.Success a) Sync.Exception me


{- |
I think in most cases we want throwMonoid,
thus we can replace 'throw' by 'throwMonoid'.
-}
throw :: e -> Exceptional e ()
throw e = broken e ()

throwMonoid :: Monoid a => e -> Exceptional e a
throwMonoid e = broken e mempty

{- |
You might use an exception of type @Maybe e@ in 'manyMonoidT'
in order to stop the loop.
After finishing the loop you will want
to turn the @Nothing@ exception into a success.
This is achieved by this function.
-}
eatNothing :: Exceptional (Maybe e) a -> Exceptional e a
eatNothing (Exceptional e a) =
   Exceptional (join e) a


-- ** handling of special result types

{- |
This is an example for application specific handling of result values.
Assume you obtain two lazy lists say from 'readFile'
and you want to zip their contents.
If one of the stream readers emits an exception,
we quit with that exception.
If both streams have throw an exception at the same file position,
the exception of the first stream is propagated.
-}
zipWith ::
   (a -> b -> c) ->
   Exceptional e [a] -> Exceptional e [b] -> Exceptional e [c]
zipWith f (Exceptional ea a0) (Exceptional eb b0) =
   let recourse (a:as) (b:bs) =
          fmap (f a b :) (recourseF as bs)
       recourse as _ =
          Exceptional (case as of [] -> mplus ea eb; _ -> eb) []
       recourseF as bs = force $ recourse as bs
   in  recourseF a0 b0


infixr 1 `append`, `continue`, `maybeAbort`

{- |
This is an example for application specific handling of result values.
Assume you obtain two lazy lists say from 'readFile'
and you want to append their contents.
If the first stream ends with an exception,
this exception is kept
and the second stream is not touched.
If the first stream can be read successfully,
the second one is appended until stops.

'append' is less strict than the 'Monoid' method 'mappend' instance.
-}
append ::
   Monoid a =>
   Exceptional e a -> Exceptional e a -> Exceptional e a
append (Exceptional ea a) b =
   fmap (mappend a) $ continue ea b

continue ::
   Monoid a =>
   Maybe e -> Exceptional e a -> Exceptional e a
continue ea b =
   force $
   case ea of
--      Just e  -> throwMonoid e
      Just _  -> Exceptional ea mempty
      Nothing -> b

maybeAbort ::
   Exceptional e a -> Maybe e -> Exceptional e a
maybeAbort ~(Exceptional ea a) eb =
   Exceptional (mplus ea eb) a


instance (NFData e, NFData a) => NFData (Exceptional e a) where
   rnf (Exceptional e a) = rnf (e, a)

instance Monoid a => Semigroup (Exceptional e a) where
   Exceptional ea a <> Exceptional eb b =
      Exceptional (mplus ea eb) (mappend a (maybe b (const mempty) ea))

{- |
'mappend' must be strict in order to fulfill the Monoid laws
@mappend mempty a = a@ and @mappend a mempty = a@ for @a=undefined@.
-}
instance Monoid a => Monoid (Exceptional e a) where
   mempty = pure mempty
--   mappend = append
   mappend = (<>)


{- | construct Exceptional constructor lazily -}
{-# INLINE force #-}
force :: Exceptional e a -> Exceptional e a
force ~(Exceptional e a) = Exceptional e a

mapException :: (e0 -> e1) -> Exceptional e0 a -> Exceptional e1 a
mapException f ~(Exceptional e a) = Exceptional (fmap f e) a

mapExceptional :: (e0 -> e1) -> (a -> b) -> Exceptional e0 a -> Exceptional e1 b
mapExceptional f g ~(Exceptional e a) = Exceptional (fmap f e) (g a)


{- |
fmap (f.g) = fmap f . fmap g

The law
  fmap id = id
requires that we match the constructor strictly.

Strict matching
  fmap id undefined = undefined = id undefined

Lazy matching
  fmap id undefined = Exceptional undefined undefined
      /= undefined = id undefined
-}
{-
This was pointed out by kahl@cas.mcmaster.ca in libraries@haskell.org, 2011-01-22.
However, I think we rely a lot on the lazy matching in http-monad and parallelweb.
-}
instance Functor (Exceptional e) where
   fmap f ~(Exceptional e a) = Exceptional e (f a)


infixr 1 `simultaneousBind`, `simultaneousBindM`

{- |
I consider both actions to process the data simultaneously through lazy evaluation.
If the second one fails too, it must have encountered an exception
in the data that was successfully emitted by the first action,
and thus the exception of the second action is probably earlier.

We cannot check in general whether the two exception occur at the same time,
e.g. the second one might occur since the first occured and left an invalid structure.
In this case we should emit the first exception, not the second one.
Because of this I expect that this function is not particularly useful.
Otherwise it could be used as bind operation for a monad instance.
-}
{-# DEPRECATED simultaneousBind, simultaneousBindM "Check whether this function is really what you need. It generates an unreasonable exception when the second exception is caused by the first one." #-}
simultaneousBind :: Exceptional e a -> (a -> Exceptional e b) -> Exceptional e b
simultaneousBind ~(Exceptional mea a) actB =
   let Exceptional meb b = actB a
   in  Exceptional (mplus meb mea) b

simultaneousBindM :: (Monad m) => m (Exceptional e a) -> (a -> m (Exceptional e b)) -> m (Exceptional e b)
simultaneousBindM actA actB =
   do Exceptional mea a <- actA
      Exceptional meb b <- actB a
      return (Exceptional (mplus meb mea) b)


-- | Is there a better name?
{-# INLINE sequenceF #-}
sequenceF :: Functor f => Exceptional e (f a) -> f (Exceptional e a)
sequenceF ~(Exceptional e a) =
   fmap (Exceptional e) a

-- instance Foldable (Exceptional e) where

-- instance Traversable (Exceptional e) where

{- |
@Foldable@ instance would allow to strip off the exception too easily.

I like the methods of @Traversable@, but @Traversable@ instance requires @Foldable@ instance.
-}

{-# INLINE traverse #-}
traverse :: Applicative f => (a -> f b) -> Exceptional e a -> f (Exceptional e b)
traverse f = sequenceA . fmap f

{-# INLINE sequenceA #-}
sequenceA :: Applicative f => Exceptional e (f a) -> f (Exceptional e a)
sequenceA ~(Exceptional e a) =
   liftA (Exceptional e) a

{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Exceptional e a -> m (Exceptional e b)
mapM f = sequence . fmap f

{-# INLINE sequence #-}
sequence :: Monad m => Exceptional e (m a) -> m (Exceptional e a)
sequence ~(Exceptional e a) =
   liftM (Exceptional e) a



{-
instance Applicative (Exceptional e) where
   pure = pure
   f <*> x =
      case f of
         Exceptional e0 g ->
            case x of
               Exceptional e1 y -> Exceptional (mplus e0 e1) (g y)

instance Monad (Exceptional e) where
   return = pure
   fail _msg =
      Exceptional
         [Just (error "Asynchronous.fail exception")]
         (error "Asynchronous.fail result")
   x >>= f =
      case x of
         Exceptional e0 y ->
            case f y of
               Exceptional e1 z -> Exceptional (e0 ++ e1) z
-}

{- |
Consider a file format consisting of a header and a data body.
The header can only be used if is read completely.
Its parsing might stop with an synchronous exception.
The data body can also be used if it is truncated by an exceptional event.
This is expressed by an asynchronous exception.
A loader for this file format can thus fail
by a synchronous and an asynchronous exception.
Surprisingly, both orders of nesting these two kinds of exceptional actions
are equally expressive.
This function converts to the form where the synchronous exception is the outer one.

This is a specialisation of 'sequence' and friends.
-}
swapToSynchronousAsynchronous :: Exceptional e0 (Sync.Exceptional e1 a) -> Sync.Exceptional e1 (Exceptional e0 a)
swapToSynchronousAsynchronous ~(Exceptional e0 x) =
   fmap (Exceptional e0) x

swapToAsynchronousSynchronous :: Sync.Exceptional e1 (Exceptional e0 a) -> Exceptional e0 (Sync.Exceptional e1 a)
swapToAsynchronousSynchronous x =
--   Traversable.sequenceA x
   force $
   case x of
      Sync.Exception e1 -> pure $ Sync.Exception e1
      Sync.Success s -> fmap Sync.Success s


-- * Monad/Monoid transformer

{- |
In contrast to synchronous exceptions,
the asynchronous monad transformer is not quite a monad.
You must use the 'Monoid' interface or 'bindT' instead.
-}
newtype ExceptionalT e m a =
   ExceptionalT {runExceptionalT :: m (Exceptional e a)}


fromSynchronousT :: Functor m =>
   a -> Sync.ExceptionalT e m a -> ExceptionalT e m a
fromSynchronousT deflt =
   ExceptionalT .
   fmap (fromSynchronous deflt) .
   Sync.runExceptionalT

fromSynchronousMonoidT :: (Functor m, Monoid a) =>
   Sync.ExceptionalT e m a -> ExceptionalT e m a
fromSynchronousMonoidT =
   fromSynchronousT mempty


instance Functor m => Functor (ExceptionalT e m) where
   fmap f (ExceptionalT x) =
      ExceptionalT (fmap (fmap f) x)

instance (Monad m, Monoid a) => Semigroup (ExceptionalT e m a) where
   x <> y =
      ExceptionalT $
      appendM (runExceptionalT x) (runExceptionalT y)

instance (Monad m, Monoid a) => Monoid (ExceptionalT e m a) where
   mempty = ExceptionalT $ return mempty
   mappend = (<>)

{-
instance Applicative m => Applicative (ExceptionalT e m) where
   pure = ExceptionalT . pure . pure
   ExceptionalT f <*> ExceptionalT x =
      ExceptionalT (fmap (<*>) f <*> x)

instance Monad m => Monad (ExceptionalT e m) where
   return = ExceptionalT . return . return
   x0 >>= f =
      ExceptionalT $
      do Exceptional ex x <- runExceptionalT x0
         Exceptional ey y <- runExceptionalT (f x)
         return $ Exceptional (ex ++ ey) y
-}


{- |
see 'force'
-}
forceT :: Monad m => ExceptionalT e m a -> ExceptionalT e m a
forceT =
   ExceptionalT . liftM force . runExceptionalT

mapExceptionT :: (Monad m) =>
   (e0 -> e1) ->
   ExceptionalT e0 m a ->
   ExceptionalT e1 m a
mapExceptionT f =
   ExceptionalT . liftM (mapException f) . runExceptionalT

mapExceptionalT ::
   (m (Exceptional e0 a) -> n (Exceptional e1 b)) ->
   ExceptionalT e0 m a -> ExceptionalT e1 n b
mapExceptionalT f =
   ExceptionalT . f . runExceptionalT


throwMonoidT :: (Monad m, Monoid a) =>
   e -> ExceptionalT e m a
throwMonoidT = ExceptionalT . return . throwMonoid


eatNothingT :: Monad m =>
   ExceptionalT (Maybe e) m a -> ExceptionalT e m a
eatNothingT =
   mapExceptionalT (liftM eatNothing)


infixl 1 `bindT`

{- |
The monadic bind operation.
It cannot be made an instance of the Monad class method @(>>=)@
since it requires a default return value
in case the first action fails.
We get this default value by the 'Monoid' method 'mempty'.
-}
bindT :: (Monad m, Monoid b) =>
   ExceptionalT e m a ->
   (a -> ExceptionalT e m b) ->
   ExceptionalT e m b
bindT x y =
   ExceptionalT $
   runExceptionalT x >>= \r ->
   runExceptionalT $ maybe (y $ result r) throwMonoidT (exception r)


infixr 1 {- `bindM`, -} `appendM`, `continueM`

{-
bindM :: (Monad m, Monoid b) => SynchronousExceptional m a -> (a -> AsynchronousExceptional m b) -> AsynchronousExceptional m b
bindM x y =
   Sync.tryT x >>= \result ->
      liftM Async.force
      (case result of
         Sync.Exception e -> return $ Async.throwMonoid e
         Sync.Success s -> y s)
-}

appendM :: (Monad m, Monoid a) =>
   m (Exceptional e a) -> m (Exceptional e a) -> m (Exceptional e a)
appendM x y =
   do r <- x
      liftM (fmap (mappend (result r))) $
         continueMPlain (exception r) y

continueM :: (Monad m, Monoid a) =>
   m (Maybe e) -> m (Exceptional e a) -> m (Exceptional e a)
continueM mx y =
   mx >>= \x -> continueMPlain x y

continueMPlain :: (Monad m, Monoid a) =>
   Maybe e -> m (Exceptional e a) -> m (Exceptional e a)
continueMPlain x y =
   maybe y (return . throwMonoid) x


{- |
Repeat an action with synchronous exceptions until an exception occurs.
Combine all atomic results using the @bind@ function.
It may be @cons = (:)@ and @empty = []@ for @b@ being a list type.
The @defer@ function may be @id@
or @unsafeInterleaveIO@ for lazy read operations.
The exception is returned as asynchronous exception.
-}
manySynchronousT :: (Monad m) =>
   (m (Exceptional e b) -> m (Exceptional e b))
                           {- ^ @defer@ function -} ->
   (a -> b -> b)           {- ^ @cons@ function -} ->
   b                       {- ^ @empty@ -} ->
   Sync.ExceptionalT e m a {- ^ atomic action to repeat -} ->
   m (Exceptional e b)
manySynchronousT defer cons empty action =
   let recourse =
          liftM force $ defer $
          do r <- Sync.tryT action
             case r of
                Sync.Exception e -> return (Exceptional (Just e) empty)
                Sync.Success x   -> liftM (fmap (cons x)) recourse
   in  recourse

{-# DEPRECATED manySynchronousT "use manyMonoidT with appropriate Monad like LazyIO and result Monoid like Endo instead" #-}

{- |
We advise to use the Endo Monoid
when you want to read a series of characters into a list.
This means you use the difference lists technique
in order to build the list, which is efficient.

> import Data.Monoid (Endo, appEndo, )
> import Control.Exception (try, )
> import qualified Control.Monad.Exception.Synchronous as Sync

> fmap (flip appEndo []) $ manyMonoidT (fromSynchronousMonoidT $ fmap (Endo . (:)) $ Sync.fromEitherT $ try getChar)

If you want Lazy IO you must additionally convert @getChar@ to LazyIO monad.
-}
manyMonoidT :: (Monad m, Monoid a) =>
   ExceptionalT e m a {- ^ atomic action to repeat -} ->
   ExceptionalT e m a
manyMonoidT act =
   let -- like fmap, but doesn't require Functor instance of @m@
       customFmap f = mapExceptionalT (liftM (fmap f))
       go = act `bindT` \r -> customFmap (mappend r) go
   in  go

{- |
Scan @x@ using the @decons@ function
and run an action with synchronous exceptions for each element fetched from @x@.
Each invocation of an element action may stop this function
due to an exception.
If all element actions can be performed successfully
and if there is an asynchronous exception
then at the end this exception is raised as synchronous exception.
@decons@ function might be @Data.List.HT.viewL@.
-}
processToSynchronousT_ :: (Monad m) =>
   (b -> Maybe (a,b))  {- ^ decons function -} ->
   (a -> Sync.ExceptionalT e m ())
                       {- ^ action that is run for each element fetched from @x@ -} ->
   Exceptional e b     {- ^ value @x@ of type @b@ with asynchronous exception -} ->
   Sync.ExceptionalT e m ()
processToSynchronousT_ decons action (Exceptional me x) =
   let recourse b0 =
          maybe
             (maybe (return ()) Sync.throwT me)
             (\(a,b1) -> action a >> recourse b1)
             (decons b0)
   in  recourse x
