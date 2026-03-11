{- |
Synchronous exceptions immediately abort a series of computations.
We provide monads for describing this behaviour.
In contrast to ErrorT from @mtl@ or @transformers@ package
we do not pose restrictions on the exception type.

How to tell, that a function can possibly throw more than one (kind of) exception?

If you would use the exception type @(Either ParserException IOError)@
then this is different from @(Either IOError ParserException)@.
Thus we recommned using type classes for exceptions.
Then you can use one type containing all exceptions in an application,
but the type signature still tells which exceptions are actually possible.
Examples:

> parser :: ParserException e => ExceptionalT e ParserMonad a
>
> getLine :: IOException e => ExceptionalT e IO String
>
> fileParser :: (ParserException e, IOException e) => ExceptionalT e IO String

You can remove single exceptions from the set,
but with Haskell 98 you need instances for all the other constraints
in the exception constraint set.
There is a more advanced approach,
that allows removing exceptions constraints
without a quadratic growth of instances.
It uses some non-Haskell-98 type hackery,
see the @exception@ package by Joseph Iborra.
Fortunately, you use this package in every case
and let the user decide
whether he wants Haskell 98 or non-standard way of handling exceptions.

See also: <https://wiki.haskell.org/Exception>.
-}
module Control.Monad.Exception.Synchronous (
   Exceptional(..),
   fromMaybe,    toMaybe,
   fromEither,   toEither,
   fromExitCode, toExitCode,
   getExceptionNull,
   switch,
   force,
   mapException,
   mapExceptional,
   throw,
   assert,
   catch,
   resolve,
   merge,
   alternative,

   ExceptionalT(..),
   fromMaybeT,    toMaybeT,
   fromEitherT,   toEitherT,
   fromExitCodeT, toExitCodeT,
   liftT,
   switchT,
   forceT,
   mapExceptionT,
   mapExceptionalT,
   throwT,
   assertT,
   catchT,
   bracketT,
   resolveT,
   tryT,
   manyT,
   manyMonoidT,
   mergeT,
   alternativeT,
   ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad, return, liftM, liftM2, (>>=), (>>), (=<<),
          {- MonadPlus(mzero, mplus), -})
import Control.Monad.Fix (MonadFix, mfix, )
import Control.Monad.Trans.Class (MonadTrans, lift, )
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.DeepSeq (NFData, rnf, )
import Data.Functor (Functor, fmap, )
import Data.Monoid(Monoid, mappend, mempty, Endo(Endo, appEndo), )
import Data.Function (flip, const, (.), ($), )
import Data.Either (Either(Left, Right), either, )
import Data.Maybe (Maybe(Just, Nothing), maybe, )
import Data.Bool (Bool, )
import Data.Eq (Eq, )

import System.Exit (ExitCode(ExitSuccess, ExitFailure), )

import Prelude (Show, Int, )


-- * Plain monad

{- |
Like 'Either', but explicitly intended for handling of exceptional results.
In contrast to 'Either' we do not support 'fail'.
Calling 'fail' in the 'Exceptional' monad is an error.
This way, we do not require that an exception can be derived from a 'String',
yet, we require no constraint on the exception type at all.
-}
data Exceptional e a =
     Success a
   | Exception e
   deriving (Show, Eq)


fromMaybe :: e -> Maybe a -> Exceptional e a
fromMaybe e = maybe (Exception e) Success

fromEither :: Either e a -> Exceptional e a
fromEither = either Exception Success

toMaybe :: Exceptional e a -> Maybe a
toMaybe = switch (const Nothing) Just

toEither :: Exceptional e a -> Either e a
toEither x =
   case x of
      Success a   -> Right a
      Exception e -> Left e


toExitCode :: Exceptional Int () -> ExitCode
toExitCode e =
   case e of
      Success () -> ExitSuccess
      Exception n -> ExitFailure n

fromExitCode :: ExitCode -> Exceptional Int ()
fromExitCode e =
   case e of
      ExitSuccess -> Success ()
      ExitFailure n -> Exception n


-- | useful in connection with 'Control.Monad.Exception.Asynchronous.continue'
getExceptionNull :: Exceptional e () -> Maybe e
getExceptionNull x =
   case x of
      Success _   -> Nothing
      Exception e -> Just e


{- |
Counterpart to 'either' for 'Either'.
-}
switch :: (e -> b) -> (a -> b) -> Exceptional e a -> b
switch f g x =
   case x of
      Success a -> g a
      Exception e -> f e

{- |
If you are sure that the value is always a 'Success'
you can tell that the run-time system
thus making your program lazy.
However, try to avoid this function by using 'catch' and friends,
since this function is partial.
-}
force :: Exceptional e a -> Exceptional e a
force ~(Success a) = Success a

mapException :: (e0 -> e1) -> Exceptional e0 a -> Exceptional e1 a
mapException f x =
   case x of
      Success a   -> Success a
      Exception e -> Exception (f e)

mapExceptional :: (e0 -> e1) -> (a -> b) -> Exceptional e0 a -> Exceptional e1 b
mapExceptional f g x =
   case x of
      Success a   -> Success (g a)
      Exception e -> Exception (f e)

throw :: e -> Exceptional e a
throw = Exception

assert :: e -> Bool -> Exceptional e ()
assert e b =
   if b then Success () else throw e

catch :: Exceptional e0 a -> (e0 -> Exceptional e1 a) -> Exceptional e1 a
catch x handler =
   case x of
      Success a   -> Success a
      Exception e -> handler e

{-
bracket ::
   Exceptional e h ->
   (h -> Exceptional e ()) ->
   (h -> Exceptional e a) ->
   Exceptional e a
bracket open close action =
   open >>= \h ->
   case action h of
-}

resolve :: (e -> a) -> Exceptional e a -> a
resolve handler x =
   case x of
      Success a   -> a
      Exception e -> handler e

-- like Applicative.<|>
infixl 3 `alternative`, `alternativeT`

alternative, _alternative ::
   Exceptional e a -> Exceptional e a -> Exceptional e a
alternative x y = catch x (const y)
_alternative x y = switch (const y) Success x



-- like Applicative.<*>
infixl 4 `merge`, `mergeT`

{- | see 'mergeT' -}
merge, mergeLazy, _mergeStrict ::
   (Monoid e) =>
   Exceptional e (a -> b) -> Exceptional e a -> Exceptional e b
merge = mergeLazy

mergeLazy ef ea =
   case ef of
      Exception e0 ->
         Exception $ mappend e0 $
         case ea of
            Success _ -> mempty
            Exception e1 -> e1
      Success f -> fmap f ea

_mergeStrict ef ea =
   case (ef,ea) of
      (Success f, Success a) -> Success $ f a
      (Exception e, Success _) -> Exception e
      (Success _, Exception e) -> Exception e
      (Exception e0, Exception e1) -> Exception $ mappend e0 e1


instance (NFData e, NFData a) => NFData (Exceptional e a) where
   rnf = switch rnf rnf

instance Functor (Exceptional e) where
   fmap f x =
      case x of
         Success a   -> Success (f a)
         Exception e -> Exception e

instance Applicative (Exceptional e) where
   pure = Success
   f <*> x =
      case f of
         Exception e -> Exception e
         Success g ->
            case x of
               Success a   -> Success (g a)
               Exception e -> Exception e

instance Monad (Exceptional e) where
   return = pure
   x >>= f =
      case x of
         Exception e -> Exception e
         Success y -> f y

{- |
I think it is not a good idea to use this instance,
maybe we shoul remove it.
It expects that the constructor is 'Success'
and the result is undefined otherwise.
But if the constructor must always be 'Success',
why using 'Exceptional' then, at all?
-}
instance MonadFix (Exceptional e) where
    mfix f =
       let unSuccess ~(Success x) = x
           a = f (unSuccess a)
       in  a

{-
A MonadPlus instance would require another class, say DefaultException,
that provides a default exception used for @mzero@.
In Control.Monad.Error this is handled by the Error class.
Since String is a typical type used for exceptions -
shall there be a DefaultException String instance?
-}



-- * Monad transformer

-- | like ErrorT, but ExceptionalT is the better name in order to distinguish from real (programming) errors
newtype ExceptionalT e m a =
   ExceptionalT {runExceptionalT :: m (Exceptional e a)}


_assertMaybeT :: (Monad m) => e -> Maybe a -> ExceptionalT e m a
_assertMaybeT e = maybe (throwT e) return

fromMaybeT :: Monad m => e -> MaybeT m a -> ExceptionalT e m a
fromMaybeT e  =  ExceptionalT . liftM (fromMaybe e) . runMaybeT

toMaybeT :: Monad m => ExceptionalT e m a -> MaybeT m a
toMaybeT  =  MaybeT . liftM toMaybe . runExceptionalT

fromEitherT :: Monad m => m (Either e a) -> ExceptionalT e m a
fromEitherT  =  ExceptionalT . liftM fromEither

toEitherT :: Monad m => ExceptionalT e m a -> m (Either e a)
toEitherT  =  liftM toEither . runExceptionalT

toExitCodeT ::
   (Functor m) =>
   ExceptionalT Int m () -> m ExitCode
toExitCodeT act =
   fmap toExitCode $ runExceptionalT act

fromExitCodeT ::
   (Functor m) =>
   m ExitCode -> ExceptionalT Int m ()
fromExitCodeT act =
   ExceptionalT $ fmap fromExitCode act


liftT :: (Monad m) => Exceptional e a -> ExceptionalT e m a
liftT = ExceptionalT . return


switchT ::
   (Monad m) =>
   (e -> m b) -> (a -> m b) ->
   ExceptionalT e m a -> m b
switchT e s m =
   switch e s =<< runExceptionalT m

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

throwT :: (Monad m) =>
   e -> ExceptionalT e m a
throwT = ExceptionalT . return . throw

assertT :: (Monad m) =>
   e -> Bool -> ExceptionalT e m ()
assertT e = ExceptionalT . return . assert e

catchT :: (Monad m) =>
   ExceptionalT e0 m a ->
   (e0 -> ExceptionalT e1 m a) ->
   ExceptionalT e1 m a
catchT action handler =
   ExceptionalT $ switchT (runExceptionalT . handler) (return . Success) action

{- |
If the enclosed monad has custom exception facilities,
they could skip the cleanup code.
Make sure, that this cannot happen by choosing an appropriate monad.
-}
bracketT :: (Monad m) =>
   ExceptionalT e m h ->
   (h -> ExceptionalT e m ()) ->
   (h -> ExceptionalT e m a) ->
   ExceptionalT e m a
bracketT open close action =
   open >>= \h ->
      ExceptionalT $
         do a <- runExceptionalT (action h)
            c <- runExceptionalT (close h)
            return (a >>= \r -> c >> return r)

resolveT :: (Monad m) =>
   (e -> m a) -> ExceptionalT e m a -> m a
resolveT handler x =
   do r <- runExceptionalT x
      resolve handler (fmap return r)

tryT :: (Monad m) =>
   ExceptionalT e m a -> m (Exceptional e a)
tryT = runExceptionalT


{- |
Repeat an action until an exception occurs.
Initialize the result with @empty@ and add new elements using @cons@
(e.g. @[]@ and @(:)@).
The exception handler decides whether the terminating exception
is re-raised ('Just') or catched ('Nothing').
-}
manyT :: (Monad m) =>
   (e0 -> Maybe e1)        {- ^ exception handler -} ->
   (a -> b -> b)           {- ^ @cons@ function -} ->
   b                       {- ^ @empty@ -} ->
   ExceptionalT e0 m a     {- ^ atomic action to repeat -} ->
   ExceptionalT e1 m b
manyT handler cons empty action =
   liftM (flip appEndo empty) $
   manyMonoidT handler $
   liftM (Endo . cons) action

manyMonoidT :: (Monad m, Monoid a) =>
   (e0 -> Maybe e1)        {- ^ exception handler -} ->
   ExceptionalT e0 m a     {- ^ atomic action to repeat -} ->
   ExceptionalT e1 m a
manyMonoidT handler action =
   let recourse =
          do r <- lift $ tryT action
             case r of
                -- Exception e -> maybe (return empty) throwT (handler e)
                -- more lazy
                Exception e -> ExceptionalT $ return $ maybe (Success mempty) throw (handler e)
                Success x   -> liftM (mappend x) recourse
   in  recourse


{- |
This combines two actions similar to Applicative's @<*>@.
The result action fails if one of the input action fails,
but both actions are executed.
E.g. consider a compiler that emits all errors
that can be detected independently,
but eventually aborts if there is at least one error.

The exception type @e@ might be a list type,
or an @Endo@ type that implements a difflist.
-}
mergeT ::
   (Monoid e, Monad m) =>
   ExceptionalT e m (a -> b) ->
   ExceptionalT e m a ->
   ExceptionalT e m b
mergeT mf ma =
   ExceptionalT $
   liftM2 merge (runExceptionalT mf) (runExceptionalT ma)

alternativeT, _alternativeT ::
   (Monad m) =>
   ExceptionalT e m a -> ExceptionalT e m a -> ExceptionalT e m a
alternativeT x y = catchT x (const y)
_alternativeT x y =
   ExceptionalT $ switchT (const $ runExceptionalT y) (return . Success) x


instance Functor m => Functor (ExceptionalT e m) where
   fmap f (ExceptionalT x) =
      ExceptionalT (fmap (fmap f) x)

instance Applicative m => Applicative (ExceptionalT e m) where
   pure = ExceptionalT . pure . pure
   ExceptionalT f <*> ExceptionalT x =
      ExceptionalT (fmap (<*>) f <*> x)

instance Monad m => Monad (ExceptionalT e m) where
   return = ExceptionalT . return . return
   x0 >>= f =
      ExceptionalT $
         runExceptionalT x0 >>= \x1 ->
         case x1 of
            Exception e -> return (Exception e)
            Success x -> runExceptionalT $ f x

{- |
Same restrictions applies as for @instance MonadFix (Exceptional e a)@.
-}
instance (MonadFix m) => MonadFix (ExceptionalT e m) where
   mfix f = ExceptionalT $ mfix $ \ ~(Success r) -> runExceptionalT $ f r

instance MonadTrans (ExceptionalT e) where
   lift m = ExceptionalT $ liftM Success m

{-
instance MonadIO m => MonadIO (ExceptionalT e m) where
   liftIO act = ExceptionalT $ liftIO $ liftM Success act
-}
