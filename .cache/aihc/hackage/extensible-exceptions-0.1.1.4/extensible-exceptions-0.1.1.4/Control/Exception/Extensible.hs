#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------
-- | 
-- Module      :  Control.Exception
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses ExistentialQuantification and DeriveDataTypeable)
--
-- This module provides the extensible exceptions API for raising and catching both
-- built-in and user-defined exceptions.
-- 
-- For newer versions of GHC (>=6.9), this package re-exports 'Control.Exception'.  
-- Otherwise, it provides a compatibility layer around the previous version of the 
-- extensions API.

#ifdef USE_NEW_EXCEPTIONS
module Control.Exception.Extensible (module Control.Exception) where

import Control.Exception hiding (blocked)

#else
module Control.Exception.Extensible (
    -- * The Exception type
    SomeException(..),
    Exception(..),
    E.IOException,
    E.ArithException(..),
    E.ArrayException(..),
    AssertionFailed(..),
    E.AsyncException(..),
    NonTermination(..),
    NestedAtomically(..),
    ExitCode(..),
    BlockedOnDeadMVar(..),
    BlockedIndefinitely(..),
    Deadlock(..),
    NoMethodError(..),
    PatternMatchFail(..),
    RecConError(..),
    RecSelError(..),
    RecUpdError(..),
    ErrorCall(..),
    
    -- * Throwing exceptions
    throwIO,
    throw,
    ioError,
    throwTo,
    -- * Catching Exceptions

    -- |There are several functions for catching and examining
    -- exceptions; all of them may only be used from within the
    -- 'IO' monad.

    -- ** The @catch@ functions
    catch,     -- :: IO a -> (Exception -> IO a) -> IO a
    catches, Handler(..),
    catchJust, -- :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a

    -- ** The @handle@ functions
    handle,    -- :: (Exception -> IO a) -> IO a -> IO a
    handleJust,-- :: (Exception -> Maybe b) -> (b -> IO a) -> IO a -> IO a

    -- ** The @try@ functions
    try,       -- :: IO a -> IO (Either Exception a)
    tryJust,   -- :: (Exception -> Maybe b) -> a    -> IO (Either b a)
    onException,

    -- ** The @evaluate@ function
    E.evaluate,  -- :: a -> IO a

    -- ** The @mapException@ function
    mapException,           -- :: (Exception -> Exception) -> a -> a

    -- * Asynchronous Exceptions

    -- $async

    -- ** Asynchronous exception control

    -- |The following two functions allow a thread to control delivery of
    -- asynchronous exceptions during a critical region.
    
    E.block,
    E.unblock,

    -- *** Applying @block@ to an exception handler

    -- $block_handler

    -- *** Interruptible operations

    -- $interruptible

    -- * Assertions

    assert,      

    -- * Utilities
    bracket,
    bracket_,
    bracketOnError,
    finally
    ) where

import Prelude hiding (catch)
import Control.Concurrent hiding (throwTo)
import qualified Control.Exception as E
import Data.Dynamic
import Data.Typeable
import System.Exit
import System.IO.Unsafe(unsafePerformIO)

class (Typeable e, Show e) => Exception e where
    toException   :: e -> SomeException
    fromException :: SomeException -> Maybe e

    toException = SomeException
    fromException (SomeException e) = cast e

data SomeException = forall e . Exception e => SomeException e
    deriving Typeable

instance Show SomeException where
    showsPrec p (SomeException e) = showsPrec p e

instance Exception SomeException where
    toException se = se
    fromException = Just

mkOldException :: Exception e => e -> E.Exception
mkOldException e = let e' = toException e
          in case fromException e' of
             Just e'' -> -- If the exception is actually a legacy exception
                         -- then throw it directly so the legacy functions
                         -- catch it as they expect
                         e''
             Nothing -> -- Otherwise, throw it as a dynamic
                        E.DynException (toDyn e')

throw :: Exception e => e -> a
throw e = E.throw (mkOldException e)

throwIO :: Exception e => e -> IO a
throwIO e = E.throwIO (mkOldException e)

throwTo :: Exception e => ThreadId -> e -> IO ()
throwTo tid e = E.throwTo tid (mkOldException e)

-----------------------------------------------------------------------------
-- Catching exceptions

-- |This is the simplest of the exception-catching functions.  It
-- takes a single argument, runs it, and if an exception is raised
-- the \"handler\" is executed, with the value of the exception passed as an
-- argument.  Otherwise, the result is returned as normal.  For example:
--
-- >   catch (openFile f ReadMode)
-- >       (\e -> hPutStr stderr ("Couldn't open "++f++": " ++ show e))
--
-- For catching exceptions in pure (non-'IO') expressions, see the
-- function 'evaluate'.
--
-- Note that due to Haskell\'s unspecified evaluation order, an
-- expression may return one of several possible exceptions: consider
-- the expression @error \"urk\" + 1 \`div\` 0@.  Does
-- 'catch' execute the handler passing
-- @ErrorCall \"urk\"@, or @ArithError DivideByZero@?
--
-- The answer is \"either\": 'catch' makes a
-- non-deterministic choice about which exception to catch.  If you
-- call it again, you might get a different exception back.  This is
-- ok, because 'catch' is an 'IO' computation.
--
-- Note that 'catch' catches all types of exceptions, and is generally
-- used for \"cleaning up\" before passing on the exception using
-- 'throwIO'.  It is not good practice to discard the exception and
-- continue, without first checking the type of the exception (it
-- might be a 'ThreadKilled', for example).  In this case it is usually better
-- to use 'catchJust' and select the kinds of exceptions to catch.
--
-- Also note that the "Prelude" also exports a function called
-- 'Prelude.catch' with a similar type to 'Control.Exception.catch',
-- except that the "Prelude" version only catches the IO and user
-- families of exceptions (as required by Haskell 98).
--
-- We recommend either hiding the "Prelude" version of 'Prelude.catch'
-- when importing "Control.Exception":
--
-- > import Prelude hiding (catch)
--
-- or importing "Control.Exception" qualified, to avoid name-clashes:
--
-- > import qualified Control.Exception as C
--
-- and then using @C.catch@
--
catch   :: Exception e
        => IO a         -- ^ The computation to run
        -> (e -> IO a)  -- ^ Handler to invoke if an exception is raised
        -> IO a
catch io poly_handler = io `E.catch` handler'
    where 
          -- First look for "new style" exceptions, which are thrown
          -- as E.DynException (SomeException e)

          -- needs scoped TVs: handler' :: E.Exception -> IO a          
          handler' e = case e of
                           E.DynException dyn ->
                               case fromDynamic dyn of
                               Just se@(SomeException _) ->
                                   case fromException se of
                                     Just e' -> poly_handler e'
                                     Nothing -> E.throw e
                               Nothing -> try_old e
                           _ -> try_old e

          -- If it's a legacy exception (E.Exception or one of the
          -- types that make up E.Exception), check for a handler than
          -- can handle them:

          -- needs scoped TVs: try_old :: E.Exception -> IO a
          try_old e = case fromException (toException e) of
                       Just e' -> poly_handler e'
                       Nothing -> E.throw e


-- | When you want to acquire a resource, do some work with it, and
-- then release the resource, it is a good idea to use 'bracket',
-- because 'bracket' will install the necessary exception handler to
-- release the resource in the event that an exception is raised
-- during the computation.  If an exception is raised, then 'bracket' will
-- re-raise the exception (after performing the release).
--
-- A common example is opening a file:
--
-- > bracket
-- >   (openFile "filename" ReadMode)
-- >   (hClose)
-- >   (\handle -> do { ... })
--
-- The arguments to 'bracket' are in this order so that we can partially apply
-- it, e.g.:
--
-- > withFile name mode = bracket (openFile name mode) hClose
--
bracket
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracket before after thing =
  E.block (do
    a <- before
    r <- E.unblock (thing a) `onException` after a
    after a
    return r
 )

onException :: IO a -> IO b -> IO a
onException io what = io `catch` \e -> do what
                                          throw (e :: SomeException)

block, unblock :: IO a -> IO a
block = E.block
unblock = E.unblock

-- | A specialised variant of 'bracket' with just a computation to run
-- afterward.
--
finally :: IO a         -- ^ computation to run first
        -> IO b         -- ^ computation to run afterward (even if an exception
                        -- was raised)
        -> IO a         -- returns the value from the first computation
a `finally` sequel =
  E.block (do
    r <- E.unblock a `onException` sequel
    sequel
    return r
  )

-- | A variant of 'bracket' where the return value from the first computation
-- is not required.
bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

-- | Like bracket, but only performs the final action if there was an
-- exception raised by the in-between computation.
bracketOnError
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracketOnError before after thing =
  block (do
    a <- before
    unblock (thing a) `onException` after a
  )

assert :: Bool -> a -> a
assert True x = x
assert False _ = throw (AssertionFailed "")

-- | The function 'catchJust' is like 'catch', but it takes an extra
-- argument which is an /exception predicate/, a function which
-- selects which type of exceptions we\'re interested in.
--
-- >   result <- catchJust errorCalls thing_to_try handler
--
-- Any other exceptions which are not matched by the predicate
-- are re-raised, and may be caught by an enclosing
-- 'catch' or 'catchJust'.
catchJust
        :: Exception e
        => (e -> Maybe b)         -- ^ Predicate to select exceptions
        -> IO a                   -- ^ Computation to run
        -> (b -> IO a)            -- ^ Handler
        -> IO a
catchJust p a handler = catch a handler'
  where handler' e = case p e of
                        Nothing -> throw e
                        Just b  -> handler b

-- | A version of 'catch' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.  For example:
--
-- >   do handle (\e -> exitWith (ExitFailure 1)) $
-- >      ...
handle     :: Exception e => (e -> IO a) -> IO a -> IO a
handle     =  flip catch

-- | A version of 'catchJust' with the arguments swapped around (see
-- 'handle').
handleJust :: Exception e => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust p =  flip (catchJust p)

-----------------------------------------------------------------------------
-- 'mapException'

-- | This function maps one exception into another as proposed in the
-- paper \"A semantics for imprecise exceptions\".

-- Notice that the usage of 'unsafePerformIO' is safe here.

mapException :: (Exception e1, Exception e2) => (e1 -> e2) -> a -> a
mapException f v = unsafePerformIO (catch (E.evaluate v)
                                          (\x -> throw (f x)))

-----------------------------------------------------------------------------
-- 'try' and variations.

-- | Similar to 'catch', but returns an 'Either' result which is
-- @('Right' a)@ if no exception was raised, or @('Left' e)@ if an
-- exception was raised and its value is @e@.
--
-- >  try a = catch (Right `liftM` a) (return . Left)
--
-- Note: as with 'catch', it is only polite to use this variant if you intend
-- to re-throw the exception after performing whatever cleanup is needed.
-- Otherwise, 'tryJust' is generally considered to be better.
--
-- Also note that "System.IO.Error" also exports a function called
-- 'System.IO.Error.try' with a similar type to 'Control.Exception.try',
-- except that it catches only the IO and user families of exceptions
-- (as required by the Haskell 98 @IO@ module).

try :: Exception e => IO a -> IO (Either e a)
try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))

-- | A variant of 'try' that takes an exception predicate to select
-- which exceptions are caught (c.f. 'catchJust').  If the exception
-- does not match the predicate, it is re-thrown.
tryJust :: Exception e => (e -> Maybe b) -> IO a -> IO (Either b a)
tryJust p a = do
  r <- try a
  case r of
        Right v -> return (Right v)
        Left  e -> case p e of
                        Nothing -> throw e
                        Just b  -> return (Left b)


-------------



data Handler a = forall e . Exception e => Handler (e -> IO a)

catches :: IO a -> [Handler a] -> IO a
catches io handlers = io `catch` catchesHandler handlers

catchesHandler :: [Handler a] -> SomeException -> IO a
catchesHandler handlers e = foldr tryHandler (throw e) handlers
    where tryHandler (Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res


-- -----------------------------------------------------------------------------
-- Asynchronous exceptions

{- $async

 #AsynchronousExceptions# Asynchronous exceptions are so-called because they arise due to
external influences, and can be raised at any point during execution.
'StackOverflow' and 'HeapOverflow' are two examples of
system-generated asynchronous exceptions.

The primary source of asynchronous exceptions, however, is
'throwTo':

>  throwTo :: ThreadId -> Exception -> IO ()

'throwTo' (also 'throwDynTo' and 'Control.Concurrent.killThread') allows one
running thread to raise an arbitrary exception in another thread.  The
exception is therefore asynchronous with respect to the target thread,
which could be doing anything at the time it receives the exception.
Great care should be taken with asynchronous exceptions; it is all too
easy to introduce race conditions by the over zealous use of
'throwTo'.
-}

{- $block_handler
There\'s an implied 'block' around every exception handler in a call
to one of the 'catch' family of functions.  This is because that is
what you want most of the time - it eliminates a common race condition
in starting an exception handler, because there may be no exception
handler on the stack to handle another exception if one arrives
immediately.  If asynchronous exceptions are blocked on entering the
handler, though, we have time to install a new exception handler
before being interrupted.  If this weren\'t the default, one would have
to write something like

>      block (
>           catch (unblock (...))
>                      (\e -> handler)
>      )

If you need to unblock asynchronous exceptions again in the exception
handler, just use 'unblock' as normal.

Note that 'try' and friends /do not/ have a similar default, because
there is no exception handler in this case.  If you want to use 'try'
in an asynchronous-exception-safe way, you will need to use
'block'.
-}

{- $interruptible

Some operations are /interruptible/, which means that they can receive
asynchronous exceptions even in the scope of a 'block'.  Any function
which may itself block is defined as interruptible; this includes
'Control.Concurrent.MVar.takeMVar'
(but not 'Control.Concurrent.MVar.tryTakeMVar'),
and most operations which perform
some I\/O with the outside world.  The reason for having
interruptible operations is so that we can write things like

>      block (
>         a <- takeMVar m
>         catch (unblock (...))
>               (\e -> ...)
>      )

if the 'Control.Concurrent.MVar.takeMVar' was not interruptible,
then this particular
combination could lead to deadlock, because the thread itself would be
blocked in a state where it can\'t receive any asynchronous exceptions.
With 'Control.Concurrent.MVar.takeMVar' interruptible, however, we can be
safe in the knowledge that the thread can receive exceptions right up
until the point when the 'Control.Concurrent.MVar.takeMVar' succeeds.
Similar arguments apply for other interruptible operations like
'System.IO.openFile'.
-}


----------------------------------------------------------------------
-- Exception instance for the legacy Exception type

instance Exception E.Exception

----------------------------------------------------------------------
-- The new Exception types. These need to map to/from E.Exception so
-- that uses of legacy catch/throw functions work.

----

instance Exception E.ArithException where
    toException ae = toException (E.ArithException ae)
    fromException (SomeException e) = case cast e of
                                      Just (E.ArithException ae) ->
                                         Just ae
                                      _ -> Nothing
----

instance Exception E.ArrayException where
    toException ae = toException (E.ArrayException ae)
    fromException (SomeException e) = case cast e of
                                      Just (E.ArrayException ae) ->
                                          Just ae
                                      _ -> Nothing

----

data AssertionFailed = AssertionFailed String
    deriving Typeable

instance Exception AssertionFailed where
    toException (AssertionFailed str) = toException (E.AssertionFailed str)
    fromException (SomeException e) = case cast e of
                                      Just (E.AssertionFailed str) ->
                                          Just (AssertionFailed str)
                                      _ -> Nothing

instance Show AssertionFailed where
    showsPrec _ (AssertionFailed err) = showString err

-----

instance Exception E.AsyncException where
    toException ae = toException (E.AsyncException ae)
    fromException (SomeException e) = case cast e of
                                      Just (E.AsyncException ae) ->
                                          Just ae
                                      _ -> Nothing

----

data BlockedOnDeadMVar = BlockedOnDeadMVar
    deriving Typeable

instance Exception BlockedOnDeadMVar where
    toException BlockedOnDeadMVar = toException (E.BlockedOnDeadMVar)
    fromException (SomeException e) = case cast e of
                                      Just E.BlockedOnDeadMVar ->
                                          Just BlockedOnDeadMVar
                                      _ -> Nothing
instance Show BlockedOnDeadMVar where
    showsPrec n BlockedOnDeadMVar = showsPrec n E.BlockedOnDeadMVar

----

data BlockedIndefinitely = BlockedIndefinitely
    deriving Typeable

instance Exception BlockedIndefinitely where
    toException BlockedIndefinitely = toException E.BlockedIndefinitely
    fromException (SomeException e) = case cast e of
                                      Just E.BlockedIndefinitely ->
                                          Just BlockedIndefinitely
                                      _ -> Nothing

instance Show BlockedIndefinitely where
    showsPrec n BlockedIndefinitely = showsPrec n E.BlockedIndefinitely

----

data NestedAtomically = NestedAtomically
    deriving Typeable

instance Exception NestedAtomically where
    toException NestedAtomically = toException E.NestedAtomically
    fromException (SomeException e) = case cast e of
                                    Just E.NestedAtomically ->
                                        Just NestedAtomically
                                    _ -> Nothing

instance Show NestedAtomically where
    showsPrec n NestedAtomically = showsPrec n E.NestedAtomically

----

data Deadlock = Deadlock
    deriving Typeable

instance Exception Deadlock where
    toException Deadlock = toException E.Deadlock
    fromException (SomeException e) = case cast e of
                                      Just E.Deadlock ->
                                          Just Deadlock
                                      _ -> Nothing

instance Show Deadlock where
    showsPrec n Deadlock = showsPrec n E.Deadlock

-----

data ErrorCall = ErrorCall String
    deriving Typeable

instance Exception ErrorCall where
    toException (ErrorCall str) = toException (E.ErrorCall str)
    fromException (SomeException e) = case cast e of
                                      Just (E.ErrorCall str) ->
                                          Just (ErrorCall str)
                                      _ -> Nothing

instance Show ErrorCall where
    showsPrec _ (ErrorCall err) = showString err

-----

instance Typeable ExitCode where
    typeOf _ = mkTyConApp (mkTyCon "ExitCode") []

instance Exception ExitCode where
    toException ee = toException (E.ExitException ee)
    fromException (SomeException e) = case cast e of
                                      Just (E.ExitException ee) ->
                                          Just ee
                                      _ -> Nothing
-----

instance Exception E.IOException where
    toException ioe = toException (E.IOException ioe)
    fromException (SomeException e) = case cast e of
                                      Just (E.IOException ioe) ->
                                          Just ioe
                                      _ -> Nothing

----

data NoMethodError = NoMethodError String
    deriving Typeable

instance Exception NoMethodError where
    toException (NoMethodError str) = toException (E.NoMethodError str)
    fromException (SomeException e) = case cast e of
                                      Just (E.NoMethodError str) ->
                                          Just (NoMethodError str)
                                      _ -> Nothing

instance Show NoMethodError where
    showsPrec _ (NoMethodError str) = showString str

----

data NonTermination = NonTermination
    deriving Typeable

instance Exception NonTermination where
    toException NonTermination = toException E.NonTermination
    fromException (SomeException e) = case cast e of
                                      Just E.NonTermination ->
                                          Just NonTermination
                                      _ -> Nothing

instance Show NonTermination where
    showsPrec n NonTermination = showsPrec n E.NonTermination

----

data PatternMatchFail = PatternMatchFail String
    deriving Typeable

instance Exception PatternMatchFail where
    toException (PatternMatchFail str) = toException (E.PatternMatchFail str)
    fromException (SomeException e) = case cast e of
                                      Just (E.PatternMatchFail str) ->
                                          Just (PatternMatchFail str)
                                      _ -> Nothing

instance Show PatternMatchFail where
    showsPrec _ (PatternMatchFail str) = showString str
    

----

data RecConError = RecConError String
    deriving Typeable

instance Exception RecConError where
    toException (RecConError str) = toException (E.RecConError str)
    fromException (SomeException e) = case cast e of
                                      Just (E.RecConError str) ->
                                          Just (RecConError str)
                                      _ -> Nothing

instance Show RecConError where
    showsPrec _ (RecConError str) = showString str
    


----

data RecSelError = RecSelError String
    deriving Typeable

instance Exception RecSelError where
    toException (RecSelError str) = toException (E.RecSelError str)
    fromException (SomeException e) = case cast e of
                                      Just (E.RecSelError str) ->
                                          Just (RecSelError str)
                                      _ -> Nothing

instance Show RecSelError where
    showsPrec _ (RecSelError str) = showString str

----

data RecUpdError = RecUpdError String
    deriving Typeable

instance Exception RecUpdError where
    toException (RecUpdError str) = toException (E.RecUpdError str)
    fromException (SomeException e) = case cast e of
                                      Just (E.RecUpdError str) ->
                                          Just (RecUpdError str)
                                      _ -> Nothing

instance Show RecUpdError where
    showsPrec _ (RecUpdError str) = showString str


#endif

