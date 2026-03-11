{-# LANGUAGE CPP #-}

{-|
Module      : Control.FromSum
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

This Haskell module exports various \"from\" functions for 'Either' and
'Maybe'.
-}
module Control.FromSum
  ( -- * Monadic in return value
    fromEitherM
  , fromEitherOrM
  , fromEitherM_
  , fromEitherOrM_
  , fromMaybeM
  , fromMaybeOrM
  , fromMaybeM_
  , fromMaybeOrM_
    -- * Monadic in both return and sum-type value
  , fromEitherMM
  , fromEitherOrMM
  , fromMaybeMM
  , fromMaybeOrMM
    -- * Completely non-monadic functions
  , fromEither
  , fromEitherOr
  , fromMaybe
  , fromMaybeOr
    -- * Converting from 'Maybe' to 'Either'
  , maybeToEither
  , maybeToEitherOr
  , eitherToMaybe
    -- * Collapsing funtions
  , collapseEither
  , collapseExceptT
  , collapseErrExceptT
  , -- * Converting to 'ExceptT'
    liftEitherExceptT
  , fromEitherExceptT
  , fromEitherOrExceptT
  , fromEitherMExceptT
  , fromEitherOrMExceptT
  , fromMaybeExceptT
  , fromMaybeOrExceptT
  , fromMaybeMExceptT
  , fromMaybeOrMExceptT
  , guardExceptT
  , guardMExceptT
    -- * Example converting to 'ExceptT'
    -- $exampleExceptT
    -- * Doctests
    -- $setup
  ) where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
import Data.Maybe (fromMaybe)

-- $setup
--
-- The following is setup code for doctests in this module.
--
-- >>> import Data.Functor.Identity (Identity(Identity))


-- | A monadic version of 'fromEither'.
--
-- @
--  'fromEitherM' leftAction === 'either' leftAction 'pure'
-- @
--
-- >>> fromEitherM (\s -> [length s]) $ Right 5
-- [5]
-- >>> fromEitherM (\s -> [length s]) $ Left ("foo" :: String)
-- [3]
fromEitherM
  :: Applicative m
  => (e -> m a) -> Either e a -> m a
fromEitherM leftAction = either leftAction pure

-- | A 'flip'ed version of 'fromEitherM'.
--
-- >>> fromEitherOrM (Right 5) $ \s -> [length s]
-- [5]
--
-- This can be nice to use as an error handler.
--
-- >>> fromEitherOrM (Right 5) $ \s -> putStrLn ("error: " ++ s) >> undefined
-- 5
-- >>> fromEitherOrM (Left "foo") $ \s -> putStrLn ("error: " ++ s) >> undefined
-- error: foo
-- ...
fromEitherOrM
  :: Applicative m
  => Either e a -> (e -> m a) -> m a
fromEitherOrM = flip fromEitherM

-- | Similar to 'fromEitherM', but only run the monadic 'leftAction' if the
-- 'Either' argument is 'Left'.  Otherwise, return 'pure' 'mempty'.
--
-- @
--  'fromEitherM_' leftAction === 'either' leftAction ('const' '$' 'pure' 'mempty')
-- @
--
-- >>> fromEitherM_ (\err -> putStrLn err >> pure "bye") $ Right 5
-- ""
-- >>> fromEitherM_ (\err -> putStrLn err >> pure "bye") $ Left "there was an error"
-- there was an error
-- "bye"
--
-- This can be convenient when you want to run some sort of logging function
-- whenever an 'Either' is 'Left'.  If you imagine the logging function is
-- @b -> 'IO' '()'@, then the effective type of 'fromEitherM_' becomes
-- @'fromEitherM_' :: (e -> 'IO' '()') -> 'Either' e a -> 'IO' '()'@, because
-- '()' has a 'Monoid' instance, and 'IO', has an 'Applicative' instance.
--
-- >>> fromEitherM_ putStrLn $ Left "there was an error"
-- there was an error
fromEitherM_
  :: (Applicative m, Monoid b)
  => (e -> m b) -> Either e a -> m b
fromEitherM_ leftAction = either leftAction (const $ pure mempty)

-- | A 'flip'ed version of 'fromEitherM_'.
fromEitherOrM_
  :: (Applicative m, Monoid b)
  => Either e a -> (e -> m b) -> m b
fromEitherOrM_ = flip fromEitherM_

-- | A monadic version of 'fromMaybe'.
--
-- @
--  'fromMaybeM' nothingAction === 'maybe' nothingAction 'pure'
-- @
--
-- >>> fromMaybeM [] $ Just 5
-- [5]
-- >>> fromMaybeM [] Nothing
-- []
fromMaybeM
  :: Applicative m
  => m a -> Maybe a -> m a
fromMaybeM nothingAction = maybe nothingAction pure

-- | A 'flip'ed version of 'fromMaybeM'.
--
-- >>> fromMaybeOrM (Just 5) []
-- [5]
--
-- This can be nice to use as an error handler.
--
-- >>> fromMaybeOrM (Just 5) $ putStrLn "some error occurred" >> undefined
-- 5
-- >>> fromMaybeOrM (Nothing) $ putStrLn "some error occurred" >> undefined
-- some error occurred
-- ...
fromMaybeOrM
  :: Applicative m
  => Maybe a -> m a -> m a
fromMaybeOrM = flip fromMaybeM

-- | Similar to 'fromMaybeM', but only run the monadic 'nothingAction' if the
-- 'Maybe' argument is 'Nothing'.  Otherwise, return 'pure' 'mempty'.
--
-- @
--  'fromMaybeM_' nothingAction === 'maybe' nothingAction ('const' '$' 'pure' 'mempty')
-- @
--
-- >>> fromMaybeM_ (putStrLn "hello" >> pure "bye") $ Just 5
-- ""
-- >>> fromMaybeM_ (putStrLn "hello" >> pure "bye") Nothing
-- hello
-- "bye"
--
-- This can be convenient when you want to run some sort of logging function
-- whenever a 'Maybe' is 'Nothing'.  If you imagine the logging function is
-- @'IO' '()'@, then the effective type of 'fromMaybeM_' becomes
-- @'fromMaybeM_' :: 'IO' '()' -> 'Maybe' a -> 'IO' '()'@, because '()' has a
-- 'Monoid' instance, and 'IO', has an 'Applicative' instance.
--
-- >>> fromMaybeM_ (putStrLn "hello") Nothing
-- hello
fromMaybeM_
  :: (Applicative m, Monoid b)
  => m b -> Maybe a -> m b
fromMaybeM_ nothingAction = maybe nothingAction (const $ pure mempty)

-- | A 'flip'ed version of 'fromMaybeM'.
fromMaybeOrM_
  :: (Applicative m, Monoid b)
  => Maybe a -> m b -> m b
fromMaybeOrM_ = flip fromMaybeM_

-- | Similar to 'fromEitherM' but the 'Either' argument is also a monadic value.
--
-- >>> fromEitherMM (\s -> [length s]) [Right 5, Right 10]
-- [5,10]
-- >>> fromEitherMM (\s -> [length s]) [Left ("foo" :: String), Right 100]
-- [3,100]
--
-- __NOTE__: I don't particularly like the name of this function.  If you have a
-- suggestion for a better name, please submit a PR or issue.
fromEitherMM
  :: Monad m
  => (e -> m a) -> m (Either e a) -> m a
fromEitherMM eitherAction mEither = fromEitherM eitherAction =<< mEither

-- | A 'flip'ed version of 'fromEitherMM'.
fromEitherOrMM
  :: Monad m
  => m (Either e a) -> (e -> m a) -> m a
fromEitherOrMM = flip fromEitherMM

-- | Similar to 'fromMaybeM' but the 'Maybe' argument is also a monadic value.
--
-- >>> fromMaybeMM [] [Just 6, Just 5]
-- [6,5]
-- >>> fromMaybeMM [] [Just 6, Nothing, Just 7]
-- [6,7]
--
-- __NOTE__: I don't particularly like the name of this function.  If you have a
-- suggestion for a better name, please submit a PR or issue.
fromMaybeMM
  :: Monad m
  => m a -> m (Maybe a) -> m a
fromMaybeMM nothingAction mMaybe = fromMaybeM nothingAction =<< mMaybe

-- | A 'flip'ed version of 'fromMaybeMM'.
fromMaybeOrMM
  :: Monad m
  => m (Maybe a) -> m a -> m a
fromMaybeOrMM = flip fromMaybeMM

-- | Similar to 'fromMaybe'.
--
-- >>> fromEither show $ Left 5
-- "5"
-- >>> fromEither show $ Right "hello"
-- "hello"
fromEither :: (e -> a) -> Either e a -> a
fromEither f = either f id

-- | A 'flip'ed version of 'fromEither'.
fromEitherOr :: Either e a -> (e -> a) -> a
fromEitherOr = flip fromEither

-- | A 'flip'ed version of 'fromMaybe'.
fromMaybeOr :: Maybe a -> a -> a
fromMaybeOr = flip fromMaybe

-- | Collapse an @'Either' a a@ to an @a@.  Defined as @'fromEither' 'id'@.
--
-- Note: Other libraries export this function as @fromEither@, but our
-- 'fromEither' function is slightly more general.
--
-- >>> collapseEither (Right 3)
-- 3
-- >>> collapseEither (Left "hello")
-- "hello"
collapseEither :: Either a a -> a
collapseEither = fromEither id

-- | Similar to 'collapseEither', but for 'ExceptT'.
--
-- >>> collapseExceptT (ExceptT $ pure (Right 3))
-- 3
-- >>> collapseExceptT (ExceptT $ pure (Left "hello"))
-- "hello"
collapseExceptT :: Monad m => ExceptT a m a -> m a
collapseExceptT = pure . collapseEither <=< runExceptT

-- | Collapse an 'ExceptT' where the error returns the same type as the whole
-- computation.
--
-- >>> let exceptTOne = pure 3 :: ExceptT (IO Int) IO Int
-- >>> collapseErrExceptT exceptTOne :: IO Int
-- 3
--
-- This is helpful when writing short-circuiting computations where you
-- throw errors that match the type of the underlying computation.
--
-- >>> :{
-- let go :: Int -> ExceptT (IO ()) IO ()
--     go x = do
--       bar <-
--         if x < 10
--         then
--           pure "hello"
--         else
--           throwE (putStrLn "Error occurred, x too big!")
--       lift $ putStrLn $ bar ++ " world"
-- :}
--
-- >>> collapseErrExceptT (go 100) :: IO ()
-- Error occurred, x too big!
-- >>> collapseErrExceptT (go 3) :: IO ()
-- hello world
--
-- In this example, the error type in the 'ExceptT' is @'IO' ()@.
-- This allows us to easily short-circuit the remaining computations.
-- In this example, the remaining computation is just printing
-- @bar '++' \" world\"@.
collapseErrExceptT :: Monad m => ExceptT (m a) m a -> m a
collapseErrExceptT exceptT = do
  res <- runExceptT exceptT
  case res of
    Left errHandler -> errHandler
    Right success -> pure success

-- | Convert a 'Maybe' to an 'Either'.
--
-- If the 'Maybe' is 'Just', then return the value in 'Right'.
--
-- >>> maybeToEither 3 $ Just "hello"
-- Right "hello"
--
-- If the 'Maybe' is 'Nothing', then use the given @e@ as 'Left'.
--
-- >>> maybeToEither 3 Nothing
-- Left 3
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

-- | A 'flip'ed version of 'maybeToEither'.
--
-- >>> maybeToEitherOr (Just "hello") 3
-- Right "hello"
--
-- >>> maybeToEitherOr Nothing 3
-- Left 3
maybeToEitherOr :: Maybe a -> e -> Either e a
maybeToEitherOr = flip maybeToEither

-- | Convert an 'Either' to a 'Maybe'.
--
-- A 'Right' value becomes 'Just'.
--
-- >>> eitherToMaybe $ Right 3
-- Just 3
--
-- A 'Left' value becomes 'Nothing'.
--
-- >>> eitherToMaybe $ Left "bye"
-- Nothing
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

-- | Lift an 'Either' into an 'ExceptT'.
--
-- This is the same as 'Control.Monad.Except.liftEither', but the return type
-- is specialized for 'ExceptT'.
--
-- >>> liftEitherExceptT (Right 3) :: ExceptT String Identity Int
-- ExceptT (Identity (Right 3))
--
-- Note that if you want to lift @m ('Either' e a)@ to @'ExceptT' e m a@,
-- just use 'ExceptT':
--
-- >>> action = Identity (Left "error") :: Identity (Either String Int)
-- >>> ExceptT action :: ExceptT String Identity Int
-- ExceptT (Identity (Left "error"))
liftEitherExceptT :: Applicative m => Either e a -> ExceptT e m a
liftEitherExceptT = ExceptT . pure

-- | Lift an 'Either' to an 'ExceptT' with a handler for transforming the error
-- value.
--
-- If the input 'Either' is 'Right', then just return it like normal:
--
-- >>> let rightEither = Right () :: Either String ()
-- >>> fromEitherExceptT (\str -> length str) rightEither :: ExceptT Int Identity ()
-- ExceptT (Identity (Right ()))
--
-- If the input 'Either' is 'Left', then pass the value to the handler:
--
-- >>> let leftEither = Left "hello" :: Either String ()
-- >>> fromEitherExceptT (\str -> length str) leftEither :: ExceptT Int Identity ()
-- ExceptT (Identity (Left 5))
fromEitherExceptT :: Monad m => (e -> x) -> Either e a -> ExceptT x m a
fromEitherExceptT handler eith = fromEitherMExceptT handler (pure eith)

-- | Just like 'fromEitherExceptT', but the arguments are flipped.
fromEitherOrExceptT :: Monad m => Either e a -> (e -> x) -> ExceptT x m a
fromEitherOrExceptT eith handler = fromEitherExceptT handler eith

-- | Similar to 'fromEitherExceptT' but the 'Either' value is lifted in a
-- 'Monad'.
--
-- >>> let identityLeft = Identity (Left "hello") :: Identity (Either String ())
-- >>> fromEitherMExceptT (\str -> length str) identityLeft :: ExceptT Int Identity ()
-- ExceptT (Identity (Left 5))
--
-- This is similar to 'Control.Monad.Trans.Except.withExceptT', but the second
-- argument is the unwrapped 'ExceptT' computation.
fromEitherMExceptT :: Monad m => (e -> x) -> m (Either e a) -> ExceptT x m a
fromEitherMExceptT handler action = do
  res <- lift action
  case res of
    Left e -> throwE (handler e)
    Right a -> pure a

-- | Just like 'fromEitherOrMExceptT', but the arguments are flipped.
fromEitherOrMExceptT :: Monad m => m (Either e a) -> (e -> x) -> ExceptT x m a
fromEitherOrMExceptT action handler = fromEitherMExceptT handler action

-- | Lift a 'Maybe' to an 'ExceptT' with a default value for the case when
-- the 'Maybe' is 'Nothing'.
--
-- If the 'Maybe' is 'Just', then just return the value like normal:
--
-- >>> let justVal = Just True :: Maybe Bool
-- >>> fromMaybeExceptT 5 justVal :: ExceptT Int Identity Bool
-- ExceptT (Identity (Right True))
--
-- If the 'Maybe' is 'Nothing', then use the default value as the error value:
--
-- >>> let nothingVal = Nothing :: Maybe Bool
-- >>> fromMaybeExceptT 5 nothingVal :: ExceptT Int Identity Bool
-- ExceptT (Identity (Left 5))
fromMaybeExceptT :: Monad m => x -> Maybe a -> ExceptT x m a
fromMaybeExceptT handler mayb = fromMaybeMExceptT handler (pure mayb)

-- | Just like 'fromMaybeExceptT' but with the arguments flipped.
fromMaybeOrExceptT :: Monad m => Maybe a -> x -> ExceptT x m a
fromMaybeOrExceptT = flip fromMaybeExceptT

-- | Similar to 'fromMaybeExceptT' except the 'Maybe' value is lifted in a
-- 'Monad'.
--
-- >>> let identityNothing = Identity Nothing :: Identity (Maybe Bool)
-- >>> fromMaybeMExceptT 5 identityNothing :: ExceptT Int Identity Bool
-- ExceptT (Identity (Left 5))
fromMaybeMExceptT :: Monad m => x -> m (Maybe a) -> ExceptT x m a
fromMaybeMExceptT handler action = do
  res <- lift action
  case res of
    Nothing -> throwE handler
    Just a -> pure a

-- | Just like 'fromMaybeMExceptT' but with the arguments flipped.
fromMaybeOrMExceptT :: Monad m => m (Maybe a) -> x -> ExceptT x m a
fromMaybeOrMExceptT = flip fromMaybeMExceptT

-- | Similar to 'guard', but for 'ExceptT'.
--
-- If the 'Bool' is 'True', then do nothing.
--
-- >>> guardExceptT True "error occurred" :: ExceptT String Identity ()
-- ExceptT (Identity (Right ()))
--
-- If the 'Bool' is 'False', then return the error case:
--
-- >>> guardExceptT False "error occurred" :: ExceptT String Identity ()
-- ExceptT (Identity (Left "error occurred"))
guardExceptT :: Monad m => Bool -> x -> ExceptT x m ()
guardExceptT True _ = pure ()
guardExceptT False handler = throwE handler

-- | Just like 'guardExceptT' (and similar to 'guardM'), except the boolean is
-- lifted in a 'Monad'.
--
-- >>> guardMExceptT (Identity False) "error occurred" :: ExceptT String Identity ()
-- ExceptT (Identity (Left "error occurred"))
guardMExceptT :: Monad m => m Bool -> x -> ExceptT x m ()
guardMExceptT action handler = do
  res <- lift action
  guardExceptT res handler

-- $exampleExceptT
--
-- Functions like 'fromMaybeExceptT' and 'fromEitherExceptT' are convenient
-- when paired with 'collapseErrExceptT'.  This section explains how
-- you can use these functions together.
--
-- Imagine you're writing a function that pulls user names from a database,
-- reads the first character of the name, and prints it to the console.
-- The functions for reading names from a database, and for
-- parsing the first character of the name could fail, so we will
-- handle these errors by logging to the console.
--
-- Here's the function we will be using for pulling user names from
-- the database.  If we pass @0@, it returns @\"SPJ\"@.  If we pass @1@,
-- it returns an empty string.  Otherwise it returns 'Nothing':
--
-- >>> :{
--   let getUserNameFromDb :: Int -> Maybe String
--       getUserNameFromDb 0 = Just "SPJ"
--       getUserNameFromDb 1 = Just ""
--       getUserNameFromDb _ = Nothing
-- :}
--
-- Here's the function we will be using for parsing the first character
-- of a user name.  If the user name is an empty string, we return
-- 'Left' with an error message.  Otherwise we return the first
-- character of the user name:
--
-- >>> :{
--   let parseFirstCharFromName :: String -> Either String Char
--       parseFirstCharFromName [] = Left "user name is empty"
--       parseFirstCharFromName (h:_) = Right h
-- :}
--
-- Now let's write our function.  If you didn't have the combinators from above
-- like 'fromEitherExceptT' and 'collapseErrExceptT', you might be tempted to
-- write nested @case@ patterns:
--
-- >>> :{
--   let nestedPrintFirstCharOfUserName :: Int -> IO ()
--       nestedPrintFirstCharOfUserName i =
--         -- Try to get the username for id i.
--         case getUserNameFromDb i of
--           -- If we couldn't get the user name from the database
--           -- print an error to the console.
--           Nothing -> putStrLn $ "ERROR: couldn't get user name for user " ++ show i
--           Just name ->
--             -- Try to parse the first character of the user name.
--             case parseFirstCharFromName name of
--               -- If we couldn't parse the first character of the user name,
--               -- print an error to the console.
--               Left err -> putStrLn $ "ERROR: " ++ err
--               Right firstChar ->
--                 -- Print the first character of the user name to the console.
--                 putStrLn $
--                   "Got first character of name for id " ++ show i ++ ": " ++ [firstChar]
-- :}
--
-- Here's an example of using this function, including the error cases:
--
-- >>> nestedPrintFirstCharOfUserName 100
-- ERROR: couldn't get user name for user 100
-- >>> nestedPrintFirstCharOfUserName 1
-- ERROR: user name is empty
-- >>> nestedPrintFirstCharOfUserName 0
-- Got first character of name for id 0: S
--
-- This works, and is understandable, but it gets unwieldy when there
-- are even more parsing steps.  You can get very deeply nested cases.
--
-- In order to write this without deeply nested error handling, you need a
-- short-circuiting 'Monad'.  Two popular examples are 'MaybeT' and 'ExceptT'.
--
-- Using 'collapseErrExceptT' (and 'ExcepT'), it is possible to write this
-- function by short-circuiting on errors:
--
-- >>> :{
--   let printFirstCharOfUserName :: Int -> IO ()
--       printFirstCharOfUserName i =
--         -- The argument to collapseErrExceptT is @ExceptT (IO ()) IO ()@.
--         -- This can be thought of as an action that can short-circuit
--         -- with @IO ()@ error-handling actions.
--         --
--         -- The error-handling actions below just log to the console.
--         collapseErrExceptT $ do
--           -- Get the user name from the db.
--           -- If getUserNameFromDb returns Nothing, then this whole
--           -- block will short circuit and collapseErrExceptT will
--           -- run our error handler.
--           --
--           -- Note that for the type of the error handler to work
--           -- correctly with @collapseErrExceptT@, the error
--           -- handler has to return @IO ()@.
--           name <-
--             fromMaybeOrExceptT (getUserNameFromDb i) $
--               putStrLn $ "ERROR: couldn't get user name for user " ++ show i
--           -- Parse out the first character from the user name.
--           -- If parseFirstCharFromName returns Left, then this whole
--           -- block will short circuit and collapseErrExceptT will
--           -- run our error handler.
--           firstChar <-
--             fromEitherOrExceptT (parseFirstCharFromName name) $ \err ->
--               putStrLn $ "ERROR: " ++ err
--           -- Print the first character of the name.
--           -- This needs to be 'lift'ed because this whole block is
--           -- actually @ExceptT (IO ()) IO ()@.
--           lift $
--             putStrLn $
--               "Got first character of name for id " ++ show i ++ ": " ++ [firstChar]
-- :}
--
-- The main good point here is that using the short-circuiting functionality of
-- 'ExceptT', we can write everything without nesting.
--
-- Here's a few examples of calling @printFirstCharOfUserName@.
--
-- Here we pass a user id that doesn't exist, so @getUserNameFromDb@ will
-- return 'Nothing'.  This causes the function to short-circuit and the
-- first error handler to be called.
--
-- >>> printFirstCharOfUserName 100
-- ERROR: couldn't get user name for user 100
--
-- Here we pass a user id that does exist, but the user name for this user id
-- is empty.  This causes the function to short-circuit and the
-- second error handler to be called.
--
-- >>> printFirstCharOfUserName 1
-- ERROR: user name is empty
--
-- This time the function succeeds:
--
-- >>> printFirstCharOfUserName 0
-- Got first character of name for id 0: S
--
-- In real code, the functions @getUserNameFromDb@ and @parseFirstCharFromName@
-- will have monadic return values.  In that case, you can use
-- 'fromMaybeOrMExceptT' and 'fromEitherOrMExceptT'.
