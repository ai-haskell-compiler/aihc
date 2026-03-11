{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Try (
	-- * DATA TRY
	Try, maybeToTry,
	-- * RUN TRY
	runTry, gatherSuccess,
	-- * THROW AND CATCH ERROR
	throw, catch, rights,
	-- * WRITE AND GET LOG
	Set, tell, partial,
	-- * TOOL
	cons ) where

import Control.Applicative (Alternative(..))
import Control.Arrow (first)
import Control.Monad (MonadPlus)
import Data.Maybe (catMaybes)

---------------------------------------------------------------------------

-- * DATA TRY
--	+ DATA
--	+ INSTANCE
-- * RUN TRY
-- * THROW AND CATCH ERROR
-- * WRITE AND GET LOG
-- * TOOL

---------------------------------------------------------------------------
-- DATA TRY
---------------------------------------------------------------------------

-- DATA

data Try e w a = Try (Either e a) w deriving Show

try :: (Either e a -> w -> b) -> Try e w a -> b
try f (Try ex w) = f ex w

maybeToTry :: Monoid w => e -> Maybe a -> Try e w a
maybeToTry e = maybe (throw e) pure

-- INSTANCE

instance Functor (Try e w) where
	fmap f = try $ either (Try . Left) (Try . Right . f)

instance Monoid w => Applicative (Try e w) where
	pure = (`Try` mempty) . Right
	Try (Left e) w <*> _ = Try (Left e) w
	Try (Right f) w <*> mx = (\ex -> Try ex . (w <>)) `try` (f <$> mx)

instance Monoid w => Alternative (Try w w) where
	empty = Try (Left mempty) mempty
	Try (Left e) w <|> t = tell w >> tell e >> t
	t@(Try (Right _) _) <|> _ = t

instance Monoid w => Monad (Try e w) where
	Try (Left e) w >>= _ = Try (Left e) w
	Try (Right x) w >>= f = (\ex -> Try ex . (w <>)) `try` f x

instance Monoid w => MonadPlus (Try w w)

---------------------------------------------------------------------------
-- RUN TRY
---------------------------------------------------------------------------

runTry :: Try e w a -> (Either e a, w)
runTry (Try ex w) = (ex, w)

gatherSuccess :: (Monoid w, Set w w) => [Try w w a] -> ([a], w)
gatherSuccess = (either (const []) id `first`) . runTry . rights

---------------------------------------------------------------------------
-- THROW AND CATCH ERROR
---------------------------------------------------------------------------

throw :: Monoid w => e -> Try e w a
throw = (`Try` mempty) . Left

catch :: Semigroup w => Try e w a -> (e -> Try e w a) -> Try e w a
Try (Left e) w `catch` h = (\ex -> Try ex . (w <>)) `try` h e
t@(Try (Right _) _) `catch` _ = t

rights :: (Monoid w, Set w w) => [Try w w a] -> Try w w [a]
rights = (catMaybes <$>) . mapM ((`catch` (Nothing <$) . tell) . (Just <$>))

---------------------------------------------------------------------------
-- WRITE AND GET LOG
---------------------------------------------------------------------------

class Set x xs where set :: x -> xs

instance Set x x where set = id
instance Monoid xs => Set x (x, xs) where set x = (x, mempty)

instance {-# OVERLAPPABLE #-} (Monoid y, Set x xs) => Set x (y, xs) where
	set x = (mempty, set x)

tell :: Set w ws => w -> Try e ws ()
tell = Try (Right ()) . set

partial :: Try e (w, ws) a -> Try e ws (Either e a, w)
partial (Try ex (w, ws)) = Try (Right (ex, w)) ws

---------------------------------------------------------------------------
-- TOOL
---------------------------------------------------------------------------

cons :: (Monoid w, Set w w) => Either w a -> [a] -> Try w w [a]
cons = either (flip (<$) . tell) ((pure .) . (:))
