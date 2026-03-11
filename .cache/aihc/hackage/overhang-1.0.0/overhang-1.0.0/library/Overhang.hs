{-# LANGUAGE NoImplicitPrelude #-}

module Overhang
  ( -- * Introduction
    -- $intro

    -- * Getting Started
    -- $gettingStarted

    -- * Functions

    -- ** Either
    onLeft
  , onRight

    -- ** Maybe
  , onJust
  , onNothing

    -- ** Regular ol' Application
  , onApp

    -- ** Functor
  , onMap

    -- ** ExitCode
  , onExitSuccess
  , onExitFailure

    -- ** Bool
  , onTrue
  , onFalse

    -- ** Bifoldable
  , onBiforFirst_
  , onBiforFirstM_

    -- ** Bifunctor
  , onBimapFirst
  , onBimapSecond

    -- ** Bitraversable
  , onBiforFirst
  , onBiforFirstM
  ) where

import Control.Applicative (Applicative)
import Data.Bifoldable (Bifoldable)
import qualified Data.Bifoldable as Bifoldable
import Data.Bifunctor (Bifunctor)
import qualified Data.Bifunctor as Bifunctor
import Data.Bitraversable (Bitraversable)
import qualified Data.Bitraversable as Bitraversable
import Data.Bool (Bool)
import qualified Data.Bool as Bool
import Data.Int (Int)
import Data.Either (Either)
import qualified Data.Either as Either
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Function ((&))
import qualified Data.Function as Function
import Data.Functor (Functor, fmap)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

-- | Hang on the 'Data.Either.Left' case of an 'Either'.
--
-- @ onLeft e r l = either l r e @
onLeft :: Either a b -> (b -> c) -> (a -> c) -> c
onLeft e r l = Either.either l r e

-- | Hang on the 'Data.Either.Right' case of an 'Either'.
--
-- @ onRight e l r = either l r e @
onRight :: Either a b -> (a -> c) -> (b -> c) -> c
onRight e l r = Either.either l r e

-- | Hang on the 'Data.Maybe.Just' case of a 'Maybe'.
--
-- @ onJust m d j = maybe d j m @
onJust :: Maybe a -> b -> (a -> b) -> b
onJust m d j = Maybe.maybe d j m

-- | Hang on the 'Data.Maybe.Nothing' case of a 'Maybe'. Mostly useful when @b@ is some
-- monadic computation.
--
-- @ onNothing m j d = maybe d j m @
onNothing :: Maybe a -> (a -> b) -> b -> b
onNothing m j d = Maybe.maybe d j m

-- | Hang on function application, a.k.a. non-operator version of
-- 'Data.Function.&'.
--
-- @ onApp = (&) @
onApp :: a -> (a -> b) -> b
onApp = (&)

-- | Hang on a 'Functor' mapping function, a.k.a. non-operator version of
-- @\<&\>@ from @lens@.
--
-- @ onMap = flip fmap @
onMap :: Functor f => f a -> (a -> b) -> f b
onMap = Function.flip fmap

-- | Hang on the 'ExitSuccess' case of an 'ExitCode'. Mostly useful when @b@ is some
-- monadic computation.
onExitSuccess :: ExitCode -> (Int -> b) -> b -> b
onExitSuccess ExitSuccess _ s = s
onExitSuccess (ExitFailure c) f _ = f c

-- | Hang on the 'ExitFailure' case of an 'ExitCode'.
onExitFailure :: ExitCode -> b -> (Int -> b) -> b
onExitFailure ExitSuccess s _ = s
onExitFailure (ExitFailure c) _ f = f c

-- | Hang on the 'Data.Bool.True' case of 'Bool.bool'.
--
-- @ onTrue b f t = bool f t b @
onTrue :: Bool -> a -> a -> a
onTrue b f t = Bool.bool f t b

-- | Hang on the 'Data.Bool.False' case of 'Bool.bool'.
--
-- @ onFalse b t f = bool f t b @
onFalse :: Bool -> a -> a -> a
onFalse b t f = Bool.bool f t b

-- | Hang on the "left" folding portion of a 'Bifoldable'. A variant of 'Bifoldable.bifor_'.
--
-- @ onBiforFirst_ t g f = bifor_ t f g @
onBiforFirst_ :: (Bifoldable t, Applicative f) => t a b -> (b -> f d) -> (a -> f c) -> f ()
onBiforFirst_ t g f = Bifoldable.bifor_ t f g

-- | Alias for 'onBiforFirst_'.
onBiforFirstM_ :: (Bifoldable t, Applicative f) => t a b -> (b -> f d) -> (a -> f c) -> f ()
onBiforFirstM_ = onBiforFirst_

-- | Hang on the "left" mapping portion of a 'Bifunctor'.
--
-- @ onBimapFirst t g f = bimap f g t @
onBimapFirst :: Bifunctor t => t a c -> (c -> d) -> (a -> b) -> t b d
onBimapFirst t g f = Bifunctor.bimap f g t

-- | Hang on the "right" mapping portion of a 'Bifunctor'.
--
-- @ onBimapSecond t f g = bimap f g t @
onBimapSecond :: Bifunctor t => t a c -> (a -> b) -> (c -> d) -> t b d
onBimapSecond t f g = Bifunctor.bimap f g t

-- | Hang on the "left" traversing portion of a 'Bitraversable'. A variant of 'Bitraversable.bifor'.
--
-- @ onBiforFirst t g f = bifor t f g @
onBiforFirst :: (Bitraversable t, Applicative f) => t a b -> (b -> f d) -> (a -> f c) -> f (t c d)
onBiforFirst t g f = Bitraversable.bifor t f g

-- | Alias for 'onBiforFirst'.
onBiforFirstM :: (Bitraversable t, Applicative f) => t a b -> (b -> f d) -> (a -> f c) -> f (t c d)
onBiforFirstM = onBiforFirst

{- $intro

@overhang@ provides convenience combinators for clean, "hanging" lambdas and depends only on @base@.  The gist of this library is offering variants of functions with parameter orders more conducive to finishing off the function call with a lambda.  When using @Data.Foldable@, sometimes we reach for @for_@ \/ @forM_@ instead of @traverse@ \/ @mapM_@ so we that can define the @Applicative@ \/ @Monad@ computation right there in a lambda instead of making a separate function.  That is the guiding principle of this library.

This package contains a single module and is intended to be imported qualified so that function intents are clear.  The [Getting Started](#start) section walks through a concrete example.

-}

{- $gettingStarted #start#

Suppose we need to do something with an @Either String [Word16]@ value:

@
main :: IO ()
main = do
  result <- readSamples someCd :: IO (Either String [Word16])
  -- do some stuff...
@

We have a few choices.  One choice is explicit pattern matching:

@
main :: IO ()
main = do
  result <- readSamples someCd :: IO (Either String [Word16])
  case result of
    Left errString -> do
      putStrLn "R.I.P. to the CD, can't even play my hits!"
      putStrLn $ "Failure: " <> errString
      exitFailure
    Right samples -> playOnRepeat samples
@

Another option is to use the @either@ function from @Data.Either@:

@
main :: IO ()
main = do
  result <- readSamples someCd :: IO (Either String [Word16])
  either (\\errString -> do
             putStrLn "R.I.P. to the CD, can't even play my hits!"
             putStrLn $ "Failure: " <> errString
             exitFailure)
         playOnRepeat
         result
@

We have gotten rid of explicit pattern matching, but the code doesn't exactly
look nice (subjective!), particularly in the handler for the @Left@ case.

A third option is to use the @ExceptT@ monad transformer:

@
main :: IO ()
main = do
  result <- runExceptT $ do
    samples <- ExceptT (readSamples someCd) :: ExceptT String IO [Word16]
    liftIO (playOnRepeat samples)
  -- Now what?  We'll pattern match, but this feels like extra work...
  case result of
    Left errString -> do
      putStrLn "R.I.P. to the CD, can't even play my hits!"
      putStrLn $ "Failure: " <> errString
      exitFailure
    Right () -> pure ()
@

That was not super fun and arguably the least readable of all the examples so
far.  For me, it is often the case when I'm processing an @Either a b@ value
that I have a teensy bit of work to do on one of the cases, and a bit more
work to do on the other case.

What if we switched the parameters around to @either@ and gave it a new name,
like @onLeft@?

@
import qualified Overhang

main :: IO ()
main = do
  result <- readSamples someCd :: IO (Either String [Word16])
  Overhang.onLeft result playOnRepeat $ \\errString -> do
    putStrLn "R.I.P. to the CD, can't even play my hits!"
    putStrLn $ "Failure: " <> errString
    exitFailure
@

Tada!  We have gotten rid of explicit pattern matching just like we did with
@either@, and depending on who you ask, the code is much more readable.

In the example, we were only dealing with a single @Either@ value.  Sometimes
we have to deal with @Either@s of @Eithers@s, @Either@s of @Maybe@s and so on.
@overhang@ can help in these cases when we just want to neatly process the
structures and not pull in a large (but way more powerful!) dependency like
@lens@.

-}
